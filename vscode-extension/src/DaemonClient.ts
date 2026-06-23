// daemon client
import * as net from "net";
import * as readline from "readline";
import { spawn, ChildProcess } from "child_process";

import { Logger } from "./Logger";
import { CLI } from "./CLI";

/**
 * Configuration for launching and communicating with the Java daemon.
 */
interface DaemonConfig {
   /** Path to the Eclipse RCP launcher or java executable */
   executablePath: string;
   /** Timeout in ms to wait for the port to be announced */
   startupTimeoutMs?: number;
   /** Temporary directory for source and target file (used to simplify logging messages) */
   tempDirEscaped?: string;
}

/**
 * Manages the lifecycle of a Java daemon process and socket communication.
 */
export class DaemonClient {
   // cp. class CommandLineArgs in Java
   public static readonly ARG_DAEMONIZE = '--daemonize';
   public static readonly ARG_IDLE_TIMEOUT = '--idle-timeout';

   public static readonly ARG_PING = '--ping';
   public static readonly ARG_STATUS = '--status';
   public static readonly ARG_REQUEST_ID = '--request-id';
   public static readonly ARG_KEEP_ALIVE = '--keep-alive';
   public static readonly ARG_STOP = '--stop';
   public static readonly END_MARKER = '<<<END>>>';

   public static readonly DAEMON_MSG_PREFIX = '[Daemon]'; // [ABAP cleaner daemon]
   public static readonly CLIENT_MSG_PREFIX = '[DaemonClient]'; // [ABAP cleaner daemon client]

   public static readonly DEFAULT_COMMAND_TIMEOUT_MS = 30_000;
   public static readonly IDLE_TIMEOUT_S = 600; // could be exposed as a config option, but this is only for cleaning up in case of VS Code crashes

   private static readonly LOCAL_HOST = "127.0.0.1";

   private process: ChildProcess | null = null;
   private socket: net.Socket | null = null;
   private port: number | null = null;
   private connectingPromise: Promise<void> | null = null;
   private requestIdCounter: number = 0;
   private responseHandlers: Map<number, { resolve: (line: string) => void; reject: (err: Error) => void }> = new Map();
   private openUiRequestId: number = -1;
   private keepAliveTimer: NodeJS.Timeout | null = null;
   private lastCommandTime: Date = new Date();

   constructor(private readonly config: DaemonConfig) { }

   /**
    * Spawns the Java process with --daemonize, waits for the port on stdout,
    * then connects via TCP socket.
    *
    * Concurrent calls coalesce on `this.connectingPromise` so that a second caller
    * cannot spawn a duplicate daemon process while the first is still connecting.
    */
   async connect(): Promise<void> {
      if (this.connectingPromise) {
         return this.connectingPromise;
      }
      this.connectingPromise = this.connectInternal().finally(() => {
         this.connectingPromise = null;
      });
      return this.connectingPromise;
   }

   private async connectInternal(): Promise<void> {
      const start = Date.now();
      Logger.log(`${DaemonClient.CLIENT_MSG_PREFIX} Connecting to daemon...`);
      try {
         this.port = await this.spawnAndReadPort();
         await this.connectSocket(this.port);
         const elapsed = Date.now() - start;
         this.updateLastCommandTime();
         Logger.log(`${DaemonClient.CLIENT_MSG_PREFIX} Connected to daemon on port ${this.port} (elapsed: ${elapsed} ms)`);

      } catch (err) {
         Logger.error(`${DaemonClient.CLIENT_MSG_PREFIX} Failed to connect to daemon: ${err}`);
         throw err;
      }
   }

   /**
    * Spawns the Java process and reads the port number from its first stdout line.
    */
   private spawnAndReadPort(): Promise<number> {
      const { executablePath, startupTimeoutMs = 10_000 } = this.config;

      return new Promise((resolve, reject) => {
         const args = [DaemonClient.ARG_DAEMONIZE, DaemonClient.ARG_IDLE_TIMEOUT, DaemonClient.IDLE_TIMEOUT_S.toString()];

         this.process = spawn(executablePath, args, { stdio: ["ignore", "pipe", "pipe"], });

         // forward stderr for visibility
         this.process.stderr?.on("data", (data: Buffer) => {
            // stderr is used by the daemon for logging, so we shouldn't treat it as an error
            Logger.log(`${DaemonClient.DAEMON_MSG_PREFIX} ${data.toString()}`, true);
         });

         this.process.on("error", (err) => {
            reject(new Error(`${DaemonClient.CLIENT_MSG_PREFIX} Failed to spawn ABAP cleaner daemon process: ${err.message}`));
         });

         this.process.on("exit", (code) => {
            if (this.socket === null) {
               // Exited before we could connect
               reject(new Error(`${DaemonClient.CLIENT_MSG_PREFIX} ABAP cleaner daemon exited prematurely with code ${code}`));
            }
         });

         // Read the first line of stdout — it must be the port number
         const rl = readline.createInterface({ input: this.process.stdout! });
         let resolved = false;

         const timeout = setTimeout(() => {
            if (!resolved) {
               rl.close();
               this.process?.kill();
               this.process = null;
               reject(new Error(`${DaemonClient.CLIENT_MSG_PREFIX} Timed out waiting for ABAP cleaner daemon port after ${startupTimeoutMs} ms`));
            }
         }, startupTimeoutMs);

         rl.once("line", (line: string) => {
            resolved = true;
            clearTimeout(timeout);
            rl.close();

            const port = parseInt(line.trim(), 10);
            if (isNaN(port) || port < 1 || port > 65535) {
               reject(new Error(`${DaemonClient.CLIENT_MSG_PREFIX} Daemon sent invalid port: "${line}"`));
            } else {
               resolve(port);
            }
         });
      });
   }

   /**
    * Opens a TCP connection to the given port and wires up line-by-line reading
    * until the STOP_MARK is received
    */
   private connectSocket(port: number): Promise<void> {
      return new Promise((resolve, reject) => {
         this.socket = new net.Socket();

         this.updateLastCommandTime();
         this.socket.connect(port, DaemonClient.LOCAL_HOST, () => {
            this.startKeepAlive();
            resolve();
         });

         this.socket.on("error", (err) => {
            reject(new Error(`${DaemonClient.CLIENT_MSG_PREFIX} Socket error: ${err.message}`));
         });

         const collectedLines: string[] = [];

         // Read responses line by line, collect the laines and dispatch the result
         // to waiting handlers as soon as a stop mark is received.
         const readLineInterface = readline.createInterface({ input: this.socket });
         readLineInterface.on("line", (line: string) => {
            if (line.startsWith(DaemonClient.END_MARKER)) {
               const requestId = parseInt(line.substring(DaemonClient.END_MARKER.length).trim(), 10);
               const handler = this.responseHandlers.get(requestId);
               if (handler) {
                  this.removeHandler(requestId);
                  handler.resolve(collectedLines.join("\n"));
                  collectedLines.length = 0; // clear for next response
               } else {
                  Logger.warn(`${DaemonClient.CLIENT_MSG_PREFIX} Unhandled response for request-id ${requestId}: ${collectedLines.join("\n")}`);
               }
            } else {
               collectedLines.push(line);
            }
         });

         this.socket.on("close", () => {
            this.stopKeepAlive();
            readLineInterface.close();
            // reject any pending command promises
            const pending = [...this.responseHandlers.values()];
            this.responseHandlers.clear();
            this.openUiRequestId = -1;
            for (const { reject } of pending) {
               reject(new Error(`${DaemonClient.CLIENT_MSG_PREFIX} Socket closed unexpectedly`));
            }
            Logger.log(`${DaemonClient.CLIENT_MSG_PREFIX} Socket closed.`);
         });
      });
   }

   public isUiOpen(): boolean {
      return this.openUiRequestId >= 0;
   }

   private updateLastCommandTime(): void {
      this.lastCommandTime = new Date();
   }

   private async ensureConnected(): Promise<void> {
      if (this.socket && !this.socket.destroyed) {
         return; // already connected
      }
      // connect() coalesces concurrent calls via this.connectingPromise, so this is safe to call from any path
      await this.connect();
      // reset open UI request id on any reconnect, since the previous UI (if any) would have been closed when the socket was closed
      this.openUiRequestId = -1;
      if (!this.socket || this.socket.destroyed) {
         throw new Error(`${DaemonClient.CLIENT_MSG_PREFIX} Socket is not connected.`);
      }
   }

   /**
    * Sends the args to the daemon and returns the response line.
    */
   async sendArgs(args: string[], commandTimeout_ms: number = DaemonClient.DEFAULT_COMMAND_TIMEOUT_MS): Promise<string> {
      const interactive = (args.find(arg => arg === CLI.ARG_INTERACTIVE) !== undefined);
      return this.sendCommandInternal(this.joinArgs(args), interactive, commandTimeout_ms);
   }

   /**
    * Sends a command string to the daemon and returns the response line. If necessary, attempts to reconnect first.
    */
   async sendCommand(command: string, commandTimeout_ms: number = DaemonClient.DEFAULT_COMMAND_TIMEOUT_MS): Promise<string> {
      return this.sendCommandInternal(command, false, commandTimeout_ms);
   }

   /**
    * Sends a command string to the daemon and returns the response line. If necessary, attempts to reconnect first.
    */
   private async sendCommandInternal(command: string, interactive: boolean, commandTimeout_ms: number = DaemonClient.DEFAULT_COMMAND_TIMEOUT_MS): Promise<string> {
      // if disconnected, try to reconnect
      try {
         await this.ensureConnected();
      } catch (err) {
         throw new Error(`${DaemonClient.CLIENT_MSG_PREFIX} Failed to reconnect to daemon: ${err}`);
      }
      if (this.isUiOpen()) {
         throw new Error(`Please close the interactive ABAP cleaner UI first.`);
      }

      return new Promise((resolve, reject) => {

         // register a one-shot response handler
         const start = Date.now();
         let resolved = false;
         const requestId = this.getNextRequestId();
         const handler = (multiLine: string) => {
            resolved = true;
            clearTimeout(timeout);

            const elapsed_ms = Date.now() - start;
            let simplifiedCommand = command;
            if (this.config.tempDirEscaped) {
               simplifiedCommand = simplifiedCommand.replaceAll(this.config.tempDirEscaped, '<tempDir>');
            }
            const lengthInfo = (multiLine.length > 0) ? ` ${multiLine.length} bytes` : '';
            Logger.log(`${DaemonClient.CLIENT_MSG_PREFIX} Daemon returned${lengthInfo} after ${elapsed_ms} ms for request-id ${requestId}: ${simplifiedCommand}`);

            resolve(multiLine);
         };
         this.responseHandlers.set(requestId, { resolve: handler, reject });

         // for interactive cleanup, track the request id so that we can block other commands until the UI is closed
         if (interactive) {
            // checking this.isUiOpen() above ensures that this.openUiRequestId is -1 at this point
            this.openUiRequestId = requestId;
         }

         const timeout = (commandTimeout_ms <= 0) ? undefined : setTimeout(() => {
            if (!resolved) {
               // remove the handler on timeout
               this.removeHandler(requestId);
               reject(new Error(`${DaemonClient.CLIENT_MSG_PREFIX} Timed out after ${commandTimeout_ms} ms waiting for response to request-id ${requestId}.`));
            }
         }, commandTimeout_ms);

         command = this.addRequestId(command, requestId);
         this.socket!.write(command + "\n", "utf8", (err) => {
            if (err) {
               // remove the handler if write failed
               this.removeHandler(requestId);
               if (timeout) {
                  clearTimeout(timeout);
               }
               reject(new Error(`${DaemonClient.CLIENT_MSG_PREFIX} Failed to write request-id ${requestId} to socket: ${err.message}`));
            }
         });
         this.updateLastCommandTime();
      });
   }

   private removeHandler(requestId: number): void {
      this.responseHandlers.delete(requestId);
      if (this.openUiRequestId === requestId) {
         this.openUiRequestId = -1;
      }
   }

   private getNextRequestId(): number {
      return ++this.requestIdCounter;
   }

   private addRequestId(command: string, requestId: number): string {
      // add a unique request id to the command to correlate responses in the logs, since some commands may take a long time and we want to be able to distinguish their responses from keep-alive responses
      return `${command} ${DaemonClient.ARG_REQUEST_ID} ${requestId}`;
   }

   private joinArgs(args: string[]): string {
      return args.map((arg) => {
         // if the arg contains spaces, quotes, or special characters, wrap in quotes
         if (/[^\w\-.,:/=@]/.test(arg)) {  // quote anything non-trivially safe
            // escape any existing double quotes and backslashes
            const escaped = arg
               .replace(/\\/g, "\\\\")
               .replace(/"/g, '\\"');
            return `"${escaped}"`;
         }
         return arg;
      }).join(" ");
   }

   private startKeepAlive(): void {
      this.stopKeepAlive(); // prevent duplicate timers

      if (DaemonClient.IDLE_TIMEOUT_S <= 0) {
         return; // no keep-alive needed
      }

      this.keepAliveTimer = setInterval(() => {
         if (this.socket?.writable) {
            // if the last command was sent less than a quarter of the idle timeout ago, skip sending a keep-alive 
            // to avoid unnecessary traffic during active use 
            if (this.lastCommandTime) {
               const idle_s = (Date.now() - this.lastCommandTime.getTime()) / 1000;
               if (this.isUiOpen()) {
                  Logger.log(`${DaemonClient.CLIENT_MSG_PREFIX} Keep-alive skipped because interactive UI is open`);
                  return;
               } else if (idle_s < DaemonClient.IDLE_TIMEOUT_S / 4) {
                  Logger.log(`${DaemonClient.CLIENT_MSG_PREFIX} Keep-alive skipped (idle for only ${idle_s} s)`);
                  return;
               }
            }

            const requestId = this.getNextRequestId();
            const handler = (multiLine: string) => {
               clearTimeout(keepAliveTimeout);
               Logger.log(`Keep-alive sent, response: ${multiLine}`);
            };
            this.responseHandlers.set(requestId, { resolve: handler, reject: (_err: Error) => { } });

            const keepAliveTimeout = setTimeout(() => {
               if (this.responseHandlers.has(requestId)) {
                  this.removeHandler(requestId);
                  Logger.warn(`${DaemonClient.CLIENT_MSG_PREFIX} Keep-alive timed out, removing stale handler`);
               }
            }, DaemonClient.DEFAULT_COMMAND_TIMEOUT_MS);

            const command = this.addRequestId(DaemonClient.ARG_KEEP_ALIVE, requestId);
            this.socket.write(command + '\n', (err) => {
               if (err) {
                  clearTimeout(keepAliveTimeout);
                  this.removeHandler(requestId);
                  Logger.error(`${DaemonClient.CLIENT_MSG_PREFIX} Failed to send keep-alive: ${err.message}`);
                  this.stopKeepAlive();
               } else {
                  this.updateLastCommandTime();
               }
            });
         } else {
            Logger.warn(`${DaemonClient.CLIENT_MSG_PREFIX} Socket not writable, stopping keep-alive`);
            this.stopKeepAlive();
         }
      }, DaemonClient.IDLE_TIMEOUT_S * 1000 / 2); // Send keep-alive at half the idle timeout interval

      this.keepAliveTimer.unref(); // don't block process exit
   }

   private stopKeepAlive(): void {
      if (this.keepAliveTimer) {
         clearInterval(this.keepAliveTimer);
         this.keepAliveTimer = null;
      }
   }

   /**
    * Gracefully stops the daemon and cleans up.
    */
   async disconnect(): Promise<void> {
      this.stopKeepAlive();
      try {
         await this.sendCommand(DaemonClient.ARG_STOP);
      } catch {
         // ignore errors during shutdown
      }
      this.socket?.destroy();
      this.process?.kill();
      this.socket = null;
      this.process = null;
      Logger.log(`${DaemonClient.CLIENT_MSG_PREFIX} Disconnected.`);
   }

}
