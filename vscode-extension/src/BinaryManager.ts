import * as path from 'path';
import * as fs from 'fs';
import { execFile, execSync, ChildProcess } from 'child_process';

import { Logger } from './Logger';
import { DaemonClient } from './DaemonClient';
import { testDaemon } from './DaemonClientTest';

export class BinaryManager {
   private static readonly DAEMON_STARTUP_TIMEOUT_MS = 20 * 1000; // 20 seconds
   private static readonly DAEMON_STARTUP_TIMEOUT_WIN_MS = 45 * 1000; // 45 seconds

   private static readonly INTERACTIVE_CLEANUP_TIMEOUT_MS = 0; // no timeout
   private static readonly AUTOMATED_CLEANUP_TIMEOUT_MS = 30 * 1000; // 30 seconds

   /**
    * Optional override pointing at a developer-built abap-cleaner binary.
    * Used for local development when iterating on the Java side without rebuilding the VSIX.
    */
   private static readonly ENV_DEV_BINARY_PATH = 'ABAP_CLEANER_DEV_BINARY_PATH';

   /**
    * Folder bundled inside the VSIX (and populated locally for dev) that contains the abap-cleaner product
    * matching the host platform. The folder name mirrors the layout of the upstream archives:
    *   binary/abapcleaner/...
    */
   private static readonly BUNDLED_BINARY_DIR = 'binary';
   private static readonly BUNDLED_BINARY_ROOT_FOLDER = 'abapcleaner';

   private static readonly PLATFORM_WIN32 = 'win32';
   private static readonly PLATFORM_MACOS = 'darwin';
   private static readonly PLATFORM_LINUX = 'linux';

   // ─── OS info ────────────────────────────────────────────────────────────

   public static isWin32() {
      return process.platform === BinaryManager.PLATFORM_WIN32;
   }

   public static isLinux() {
      return process.platform === BinaryManager.PLATFORM_LINUX;
   }

   public static isMacOS() {
      return process.platform === BinaryManager.PLATFORM_MACOS;
   }

   // ─── Attributes ─────────────────────────────────────────────────────────

   private extensionPath: string;
   private cachedBinaryPath: string | undefined;
   private tempDirEscaped: string | undefined;

   private daemonClient: DaemonClient | null = null;
   private daemonStartingPromise: Promise<void> | null = null;
   private interactiveProcess: ChildProcess | null = null;

   public constructor(extensionPath: string, tempDirEscaped: string | undefined) {
      this.extensionPath = extensionPath;
      this.tempDirEscaped = tempDirEscaped;
   }

   // ─── Binary resolution ──────────────────────────────────────────────────

   private getBundledBinaryBasePath(): string {
      return path.join(this.extensionPath, BinaryManager.BUNDLED_BINARY_DIR);
   }

   private getBinaryCandidates(base: string): string[] {
      const root = path.join(base, BinaryManager.BUNDLED_BINARY_ROOT_FOLDER);
      if (BinaryManager.isWin32()) {
         return [
            path.join(root, 'abap-cleanerc.exe'),
            path.join(root, 'abap-cleaner.exe'),
         ];
      } else if (BinaryManager.isMacOS()) {
         // Tycho's <rootFolder> wraps Linux/Windows archives in `abapcleaner/`, but the macOS
         // tar.gz puts `abapcleaner.app` at the archive root — so after extracting into `binary/`
         // the .app sits directly under `binary/`, not `binary/abapcleaner/`. Look in both shapes.
         return [
            path.join(root, 'abapcleaner.app', 'Contents', 'MacOS', 'abap-cleaner'),
            path.join(root, 'abap-cleaner'),
            path.join(base, 'abapcleaner.app', 'Contents', 'MacOS', 'abap-cleaner'),
         ];
      } else {
         return [
            path.join(root, 'abap-cleaner'),
         ];
      }
   }

   private isExecutable(filePath: string): boolean {
      if (!fs.existsSync(filePath)) {
         return false;
      }

      try {
         fs.accessSync(filePath, BinaryManager.isWin32() ? fs.constants.F_OK : fs.constants.X_OK);
         return true;
      } catch {
         return false;
      }
   }

   private findBinaryInBase(base: string): string | undefined {
      for (const candidate of this.getBinaryCandidates(base)) {
         if (this.isExecutable(candidate)) {
            return candidate;
         }
      }
      return undefined;
   }

   /**
    * Resolve the abap-cleaner binary.
    *
    * Resolution order:
    *   1. ABAP_CLEANER_DEV_BINARY_PATH env var (developer override).
    *   2. Bundled binary inside the VSIX at <extensionPath>/binary/abapcleaner/...
    */
   public ensureBinary(): string {
      // 1. dev override
      const devBinaryPath = process.env[BinaryManager.ENV_DEV_BINARY_PATH];
      if (devBinaryPath && devBinaryPath.trim().length > 0) {
         const trimmed = devBinaryPath.trim();
         if (this.isExecutable(trimmed)) {
            this.cachedBinaryPath = trimmed;
            return trimmed;
         }
         throw new Error(`${BinaryManager.ENV_DEV_BINARY_PATH} is set but points at a non-executable file: ${trimmed}`);
      }

      // 2. bundled binary
      if (this.cachedBinaryPath && this.isExecutable(this.cachedBinaryPath)) {
         return this.cachedBinaryPath;
      }

      const base = this.getBundledBinaryBasePath();
      const bundled = this.findBinaryInBase(base);
      if (bundled) {
         this.prepareBundledBinary(bundled);
         this.cachedBinaryPath = bundled;
         return bundled;
      }

      const candidates = this.getBinaryCandidates(base).join(', ');
      throw new Error(
         `ABAP cleaner binary not found inside the extension. ` +
         `Expected one of: ${candidates}. ` +
         `If you are running the extension from source, build the Java side and copy the product into '${base}', ` +
         `or set ${BinaryManager.ENV_DEV_BINARY_PATH} to point at a local build.`
      );
   }

   /**
    * One-time post-install/post-extract preparation of the bundled binary on the host platform.
    * - macOS: strip quarantine, set LSUIElement to hide the dock icon when running as a daemon.
    * - non-Windows: ensure the executable bit is set (npm/yarn/vsce don't preserve permissions reliably).
    */
   private prepareBundledBinary(binaryPath: string): void {
      if (BinaryManager.isMacOS()) {
         const appBundlePath = this.getMacAppBundlePath(binaryPath);
         const appDir = appBundlePath ?? path.dirname(binaryPath);
         try { execSync(`xattr -dr com.apple.quarantine "${appDir}"`, { timeout: 10000 }); } catch { /* ignore */ }
         this.ensureLSUIElementHidden(binaryPath);
      }

      if (!BinaryManager.isWin32()) {
         try { fs.chmodSync(binaryPath, 0o755); } catch { /* ignore */ }
      }
   }

   // ─── DaemonClient handling ──────────────────────────────────────────────

   private ensureLSUIElementHidden(binary: string): void {
      if (!BinaryManager.isMacOS()) { return; }
      const appBundlePath = this.getMacAppBundlePath(binary);
      if (!appBundlePath) { return; }
      const infoPlistPath = path.join(appBundlePath, 'Contents', 'Info.plist');
      if (fs.existsSync(infoPlistPath)) {
         try { execSync(`plutil -replace LSUIElement -bool YES "${infoPlistPath}"`, { timeout: 5000 }); } catch { /* ignore */ }
      }
   }

   public async ensureDaemonStarted(): Promise<void> {
      if (this.daemonClient) {
         return;
      }
      // Coalesce concurrent callers so they don't each construct a DaemonClient (and the second
      // caller's later sendArgs() doesn't trigger a duplicate connect()/spawn()).
      if (!this.daemonStartingPromise) {
         this.daemonStartingPromise = this.startDaemonInternal().finally(() => {
            this.daemonStartingPromise = null;
         });
      }
      return this.daemonStartingPromise;
   }

   private async startDaemonInternal(): Promise<void> {
      const binary = this.ensureBinary();
      this.ensureLSUIElementHidden(binary);
      const client = new DaemonClient({
         executablePath: binary,
         // extraArgs: ["-application", "com.example.app"],
         startupTimeoutMs: BinaryManager.isWin32() ? BinaryManager.DAEMON_STARTUP_TIMEOUT_WIN_MS : BinaryManager.DAEMON_STARTUP_TIMEOUT_MS, // allow more time on Windows due to potential overhead of antivirus scans on startup
         // pass on tempDirEscaped for simplified logging messages
         tempDirEscaped: this.tempDirEscaped,
      });
      try {
         await client.connect();
      } catch (err) {
         // do not retain a half-constructed client; surface the error to the caller so the next
         // ensureDaemonStarted() call can retry from scratch
         throw err;
      }
      this.daemonClient = client;
   }

   public async testDaemon() {
      await this.ensureDaemonStarted();
      if (this.daemonClient) {
         await testDaemon(this.daemonClient);
      } else {
         Logger.error("Daemon client not available after startup.");
      }
   }

   // ─── CLI execution ──────────────────────────────────────────────────────

   private getMacAppBundlePath(binaryPath: string): string | undefined {
      const appBundleSuffix = `${path.sep}Contents${path.sep}MacOS${path.sep}abap-cleaner`;
      if (!binaryPath.endsWith(appBundleSuffix)) {
         return undefined;
      }
      return path.dirname(path.dirname(path.dirname(binaryPath)));
   }

   private getExecEnvForMacOpen(): NodeJS.ProcessEnv {
      const env: NodeJS.ProcessEnv = { ...process.env };
      // prevent inherited VS Code/Electron launcher flags from affecting child process startup.
      for (const key of Object.keys(env)) {
         if (key.startsWith('VSCODE_') || key.startsWith('ELECTRON_')) {
            delete env[key];
         }
      }
      return env;
   }

   private execMacInteractive(executable: string, executableArgs: string[], env?: NodeJS.ProcessEnv): Promise<string> {
      return new Promise((resolve, reject) => {
         execFile(executable, executableArgs, { timeout: 0, env }, (err, stdout, stderr) => {
            if (err) {
               const msg = [stdout, stderr].filter(Boolean).join('\n').trim();
               reject(new Error(`ABAP cleaner failed: ${msg || err.message}`));
            } else {
               resolve(stderr.trim());
            }
         });
      });
   }

   private async execDaemon(executableArgs: string[], timeoutMs: number): Promise<string> {
      await this.ensureDaemonStarted();
      if (!this.daemonClient) {
         throw new Error("Daemon client not available after startup.");
      }
      try {
         const response = await this.daemonClient.sendArgs(executableArgs, timeoutMs);
         return response.trim();
      } catch (err) {
         throw new Error(`${err}`);
      }
   }

   public async execBinary(binary: string, args: string[], interactive: boolean = false): Promise<void> {
      const timeoutMs = interactive ? BinaryManager.INTERACTIVE_CLEANUP_TIMEOUT_MS : BinaryManager.AUTOMATED_CLEANUP_TIMEOUT_MS;

      let result: string;

      if (interactive && BinaryManager.isMacOS()) {
         const env = this.getExecEnvForMacOpen();
         const appBundlePath = this.getMacAppBundlePath(binary);

         let executable = binary;
         let executableArgs = args;
         if (appBundlePath && fs.existsSync(appBundlePath)) {
            executable = 'open';
            executableArgs = ['-W', '-n', '-a', appBundlePath, '--args', ...args];
         }

         // Temporarily make the app visible in the Dock so the interactive window can be found.
         // Restore LSUIElement=YES afterward so the daemon stays hidden.
         const infoPlistPath = appBundlePath ? path.join(appBundlePath, 'Contents', 'Info.plist') : undefined;
         if (infoPlistPath && fs.existsSync(infoPlistPath)) {
            try { execSync(`plutil -replace LSUIElement -bool NO "${infoPlistPath}"`, { timeout: 5000 }); } catch { /* ignore */ }
         }

         // Track the app process by PID so deactivate() can kill it if VS Code closes while it's open.
         // We look up the PID shortly after launch since `open` spawns a separate child process.
         if (appBundlePath) {
            const appBundlePathForKill = appBundlePath;
            setTimeout(() => {
               try {
                  const pid = parseInt(execSync(`pgrep -n -f "${appBundlePathForKill}"`, { timeout: 3000 }).toString().trim(), 10);
                  if (!isNaN(pid)) {
                     this.interactiveProcess = { pid, kill: (signal?: NodeJS.Signals | number) => { try { process.kill(pid, signal ?? 'SIGTERM'); } catch { /* ignore */ } return true; } } as unknown as ChildProcess;
                  }
               } catch { /* ignore — pgrep returns non-zero if no match */ }
            }, 1000);
         }

         const start = Date.now();
         Logger.log(`Interactive args: ${JSON.stringify(args)}`);
         Logger.log(`Interactive launch executable=${executable}`);
         try {
            result = await this.execMacInteractive(executable, executableArgs, env);
         } finally {
            this.interactiveProcess = null;
            if (infoPlistPath && fs.existsSync(infoPlistPath)) {
               try { execSync(`plutil -replace LSUIElement -bool YES "${infoPlistPath}"`, { timeout: 5000 }); } catch { /* ignore */ }
            }
         }

         const duration_ms = Date.now() - start;
         Logger.log(`Interactive launch finished after ${duration_ms} ms.`);

      } else {
         // on Windows, both automated and interactive cleanup can be launched via the daemon;
         // on macOS, only automated cleanup is launched via the daemon
         result = await this.execDaemon(args, timeoutMs);
      }

      if (result.length > 0) {
         Logger.warn(`stderr: ${result}`);
      }
   }

   // ─── deactivation ──────────────────────────────────────────────────────

   public deactivate() {
      if (this.interactiveProcess) {
         this.interactiveProcess.kill();
         this.interactiveProcess = null;
      }
      if (this.daemonClient) {
         void this.daemonClient.disconnect();
      }
   }

   // ─── Backwards-compatible API for extension.ts ──────────────────────────

   /** @deprecated kept for compatibility; resolves synchronously now since the binary is bundled. */
   public async ensureManagedBinary(): Promise<string> {
      return this.ensureBinary();
   }
}
