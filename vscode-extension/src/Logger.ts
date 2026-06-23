import * as vscode from 'vscode';

export class Logger {
   private static debugMode: boolean = false;
   private static outputChannel: vscode.OutputChannel | undefined;

   static setOutputChannel(channel?: vscode.OutputChannel): void {
      this.outputChannel = channel;
   }

   static setDebugMode(enabled: boolean, showOutputChannel: boolean): void {
      this.debugMode = enabled;
      if (enabled && showOutputChannel) {
         this.outputChannel?.show(true);
      }
      this.log(`Debug mode ${enabled ? 'enabled' : 'disabled'}`);
   }

   static log(message: string, fromDaemon?: boolean): void {
      if (this.debugMode) {
         message = this.removeTrailingNewline(message);
         const line = `[DEBUG] ${message}`;
         if (fromDaemon) {
            // Log daemon output with a different color  to distinguish it from regular debug logs
            console.warn(line);
         } else {
            console.log(line);
         }
         this.outputChannel?.appendLine(line);
      }
   }

   static info(message: string): void {
      message = this.removeTrailingNewline(message);
      const line = `[INFO] ${message}`;
      console.log(line);
      this.outputChannel?.appendLine(line);
   }

   static warn(message: string): void {
      message = this.removeTrailingNewline(message);
      const line = `[WARN] ${message}`;
      console.warn(line);
      this.outputChannel?.appendLine(line);
   }

   static error(message: string): void {
      message = this.removeTrailingNewline(message);
      const line = `[ERROR] ${message}`;
      console.error(line);
      this.outputChannel?.appendLine(line);
      this.outputChannel?.show(true); // show and preserve focus on current editor
   }

   private static removeTrailingNewline(message: string): string {
      if (message.endsWith('\r\n')) {
         return message.slice(0, -2);
      } else if (message.endsWith('\n')) {
         return message.slice(0, -1);
      }
      return message;
   }
}