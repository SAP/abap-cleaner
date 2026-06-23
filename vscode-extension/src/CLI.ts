import * as vscode from 'vscode';

export class CLI {
   public static readonly PING_DAEMON_COMMAND = '--ping-daemon';
   // available CLI args, cp. https://github.com/SAP/abap-cleaner/blob/main/docs/usage.md

   // - daemonization
   public static readonly ARG_HELP_WINDOWS = '/?';
   public static readonly ARG_HELP_LINUX = '/man';
   public static readonly ARG_VERSION = '--version';
   public static readonly ARG_DAEMONIZE = '--daemonize';
   public static readonly ARG_IDLE_TIMEOUT = '--idle-timeout';

   // - source args (selection)
   public static readonly ARG_SOURCE_FILE = '--sourcefile';
   public static readonly ARG_SOURCE_CODE = '--source';
   public static readonly ARG_LINE_RANGE = '--linerange';
   public static readonly ARG_SCOPE = '--scope'; // statement (default), method, class, document, user
   public static readonly VALUE_SCOPE_USER = 'user';
   public static readonly VALUE_SCOPE_METHOD = 'method';
   public static readonly VALUE_SCOPE_STATEMENT = 'statement';

   // - cleanup args (selection)
   public static readonly ARG_PROFILE_PATH = '--profile';
   public static readonly ARG_PROFILE_NAME = '--profilename';
   public static readonly ARG_LAST_PROFILE = '--last-profile';
   //   public static readonly ARG_ABAP_RELEASE = '--release';
   public static readonly ARG_WORKSPACE_DIR = '--workspace';

   // - interactive cleanup args
   public static readonly ARG_INTERACTIVE = '--ui';
   public static readonly ARG_TITLE = '--title';
   public static readonly ARG_READ_ONLY = '--readonly';
   public static readonly ARG_DARK_THEME = '--darktheme';

   // - target args (selection)
   public static readonly ARG_TARGET_FILE = '--targetfile';
   public static readonly ARG_OVERWRITE = '--overwrite';
   public static readonly ARG_SIMULATE = '--simulate';

   // - statistics
   public static readonly ARG_STATS = '--stats';
   public static readonly ARG_USED_RULES = '--usedrules';

   public static buildArgs(srcFile: string, tgtFile: string | null, lineRange?: string, expandToUserScope?: boolean, interactive?: boolean, readOnly?: boolean, docFileName?: string): string[] {
      // source args
      const args = [CLI.ARG_SOURCE_FILE, srcFile];
      if (lineRange) {
         args.push(CLI.ARG_LINE_RANGE, lineRange);
         args.push(CLI.ARG_SCOPE, expandToUserScope ? CLI.VALUE_SCOPE_USER : CLI.VALUE_SCOPE_STATEMENT);
      }

      // cleanup args
      // only automated cleanup needs a profile, as in interactive cleanup, the user can select the profile directly on the UI
      if (!interactive) {
         // use the last profile selected on the UI, if any; otherwise, use the built-in default profile
         args.push(CLI.ARG_LAST_PROFILE);
      }
      // the workspace context is used for both automated and interactive cleanup to retrieve the profile, scope 
      // and additional release restriction that was last selected on the UI
      const workspaceFolders = vscode.workspace.workspaceFolders;
      if (workspaceFolders && workspaceFolders.length > 0) {
         // even in multi-root workspaces, simply use the first workspace folder as context for ABAP cleaner
         args.push(CLI.ARG_WORKSPACE_DIR, workspaceFolders[0].uri.fsPath);
      }

      // interactive cleanup args
      if (interactive) {
         args.push(CLI.ARG_INTERACTIVE);
         if (docFileName) {
            const title = require('path').basename(docFileName); // filename only
            args.push(CLI.ARG_TITLE, title);
         }
         if (readOnly) {
            args.push(CLI.ARG_READ_ONLY);
         }
         const themeKind = vscode.window.activeColorTheme.kind;
         if (themeKind === vscode.ColorThemeKind.Dark || themeKind === vscode.ColorThemeKind.HighContrast) {
            args.push(CLI.ARG_DARK_THEME);
         }
      }

      // target args
      if (interactive && readOnly) {
         args.push(CLI.ARG_SIMULATE);
      } else if (tgtFile) {
         args.push(CLI.ARG_TARGET_FILE, tgtFile, CLI.ARG_OVERWRITE);
      }
      return args;
   }
}