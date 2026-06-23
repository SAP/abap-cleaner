import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';

import { Logger } from './Logger';
import { CLI } from "./CLI";
import { BinaryManager } from "./BinaryManager";

const TEMP_FOLDER = 'abap-cleaner-vscode';
const TEMP_INPUT_FILE_INFIX = 'input';
const TEMP_OUTPUT_FILE_INFIX = 'output';
const ABAP_SUFFIX = '.abap';
const ACDS_SUFFIX = '.acds';
const TEMP_FILE_ENCODING = 'utf8';

const TEMP_DIR = path.join(os.tmpdir(), TEMP_FOLDER);
const TEMP_DIR_ESCAPED = TEMP_DIR.replaceAll('\\', '\\\\'); // to simplify messages with escaped paths

let binaryManager: BinaryManager | null = null;

// ─── Messaging ──────────────────────────────────────────────────────────

function showWarning(message: string): void {
   vscode.window.showWarningMessage(`ABAP cleaner: ${message}`);
}

function showError(message: string): void {
   vscode.window.showErrorMessage(`ABAP cleaner: ${message}`);
}

// ─── Temp file helpers ──────────────────────────────────────────────────

function tempFileBase(): string {
   return `fmt-${Date.now()}-${Math.random().toString(36).slice(2, 8)}`;
}

function ensureTempDir(): void {
   if (!fs.existsSync(TEMP_DIR)) {
      fs.mkdirSync(TEMP_DIR, { recursive: true });
   }
}

function tempFilePath(fileBase: string, suffix: string): string {
   ensureTempDir();
   return path.join(TEMP_DIR, `${fileBase}-${suffix}`);
}

function cleanupTemp(filePath: string): void {
   try {
      fs.unlinkSync(filePath);
   } catch { /* ignore */ }
}

// ─── CLI execution ──────────────────────────────────────────────────────

/**
 * Run ABAP cleaner on a temporary source file and return the result from the temporary target file.
 */
async function runAbapCleaner(sourceText: string, docFileName?: string, lineRange?: string, expandToUserScope?: boolean, interactive?: boolean, readOnly?: boolean): Promise<string | null> {
   if (!binaryManager) {
      throw new Error('Binary manager not initialized');
   }

   const binary = binaryManager.ensureBinary();

   const fileName = tempFileBase();
   const suffix = docFileName?.toLowerCase().endsWith(ACDS_SUFFIX) ? ACDS_SUFFIX : ABAP_SUFFIX;
   const srcFile = tempFilePath(fileName, TEMP_INPUT_FILE_INFIX + suffix);
   const tgtFile = readOnly ? null : tempFilePath(fileName, TEMP_OUTPUT_FILE_INFIX + suffix);
   const args = CLI.buildArgs(srcFile, tgtFile, lineRange, expandToUserScope, interactive, readOnly, docFileName);

   try {
      fs.writeFileSync(srcFile, sourceText, TEMP_FILE_ENCODING);
      await binaryManager.execBinary(binary, args, interactive);

      if (tgtFile && fs.existsSync(tgtFile)) {
         return fs.readFileSync(tgtFile, { encoding: TEMP_FILE_ENCODING });
      }

      if (interactive || !tgtFile) {
         // interactive mode may not create a dedicated target output file.
         if (fs.existsSync(srcFile)) {
            const sourceAfterCleanup = fs.readFileSync(srcFile, { encoding: TEMP_FILE_ENCODING });
            return sourceAfterCleanup === sourceText ? null : sourceAfterCleanup;
         }
         return null;
      }

      throw new Error(`ABAP cleaner did not produce expected output file: ${tgtFile}`);

   } finally {
      cleanupTemp(srcFile);
      // the target file may not exist if ABAP cleaner failed before creating it
      if (tgtFile && fs.existsSync(tgtFile)) {
         cleanupTemp(tgtFile);
      }
   }
}

// ─── ABAP Formatter providers ───────────────────────────────────────────

/** Format Document */
const documentFormattingProvider: vscode.DocumentFormattingEditProvider = {
   async provideDocumentFormattingEdits(
      document: vscode.TextDocument,
      _options: vscode.FormattingOptions,
      token: vscode.CancellationToken
   ): Promise<vscode.TextEdit[]> {

      const sourceText = document.getText();

      try {
         const formatted = await runAbapCleaner(sourceText, document.fileName);
         if (token.isCancellationRequested || !formatted || formatted === sourceText) {
            return [];
         }

         const fullRange = new vscode.Range(document.positionAt(0), document.positionAt(sourceText.length));
         return [vscode.TextEdit.replace(fullRange, formatted)];

      } catch (err: unknown) {
         showError((err as Error).message);
         return [];
      }
   }
};

/** Format Selection - expands to user-defined scope if selection is empty */
const rangeFormattingProvider: vscode.DocumentRangeFormattingEditProvider = {
   async provideDocumentRangeFormattingEdits(
      document: vscode.TextDocument,
      range: vscode.Range,
      _options: vscode.FormattingOptions,
      token: vscode.CancellationToken
   ): Promise<vscode.TextEdit[]> {

      // even if only a range is requested, we need to pass the entire document text to provide the full context for formatting
      const sourceText = document.getText();

      // build the 1-based line range string, e.g. "5-10"
      const lineRange = `${range.start.line + 1}-${range.end.line + 1}`;

      // if no code is selected (cursor only), expand the current line to the user-defined scope (i.e. by default, to  
      // 'method' scope). This cannot be determined from range, which puts range.start to line start and range.end to line end
      // in such a case; therefore, we need to check the editor selection directly.
      const editor = vscode.window.activeTextEditor;
      const expandToUserScope = (editor && editor.selection.isEmpty);

      try {
         const formatted = await runAbapCleaner(sourceText, document.fileName, lineRange, expandToUserScope);
         if (token.isCancellationRequested || !formatted || formatted === sourceText) {
            return [];
         }

         // replace the entire document content with the formatted text, even if only a range was formatted
         const fullRange = new vscode.Range(document.positionAt(0), document.positionAt(sourceText.length));
         return [vscode.TextEdit.replace(fullRange, formatted)];

      } catch (err: unknown) {
         showError((err as Error).message);
         return [];
      }
   }
};

/** Format Interactively - either as a read-only preview, or replacing editor content with the result of interactive cleanup */
async function formatInteractively(readOnly: boolean): Promise<void> {
   const editor = vscode.window.activeTextEditor;
   const document = editor?.document;
   if (!editor || !document) {
      showWarning('No active text editor.');
      return;
   }
   const sourceText = document.getText();

   const selection = editor.selection;
   const lineRange = (selection == null) ? undefined : `${selection.start.line + 1}-${selection.end.line + 1}`;
   const expandToUserScope = (!selection || selection.isEmpty);

   try {
      const formatted = await runAbapCleaner(sourceText, document.fileName, lineRange, expandToUserScope, true, readOnly);
      if (readOnly || !formatted || formatted === sourceText) {
         return;
      }

      // before applying the edit, check whether the active editor and its contents are still the same
      const editor2 = vscode.window.activeTextEditor;
      const document2 = editor2?.document;
      if (!editor2 || document2 !== document) {
         showWarning('Cleanup result discarded: Active editor has changed in the meantime.');
         return;
      } else if (document2.getText() !== sourceText) {
         showWarning('Cleanup result discarded: Document content has been changed in the meantime.');
         return;
      }

      // apply the result of interactive cleanup
      const fullRange = new vscode.Range(document.positionAt(0), document.positionAt(sourceText.length));
      editor.edit(editBuilder => {
         editBuilder.replace(fullRange, formatted);
      });
      return;

   } catch (err: unknown) {
      showError((err as Error).message);
      return;
   }
}


// ─── Language selector ──────────────────────────────────────────────────

const ABAP_SELECTOR: vscode.DocumentFilter[] = [
   { language: 'abap' }, // ABAP
   { language: 'acds' }, // Data Definition Language (DDL) for CDS views

   // ABAP code
   // { scheme: 'abap', pattern: '**/*.abap' },
   { scheme: 'file', pattern: '**/*.abap' },
   { scheme: 'vscode-vfs', pattern: '**/*.abap' }, // vfs= virtual file system

   // Data Definition Language (DDL) code for CDS views uses the extension .acds
   // { scheme: 'acds', pattern: '**/*.acds' },
   { scheme: 'file', pattern: '**/*.acds' },
   { scheme: 'vscode-vfs', pattern: '**/*.acds' }, // vfs = virtual file system
];

// ─── Activation ─────────────────────────────────────────────────────────

function isDocumentTypeSupported(document: vscode.TextDocument): boolean {
   // both ABAP code and Data Definition Language (DDL) for CDS views are supported
   if (document.languageId === 'abap' || document.languageId === 'acds') {
      return true;
   }

   const uriPath = document.uri.path.toLowerCase();
   return uriPath.endsWith('.abap') || uriPath.endsWith('.acds');
}

function checkActiveTextEditorSupported(): boolean {
   const editor = vscode.window.activeTextEditor;
   if (!editor) {
      showWarning('No active text editor.');
      return false;

   } else if (!isDocumentTypeSupported(editor.document)) {
      showWarning('Object type of active text editor is not supported.');
      return false;
   }
   // even for 'Format Selection', no need to prevent editor.selection.isEmpty: the provider will handle this by expanding 
   // to user scope (i.e. by default, method scope), which is a convenient feature rather than a limitation. 
   return true;
}

export function activate(context: vscode.ExtensionContext): void {
   // create output channel
   let outputChannel = vscode.window.createOutputChannel('ABAP cleaner');
   context.subscriptions.push(outputChannel);

   // initialize logger
   const config = vscode.workspace.getConfiguration('abapCleaner');
   const debugMode = config.get<boolean>('debugMode', false);
   Logger.setOutputChannel(outputChannel);
   Logger.setDebugMode(debugMode, false);
   // react to config changes at runtime
   context.subscriptions.push(
      vscode.workspace.onDidChangeConfiguration(e => {
         if (e.affectsConfiguration('abapCleaner.debugMode')) {
            const updated = vscode.workspace.getConfiguration('abapCleaner');
            Logger.setDebugMode(updated.get<boolean>('debugMode', false), true);
         }
      })
   );

   const extensionPath = context.extensionPath;
   binaryManager = new BinaryManager(extensionPath, TEMP_DIR_ESCAPED);
   Logger.log(`Extension activated. Extension path: ${extensionPath}`);

   // register formatters
   const docFormatter = vscode.languages.registerDocumentFormattingEditProvider(
      ABAP_SELECTOR, documentFormattingProvider
   );
   const rangeFormatter = vscode.languages.registerDocumentRangeFormattingEditProvider(
      ABAP_SELECTOR, rangeFormattingProvider
   );

   // register commands for automated cleanup
   const formatDocumentCmd = vscode.commands.registerCommand('abapCleaner.formatDocument', async () => {
      if (checkActiveTextEditorSupported()) {
         await vscode.commands.executeCommand('editor.action.formatDocument');
      }
   });

   const formatSelectionCmd = vscode.commands.registerCommand('abapCleaner.formatSelection', async () => {
      if (checkActiveTextEditorSupported()) {
         await vscode.commands.executeCommand('editor.action.formatSelection');
      }
   });

   // register commands for interactive cleanup
   const formatInteractivelyCmd = vscode.commands.registerCommand('abapCleaner.formatInteractively', async () => {
      if (checkActiveTextEditorSupported()) {
         await formatInteractively(false);
      }
   });

   const readOnlyPreviewCmd = vscode.commands.registerCommand('abapCleaner.readOnlyPreview', async () => {
      if (checkActiveTextEditorSupported()) {
         await formatInteractively(true);
      }
   });

   context.subscriptions.push(docFormatter, rangeFormatter,
      formatDocumentCmd, formatSelectionCmd,
      formatInteractivelyCmd, readOnlyPreviewCmd);

   // Pre-resolve the bundled binary so any error surfaces early (instead of on first format).
   try {
      const binary = binaryManager.ensureBinary();
      Logger.log(`Bundled binary ready: ${binary}`);
   } catch (err: unknown) {
      const message = (err instanceof Error) ? err.message : String(err);
      Logger.warn(`Failed to resolve bundled binary on startup: ${message}`);
   }

   void binaryManager.ensureDaemonStarted().then(() => {
      Logger.log(`Daemon started successfully.`);
   }).catch((err: unknown) => {
      const message = (err instanceof Error) ? err.message : String(err);
      Logger.warn(`Failed to start daemon on startup: ${message}`);
   });

   // binaryManager.testDaemon();
}

export function deactivate(): void {
   binaryManager?.deactivate();
   binaryManager = null;
}
