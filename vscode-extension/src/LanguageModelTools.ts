import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';

type RunAbapCleanerFunction = (sourceText: string, docFileName?: string, lineRange?: string) => Promise<string | null>;
type IsDocumentTypeSupportedFunction = (document: vscode.TextDocument) => boolean;
type IsFilePathSupportedFunction = (filePath: string) => boolean;

/** Functionality from the extension that the language model tools depend on. */
export interface LanguageModelToolDependencies {
   runAbapCleaner: RunAbapCleanerFunction;
   isDocumentTypeSupported: IsDocumentTypeSupportedFunction;
   isFilePathSupported: IsFilePathSupportedFunction;
   fileEncoding: BufferEncoding;
}

interface FormatDocumentToolInput {
   /** Absolute paths of files to format; if omitted, the active editor document is formatted. */
   filePaths?: string[];
}

interface FormatDocumentRangeToolInput {
   /** Absolute path of the ABAP or CDS file; if omitted, the active editor document is formatted. */
   filePath?: string;
   /** 1-based inclusive first line to format. */
   startLine: number;
   /** 1-based inclusive last line to format. */
   endLine: number;
}

/** Find an already-open document for the given file path, so unsaved changes are respected. */
function findOpenDocument(filePath: string): vscode.TextDocument | undefined {
   const target = vscode.Uri.file(filePath).fsPath.toLowerCase();

   return vscode.workspace.textDocuments.find(
      doc => doc.uri.scheme === 'file' && doc.uri.fsPath.toLowerCase() === target
   );
}

abstract class BaseAbapCleanerLanguageModelTool {
   protected runAbapCleaner: RunAbapCleanerFunction;
   protected isDocumentTypeSupported: IsDocumentTypeSupportedFunction;
   protected isFilePathSupported: IsFilePathSupportedFunction;
   protected readonly fileEncoding: BufferEncoding;

   constructor(dependencies: LanguageModelToolDependencies) {
      this.runAbapCleaner = dependencies.runAbapCleaner;
      this.isDocumentTypeSupported = dependencies.isDocumentTypeSupported;
      this.isFilePathSupported = dependencies.isFilePathSupported;
      this.fileEncoding = dependencies.fileEncoding;
   }

   protected newTextToolResult(message: string): vscode.LanguageModelToolResult {
      return new vscode.LanguageModelToolResult([
         new vscode.LanguageModelTextPart(message)
      ]);
   }
}

class FormatDocumentTool extends BaseAbapCleanerLanguageModelTool implements vscode.LanguageModelTool<FormatDocumentToolInput> {
   constructor(dependencies: LanguageModelToolDependencies) {
      super(dependencies);
   }

   public prepareInvocation(
      options: vscode.LanguageModelToolInvocationPrepareOptions<FormatDocumentToolInput>,
      _token: vscode.CancellationToken
   ): vscode.PreparedToolInvocation {
      const filePaths = options.input?.filePaths;

      let invocationMessage: string;
      let what: string;

      if (filePaths && filePaths.length === 1) {
         const name = path.basename(filePaths[0]);
         invocationMessage = `Formatting ${name} with ABAP cleaner`;
         what = name;
      } else if (filePaths && filePaths.length > 1) {
         invocationMessage = `Formatting ${filePaths.length} files with ABAP cleaner`;
         what = `${filePaths.length} files`;
      } else {
         invocationMessage = 'Formatting active document with ABAP cleaner';
         what = 'the active document';
      }

      return {
         invocationMessage,
         confirmationMessages: {
            title: 'Format with ABAP cleaner',
            message: `ABAP cleaner will reformat ${what} in place. Continue?`
         }
      };
   }

   public async invoke(
      options: vscode.LanguageModelToolInvocationOptions<FormatDocumentToolInput>,
      _token: vscode.CancellationToken
   ): Promise<vscode.LanguageModelToolResult> {
      const filePaths = options.input?.filePaths;

      // format explicitly given files directly on disk, so the agent need not open an editor for each file
      if (filePaths && filePaths.length > 0) {
         const results: string[] = [];

         for (const filePath of filePaths) {
            try {
               results.push(await this.formatFile(filePath));
            } catch (err: unknown) {
               results.push(`Error formatting "${filePath}": ${(err as Error).message}`);
            }
         }

         return this.newTextToolResult(results.join('\n'));
      }

      // fall back to the active editor when no file paths are provided
      const editor = vscode.window.activeTextEditor;

      if (!editor) {
         return this.newTextToolResult('No file paths were provided and there is no active text editor to format.');
      }

      if (!this.isDocumentTypeSupported(editor.document)) {
         return this.newTextToolResult('The active document is not an ABAP or CDS view (DDL) file, so ABAP cleaner cannot format it.');
      }

      await vscode.commands.executeCommand('abapCleaner.formatDocument');

      return this.newTextToolResult(`Formatted "${editor.document.fileName}" with ABAP cleaner.`);
   }

   private async formatFile(filePath: string): Promise<string> {
      if (!this.isFilePathSupported(filePath)) {
         return `Skipped "${filePath}": not an ABAP or CDS view (DDL) file.`;
      }

      // if the file is open in an editor, format its in-memory text (including unsaved changes) via a WorkspaceEdit
      const openDocument = findOpenDocument(filePath);

      if (openDocument) {
         const sourceText = openDocument.getText();
         const formatted = await this.runAbapCleaner(sourceText, filePath);
         if (!formatted || formatted === sourceText) {
            return `No changes for "${filePath}".`;
         }

         const wasDirty = openDocument.isDirty;
         const fullRange = new vscode.Range(openDocument.positionAt(0), openDocument.positionAt(sourceText.length));
         const edit = new vscode.WorkspaceEdit();
         edit.replace(openDocument.uri, fullRange, formatted);
         if (!await vscode.workspace.applyEdit(edit)) {
            return `Failed to apply edit for open document "${filePath}".`;
         }

         // if the document had no unsaved changes before, save it to keep disk in sync; otherwise leave the user's edits unsaved
         if (!wasDirty) {
            await openDocument.save();
            return `Formatted "${filePath}".`;
         }
         return `Formatted open document "${filePath}"; applied to unsaved buffer - save to persist.`;
      }

      // otherwise, format the file directly on disk without opening an editor
      if (!fs.existsSync(filePath)) {
         return `Skipped "${filePath}": file not found.`;
      }

      const sourceText = fs.readFileSync(filePath, { encoding: this.fileEncoding });
      const formatted = await this.runAbapCleaner(sourceText, filePath);

      if (!formatted || formatted === sourceText) {
         return `No changes for "${filePath}".`;
      }

      fs.writeFileSync(filePath, formatted, this.fileEncoding);
      return `Formatted "${filePath}".`;
   }
}

class FormatDocumentRangeTool extends BaseAbapCleanerLanguageModelTool implements vscode.LanguageModelTool<FormatDocumentRangeToolInput> {
   constructor(dependencies: LanguageModelToolDependencies) {
      super(dependencies);
   }

   public prepareInvocation(
      options: vscode.LanguageModelToolInvocationPrepareOptions<FormatDocumentRangeToolInput>,
      _token: vscode.CancellationToken
   ): vscode.PreparedToolInvocation {
      const filePath = options.input?.filePath;
      const startLine = options.input?.startLine ?? '?';
      const endLine = options.input?.endLine ?? '?';

      const target = typeof filePath === 'string' && filePath ? path.basename(filePath) : 'the active document';

      return {
         invocationMessage: `Formatting lines ${startLine}-${endLine} in ${target} with ABAP cleaner`,
         confirmationMessages: {
            title: 'Format document range with ABAP cleaner',
            message: `ABAP cleaner will reformat the statements intersecting lines ${startLine}-${endLine} in ${target}. It may change lines outside the requested range when needed to format complete statements. Continue?`
         }
      };
   }

   public async invoke(
      options: vscode.LanguageModelToolInvocationOptions<FormatDocumentRangeToolInput>,
      token: vscode.CancellationToken
   ): Promise<vscode.LanguageModelToolResult> {
      const filePath = options.input?.filePath;
      const startLine = options.input?.startLine;
      const endLine = options.input?.endLine;

      if (token.isCancellationRequested) {
         return this.newTextToolResult('Range formatting cancelled; no changes were applied.');
      }

      if (typeof startLine !== 'number' || !Number.isInteger(startLine) || startLine < 1 ||
         typeof endLine !== 'number' || !Number.isInteger(endLine) || endLine < startLine) {
         return this.newTextToolResult('Invalid line range: startLine and endLine must be 1-based integers and endLine must be greater than or equal to startLine.');
      }

      if (typeof (filePath) !== 'undefined' && (typeof filePath !== 'string' || filePath.length === 0)) {
         return this.newTextToolResult('Invalid filePath: provide a non-empty absolute path or omit it to use the active document.');
      }

      if (!filePath) {
         const editor = vscode.window.activeTextEditor;

         if (!editor) {
            return this.newTextToolResult('No filePath was provided and there is no active text editor to format.');
         }

         try {
            return this.newTextToolResult(await this.formatOpenDocumentRange(editor.document, startLine, endLine, token));
         } catch (err: unknown) {
            return this.newTextToolResult(`Error formatting range in "${editor.document.fileName}": ${(err as Error).message}`);
         }
      } else {
         try {
            return this.newTextToolResult(await this.formatFileRange(filePath, startLine, endLine, token));
         } catch (err: unknown) {
            return this.newTextToolResult(`Error formatting range in "${filePath}": ${(err as Error).message}`);
         }
      }
   }

   private checkRangeInFileBounds(endLine: number, lineCount: number): string | undefined {
      if (endLine > lineCount) {
         return `Invalid line range: the range ends at line ${endLine}, but the document has only ${lineCount} lines.`;
      }

      return undefined;
   }

   private async formatFileRange(
      filePath: string,
      startLine: number,
      endLine: number,
      token: vscode.CancellationToken
   ): Promise<string> {
      if (!path.isAbsolute(filePath)) {
         return `Skipped "${filePath}": an absolute file path is required.`;
      }

      if (!this.isFilePathSupported(filePath)) {
         return `Skipped "${filePath}": not an ABAP or CDS view (DDL) file.`;
      }

      const openDoc = findOpenDocument(filePath);

      if (openDoc) {
         return this.formatOpenDocumentRange(openDoc, startLine, endLine, token);
      }

      if (!fs.existsSync(filePath)) {
         return `Skipped "${filePath}": file not found.`;
      }

      const sourceText = fs.readFileSync(filePath, { encoding: this.fileEncoding });
      const rangeError = this.checkRangeInFileBounds(endLine, sourceText.split(/\r\n|\n|\r/).length);

      if (rangeError) {
         return rangeError;
      }

      const formatted = await this.runAbapCleaner(sourceText, filePath, `${startLine}-${endLine}`);

      if (token.isCancellationRequested) {
         return 'Range formatting cancelled; no changes were applied.';
      }

      if (!formatted || formatted === sourceText) {
         return `No changes for lines ${startLine}-${endLine} in "${filePath}".`;
      }

      if (fs.readFileSync(filePath, { encoding: this.fileEncoding }) !== sourceText) {
         return `Range formatting discarded for "${filePath}": the file changed while formatting.`;
      }

      fs.writeFileSync(filePath, formatted, { encoding: this.fileEncoding });
      return `Formatted lines ${startLine}-${endLine} in "${filePath}".`;
   }

   private async formatOpenDocumentRange(
      document: vscode.TextDocument,
      startLine: number,
      endLine: number,
      token: vscode.CancellationToken
   ): Promise<string> {
      if (!this.isDocumentTypeSupported(document)) {
         return `Skipped "${document.fileName}": not an ABAP or CDS view (DDL) document.`;
      }

      const rangeError = this.checkRangeInFileBounds(endLine, document.lineCount);

      if (rangeError) {
         return rangeError;
      }

      const sourceText = document.getText();

      const wasDirty = document.isDirty;
      const formatted = await this.runAbapCleaner(sourceText, document.fileName, `${startLine}-${endLine}`);

      if (token.isCancellationRequested) {
         return 'Range formatting cancelled; no changes were applied.';
      }

      if (!formatted || formatted === sourceText) {
         return `No changes for lines ${startLine}-${endLine} in "${document.fileName}".`;
      }

      if (document.getText() !== sourceText) {
         return `Range formatting discarded for "${document.fileName}": the document changed while formatting.`;
      }

      const fullRange = new vscode.Range(document.positionAt(0), document.positionAt(sourceText.length));
      const edit = new vscode.WorkspaceEdit();
      edit.replace(document.uri, fullRange, formatted);

      if (!await vscode.workspace.applyEdit(edit)) {
         return `Failed to apply range formatting to "${document.fileName}".`;
      }

      if (!wasDirty && !await document.save()) {
         return `Formatted lines ${startLine}-${endLine} in "${document.fileName}", but failed to save the document.`;
      }

      const saveNote = wasDirty ? '; applied to unsaved buffer - save to persist' : '';
      return `Formatted lines ${startLine}-${endLine} in "${document.fileName}"${saveNote}.`;
   }
}

/** Register the ABAP cleaner language model tools for agent mode. */
export function registerLanguageModelTools(context: vscode.ExtensionContext, dependencies: LanguageModelToolDependencies): void {
   context.subscriptions.push(
      vscode.lm.registerTool('abapCleaner_formatDocument', new FormatDocumentTool(dependencies)),
      vscode.lm.registerTool('abapCleaner_formatDocumentRange', new FormatDocumentRangeTool(dependencies))
   );
}
