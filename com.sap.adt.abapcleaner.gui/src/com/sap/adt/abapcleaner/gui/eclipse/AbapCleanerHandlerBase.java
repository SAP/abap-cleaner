package com.sap.adt.abapcleaner.gui.eclipse;

import java.io.InputStream;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentRewriteSession;
import org.eclipse.jface.text.DocumentRewriteSessionType;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension4;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.part.MultiPageEditorPart;
import org.eclipse.ui.statushandlers.StatusManager;
import org.osgi.framework.FrameworkUtil;

import com.sap.adt.abapcleaner.gui.CodeDisplayColors;
import com.sap.adt.abapcleaner.gui.FrmMain;
import com.sap.adt.abapcleaner.parser.CleanupRange;
import com.sap.adt.abapcleaner.parser.CleanupResult;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.tools.abapsource.parser.padfileresolver.internal.PadFileResolver;
import com.sap.adt.tools.abapsource.sources.cleanup.TextRangeComparator;
import com.sap.adt.tools.abapsource.sources.cleanup.TextRangeComparator.CompareMode;
import com.sap.adt.tools.abapsource.ui.sources.editors.AbstractAdtEditorHandler;
import com.sap.adt.tools.abapsource.ui.sources.editors.IAbapSourcePage;
import com.sap.adt.tools.core.ui.editors.IAdtFormEditor;
import com.sap.rnd.rndrt.IPadFileResolver;

/**
 * Roughly based on
 * com.sap.adt.tools.abapsource.ui.internal.sources.cleanup.AbapSourceCleanupHandler.
 */
@SuppressWarnings("restriction")
public abstract class AbapCleanerHandlerBase extends AbstractAdtEditorHandler {

	private boolean interactive;
	private boolean readOnly;

	protected AbapCleanerHandlerBase(boolean interactive, boolean readOnly) {
		this.interactive = interactive;
		this.readOnly = readOnly;
	}

	@Override
	protected String getCommandId() {
		// TODO: This is a hack!
		// This is only used by the super class to check whether the command should be
		// enabled/disabled. Since there is no extension framework in ADT yet for 3rd
		// party add-ons, piggyback on an existing ID from ADT that does similar things.
		return "com.sap.adt.tools.abapsource.ui.cleanup.deleteUnusedVariables";
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			// get the ADT source page and file and check whether editing is allowed
			IAbapSourcePage adtSourcePage = getAdtSourcePage(HandlerUtil.getActiveEditor(event));
			IFile file = ((IFileEditorInput) adtSourcePage.getEditorInput()).getFile();
			if (!readOnly && !adtSourcePage.validateEdit(file)) {
				return null;
			}
			
			// get the whole ABAP code in this code document 
			IDocument document = adtSourcePage.getDocumentProvider().getDocument(adtSourcePage.getEditorInput());
			String oldSource = document.get();

			// get the current text selection, from which the cleanup range will be derived
			ITextSelection selection = (ITextSelection) adtSourcePage.getSelectionProvider().getSelection();
			boolean expandRange = (selection.getLength() == 0); // this is NOT the same as .isEmpty()
			CleanupRange cleanupRange = CleanupRange.create(selection.getStartLine(), selection.getEndLine() + 1, expandRange); 

			// get the ABAP release against which this code must compile
         IPadFileResolver resolver = new PadFileResolver(file.getProject());
         try (InputStream is = resolver.getPadFileContent()) {
              // do nothing, just to load data
         }
         String abapRelease = resolver.getRelease(); // e.g. "757"
         
         // execute the cleanup
			CleanupResult result;
			if (interactive) {
	         // get ADT color settings to make code in the ABAP cleaner UI appear similar to code in ADT
				CodeDisplayColors codeDisplayColors = createCodeDisplayColors(); 
				result = FrmMain.cleanInteractively(oldSource, abapRelease, cleanupRange, true, adtSourcePage.getTitle(), codeDisplayColors, readOnly);
			} else {
				result = FrmMain.cleanAutomatically(oldSource, abapRelease, cleanupRange, null, false);
			}

			if (result == null) 
				return null;

			if (result.hasCleanedCode() && !readOnly)
				replaceTextInDocument(document, adtSourcePage, oldSource, result.cleanedCode, new CleanupResultWrapper(result));
			else if (result.hasErrorMessage()) 
				MessageDialog.openError(adtSourcePage.getSite().getShell(), Program.PRODUCT_NAME, result.errorMessage);

		} catch (Exception e) {
			IStatus status = new Status(IStatus.ERROR, FrameworkUtil.getBundle(getClass()).getSymbolicName(),
					Program.PRODUCT_NAME + " has encountered a problem", e);
			StatusManager.getManager().handle(status, StatusManager.SHOW | StatusManager.LOG);
		}

		return null;
	}

	private CodeDisplayColors createCodeDisplayColors() {
		final String namePrefix = "com.sap.adt.tools.abapsource.ui."; 

		ColorRegistry colorRegistry = JFaceResources.getColorRegistry();
		Color colMethodBackground = colorRegistry.get(namePrefix + "abapMethodBackground");
		Color colDefault = colorRegistry.get(namePrefix + "default");
		Color colKeyword = colorRegistry.get(namePrefix + "keyword");
		Color colLiteral = colorRegistry.get(namePrefix + "literal");
		Color colLiteralNumber = colorRegistry.get(namePrefix + "literal.number");
		Color colComment = colorRegistry.get(namePrefix + "comment");
		// Color colError = colorRegistry.get(namePrefix + "error");
		// Color colSqlScriptBack = colorRegistry.get(namePrefix + "sqlScriptBackground");
		
		return new CodeDisplayColors(colMethodBackground, colDefault, colKeyword, colLiteral, colLiteralNumber, colComment);
	}
	
	private static void replaceTextInDocument(IDocument document, IAbapSourcePage adtSourcePage, String source,
			String newSource, ITextSelection newSelection) throws BadLocationException {

		StyledText textControl = adtSourcePage.getViewer().getTextWidget();

		// disable redraw to avoid UI flickering caused by multiple document and editor manipulations
		textControl.setRedraw(false);
		
		// now start a rewrite session - all modifications done on the document are then handled as one bigger change
		DocumentRewriteSession rewriteSession = ((IDocumentExtension4) document) .startRewriteSession(DocumentRewriteSessionType.SEQUENTIAL);

		// disable reconciler so that each small TextDelta for syntax highlighting is not affected by a syntax highlight re-calculation
		adtSourcePage.uninstallReconciler();

		try {
			MultiTextEdit edit = TextRangeComparator.createTextEdit(source, newSource, CompareMode.WORD);
			edit.apply(document, TextEdit.UPDATE_REGIONS);
			
			if (newSelection != null) {
				// as ABAP cleaner always processes full ABAP statements, the following could be used to extend the text selection 
				// to full statements (at the beginning and at the end of the selection) and thereby reflect the exact scope of code
				// that was cleaned; however, .setSelection() currently causes the vertical scrollbar to move, and it is currently 
				// unknown whether and how this could be suppressed; using textControl.getTopIndex() / setTopIndex(...) doesn't seem to 
				// help. Not extending the selection is therefore the better option until a solution is found
				
				//	adtSourcePage.getSelectionProvider().setSelection(newSelection);
			}
			
		} finally {
			textControl.setRedraw(true);
			((IDocumentExtension4) document).stopRewriteSession(rewriteSession);
			adtSourcePage.installReconciler();
		}
	}

	public static IAbapSourcePage getAdtSourcePage(IEditorPart editor) {
		if (editor instanceof IAbapSourcePage) {
			return (IAbapSourcePage) editor;
		}

		if (editor instanceof MultiPageEditorPart) { // recursion for MultiPageEditorPart
			MultiPageEditorPart multiPageEditor = (MultiPageEditorPart) editor;
			IEditorPart activePage = (IEditorPart) multiPageEditor.getSelectedPage();
			if ((activePage == null) && (multiPageEditor instanceof IAdtFormEditor)) {
				// support source page inside a form editor scenario
				int ap = multiPageEditor.getActivePage();
				IEditorPart ed = ((IAdtFormEditor) multiPageEditor).getEditor(ap);
				return getAdtSourcePage(ed);
			}
			return getAdtSourcePage(activePage);
		}

		return null;
	}
}
