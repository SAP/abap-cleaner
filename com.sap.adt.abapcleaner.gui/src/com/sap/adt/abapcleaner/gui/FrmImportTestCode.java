package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wb.swt.SWTResourceManager;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.programbase.*;
import org.eclipse.swt.graphics.Point;

public class FrmImportTestCode {
   private static final String EXTENSION = ".txt";

   private String lastClipboard = "";
   private final StringBuilder importedFiles = new StringBuilder();
   private int totalImportedLineCount = 0;
   private int totalImportedFileCount = 0;

   private Shell shell;
	private Text txtDestPath;
	private Text txtFolder;
	private Text txtImportedFiles;
	private Button chkWatchClipboard;
	private Button chkUpperCaseFilenames;
	private Button chkOverwrite;
	private Button chkBeep;
	private Label lblImportedStats;

   public void open(String destPath) {
      open(destPath, "");
   }

	/**
	 * @wbp.parser.entryPoint
	 */
	public void open(String destPath, String folder) {
		createContents();

      txtDestPath.setText(destPath);
      txtFolder.setText(folder);

      shell.open();
		shell.layout();

		Display display = shell.getDisplay();

      lastClipboard = SystemClipboard.containsText() ? SystemClipboard.getText() : "";
		display.timerExec(500, new Runnable() {
			public void run() {
				if (!shell.isDisposed()) {
					checkClipboard();
					display.timerExec(500, this);
				}
			}
		});

		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}
	}

	/**
	 * Create contents of the dialog.
	 */
	private void createContents() {
		shell = new Shell(SWT.APPLICATION_MODAL | SWT.BORDER | SWT.TITLE | SWT.CLOSE | SWT.RESIZE | SWT.MAX);
		shell.setMinimumSize(new Point(600, 240));
		shell.setImage(SWTResourceManager.getImage(FrmImportTestCode.class, "/ShellImage.png"));
		shell.setSize(640, 800);
		shell.setText("Import test code from clipboard");
		GridLayout gl_shell = new GridLayout(2, false);
		gl_shell.marginWidth = 12;
		gl_shell.marginHeight = 12;
		shell.setLayout(gl_shell);
		
		Label lblDestPath = new Label(shell, SWT.NONE);
		lblDestPath.setText("Destination Path:");
		
		txtDestPath = new Text(shell, SWT.BORDER);
		txtDestPath.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		
		Label lblFolder = new Label(shell, SWT.NONE);
		lblFolder.setText("Subfolder:");
		
		txtFolder = new Text(shell, SWT.BORDER);
		txtFolder.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		new Label(shell, SWT.NONE);
		
		Composite cpsOptions = new Composite(shell, SWT.NONE);
		cpsOptions.setLayout(new GridLayout(4, false));
		
		chkWatchClipboard = new Button(cpsOptions, SWT.CHECK);
		chkWatchClipboard.setSelection(true);
		chkWatchClipboard.setText("Watch clipboard");
		
		chkUpperCaseFilenames = new Button(cpsOptions, SWT.CHECK);
		chkUpperCaseFilenames.setSelection(true);
		chkUpperCaseFilenames.setText("Upper case filenames");
		
		chkOverwrite = new Button(cpsOptions, SWT.CHECK);
		chkOverwrite.setSelection(true);
		chkOverwrite.setText("Overwrite existing files");
		
		chkBeep = new Button(cpsOptions, SWT.CHECK);
		chkBeep.setSelection(true);
		chkBeep.setText("Beep");
		
		Label lblImportedFiles = new Label(shell, SWT.NONE);
		lblImportedFiles.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, true, 1, 1));
		lblImportedFiles.setText("Imported files:");
		
		txtImportedFiles = new Text(shell, SWT.BORDER | SWT.V_SCROLL | SWT.MULTI);
		txtImportedFiles.setEditable(false);
		txtImportedFiles.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
		new Label(shell, SWT.NONE);
		
		Composite composite = new Composite(shell, SWT.NONE);
		composite.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		composite.setLayout(new GridLayout(2, false));
		
		lblImportedStats = new Label(composite, SWT.NONE);
		lblImportedStats.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		lblImportedStats.setBounds(0, 0, 55, 15);
		lblImportedStats.setText("Ready.");
		
		Button btnClose = new Button(composite, SWT.NONE);
		btnClose.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				shell.dispose();
			}
		});
		GridData gd_btnClose = new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1);
		gd_btnClose.widthHint = 70;
		btnClose.setLayoutData(gd_btnClose);
		btnClose.setBounds(0, 0, 75, 25);
		btnClose.setText("Close");
	}
	
   private void checkClipboard() {
      if (chkWatchClipboard.getSelection() && SystemClipboard.containsText()) {
	      String text = SystemClipboard.getText();
	      if (!lastClipboard.equals(text)) {
		      lastClipboard = text;
		      boolean success = processCode(text);
		      beep(success);
	      }
      }
   }
   
   private boolean processCode(String text) {
      String fileName = ABAP.inferFileNameFromCode(text);
      if (StringUtil.isNullOrEmpty(fileName)) {
         lblImportedStats.setText("filename could not be inferred");
         return false;
      }

      if (chkUpperCaseFilenames.getSelection())
         fileName = fileName.toUpperCase();

      Persistency persistency = Persistency.get();
      String folder = txtFolder.getText();
      String folderFile = StringUtil.isNullOrEmpty(folder) ? fileName : persistency.combinePaths(folder, fileName);
      try {
         String dir = StringUtil.isNullOrEmpty(folder) ? txtDestPath.getText() : persistency.combinePaths(txtDestPath.getText(), folder);
         String path = persistency.combinePaths(dir, fileName + EXTENSION);

         persistency.ensureDirectoryExists(dir);
         if (persistency.fileExists(path) && !chkOverwrite.getSelection()) {
            lblImportedStats.setText("file '" + folderFile + "' already exists; consider activating 'Overwrite'");
            return false;
         }

         persistency.writeAllTextToFile(path, text);

      } catch (RuntimeException ex) {
         lblImportedStats.setText("exception saving " + folderFile + ": " + ex.getMessage());
         return false;
      }

      // update statistics
      int lineCount = StringUtil.instrCount(text, '\n') + (text.endsWith("\n") ? 0 : 1);
      importedFiles.append(folderFile + " (" + Cult.format(lineCount) + " lines)" + System.lineSeparator());
      txtImportedFiles.setText(importedFiles.toString());

      ++totalImportedFileCount;
      totalImportedLineCount += lineCount;
      lblImportedStats.setText("Imported " + Cult.format(totalImportedFileCount) + " files with " + Cult.format(totalImportedLineCount) + " lines");
      
      return true;
   }

   private static void beep(boolean success) {
   	if (success)
   		Display.getCurrent().beep();
   }
}
