package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.wb.swt.SWTResourceManager;

import com.sap.adt.abapcleaner.programbase.Persistency;

import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;

public class FrmInputBox{
   private final String invalidChars;
   private boolean restrictToValidFileNameChars;
   private String result;
   
	protected Shell shell;
	private Text txtResult;

	/**
	 * Create the dialog.
	 * @param parent
	 * @param style
	 */
	public FrmInputBox() {
      invalidChars = new String(Persistency.get().getInvalidFileNameChars());
	}

	/**
	 * @wbp.parser.entryPoint
	 */
	public final String open(String defaultText, String title, boolean restrictToValidFileNameChars) {
      Display display = Display.getDefault();
		createContents();

		txtResult.setText(defaultText);
		shell.setText(title);
      this.restrictToValidFileNameChars = restrictToValidFileNameChars;
		result = null;
		
		shell.open();
		shell.layout();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}
      return result;
   }

	/**
	 * Create contents of the window.
	 */
	protected void createContents() {
		shell = new Shell(SWT.APPLICATION_MODAL | SWT.BORDER | SWT.TITLE | SWT.CLOSE | SWT.TOOL);
		shell.setImage(SWTResourceManager.getImage(FrmInputBox.class, "/ShellImage.png"));
		shell.setSize(450, 120);
		shell.setText("Input Box");
		FormLayout fl_shell = new FormLayout();
		fl_shell.marginHeight = 12;
		fl_shell.marginWidth = 12;
		shell.setLayout(fl_shell);
		
		txtResult = new Text(shell, SWT.BORDER);
		txtResult.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				txtResultKeyPressed(e);
			}
		});
		FormData fd_txtResult = new FormData();
		fd_txtResult.left = new FormAttachment(0, 10);
		fd_txtResult.right = new FormAttachment(100, -10);
		txtResult.setLayoutData(fd_txtResult);
		
		Button btnOK = new Button(shell, SWT.NONE);
		btnOK.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
	         setResult(txtResult.getText());
				shell.dispose();
			}
		});
		fd_txtResult.bottom = new FormAttachment(btnOK, -6);
		FormData fd_btnOK = new FormData();
		fd_btnOK.top = new FormAttachment(0, 37);
		fd_btnOK.right = new FormAttachment(100, -10);
		btnOK.setLayoutData(fd_btnOK);
		btnOK.setText("&OK");
		
		Button btnCancel = new Button(shell, SWT.NONE);
		btnCancel.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
	         setResult(null);
				shell.dispose();
			}
		});
		FormData fd_btnCancel = new FormData();
		fd_btnCancel.top = new FormAttachment(txtResult, 6);
		fd_btnCancel.right = new FormAttachment(btnOK, -6);
		btnCancel.setLayoutData(fd_btnCancel);
		btnCancel.setText("&Cancel");

	}

   private void txtResultKeyPressed(KeyEvent e) {
      if (e.character == '\b')
      	return;
      
      if (e.character == '\r') {
         e.doit = false;
         setResult(txtResult.getText());
         shell.dispose();
         return;
      }
      if (e.character >= 32 && restrictToValidFileNameChars && invalidChars.indexOf(e.character) >= 0)
         e.doit = false;
   }
   
   private void setResult(String value) {
      if (restrictToValidFileNameChars && value != null) {
         StringBuilder validChars = new StringBuilder();
         char[] resultChars = value.toCharArray();
         for (char c : resultChars)
            validChars.append((invalidChars.indexOf(c) >= 0) ? '_' : c);
         result = validChars.toString();
      } else {
      	result = value;
      }
   }
}
