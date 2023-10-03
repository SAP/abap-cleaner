package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.wb.swt.SWTResourceManager;

import com.sap.adt.abapcleaner.programbase.Persistency;

import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.graphics.Point;

public class FrmInputBox {
   private final String invalidChars;
   private boolean restrictToValidFileNameChars;
   private String result;
   
	protected Shell shell;
	private Text txtResult;

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

	protected void createContents() {
		shell = new Shell(SWT.APPLICATION_MODAL | SWT.BORDER | SWT.CLOSE | SWT.RESIZE | SWT.TITLE);
		shell.setMinimumSize(new Point(450, 130));
		shell.setSize(450, 130);
		shell.setImage(SWTResourceManager.getImage(FrmInputBox.class, "/ShellImage.png"));
		shell.setText("Input Box");
		GridLayout gl_shell = new GridLayout(1, false);
		gl_shell.marginWidth = 10;
		gl_shell.marginHeight = 10;
		shell.setLayout(gl_shell);
		
		txtResult = new Text(shell, SWT.BORDER);
		GridData gd_txtResult = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
		gd_txtResult.widthHint = 400;
		txtResult.setLayoutData(gd_txtResult);
		txtResult.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				txtResultKeyPressed(e);
			}
		});
		
		Composite cpsGrabVerticalSpace = new Composite(shell, SWT.NONE);
		cpsGrabVerticalSpace.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));

		Composite cpsButtons = new Composite(shell, SWT.NONE);
		cpsButtons.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		GridLayout gl_cpsButtons = new GridLayout(3, false);
		gl_cpsButtons.marginWidth = 0;
		gl_cpsButtons.marginHeight = 0;
		cpsButtons.setLayout(gl_cpsButtons);
		
		Label lblExcessWidth = new Label(cpsButtons, SWT.NONE);
		lblExcessWidth.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true, false, 1, 1));
		
		Button btnCancel = new Button(cpsButtons, SWT.NONE);
		btnCancel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		btnCancel.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
	         setResult(null);
				shell.dispose();
			}
		});
		btnCancel.setText("&Cancel");
		
		Button btnOK = new Button(cpsButtons, SWT.NONE);
		btnOK.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		btnOK.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
	         setResult(txtResult.getText());
				shell.dispose();
			}
		});
		btnOK.setText("&OK");
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
