package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.wb.swt.SWTResourceManager;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;

import com.sap.adt.abapcleaner.programbase.*;

public class FrmProgress {
   private static final int UPDATE_INTERVAL_MS = 100;

   private BackgroundJob job;
	private boolean result;

	private Shell shell;
	private ProgressBar pgbParser;
	private ProgressBar pgbCleaner;
	private ProgressBar pgbComparer;
	private ProgressBar pgbIntegrityTest;
	private Label lblIntegrityTest;

	/**
	 * Open the dialog.
	 * @return 
	 * @wbp.parser.entryPoint
	 */
	public boolean open(BackgroundJob job) {
		this.job = job;
		this.result = false;
		
		createContents();

      pgbParser.setSelection(0);
      pgbCleaner.setSelection(0);
      pgbComparer.setSelection(0);
      pgbIntegrityTest.setSelection(0);

      updateText(job.getInitialProgress().getTitle());

		shell.open();
		shell.layout();

		pgbIntegrityTest.setVisible(false);
		lblIntegrityTest.setVisible(false);

		// start the job in a separate thread until it is done or cancelled 
		new Thread() {
			@Override
			public void run() {
				job.run();
			}
		}.start();
		
		// regularly get progress updates on the display thread
		Display display = shell.getDisplay();
		display.timerExec(UPDATE_INTERVAL_MS, new Runnable() {
			public void run() {
				JobProgress progress = job.consumeLatestProgress();
				if (progress != null)
					progressChanged(progress);
				
				if (job.isDone()) {
					shell.dispose();
				} else {
					display.timerExec(UPDATE_INTERVAL_MS, this);
				}
			}
		});
		
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}

		result = !job.wasCancelled();
		return result;
	}

	/**
	 * Create contents of the dialog.
	 */
	private void createContents() {
		shell = new Shell(SWT.APPLICATION_MODAL | SWT.BORDER | SWT.TITLE | SWT.TOOL);
		shell.setImage(SWTResourceManager.getImage(FrmProgress.class, "/ShellImage.png"));
		shell.setSize(578, 209);
		shell.setText("Progress");
		GridLayout gl_shell = new GridLayout(2, false);
		gl_shell.horizontalSpacing = 12;
		gl_shell.verticalSpacing = 8;
		gl_shell.marginWidth = 12;
		gl_shell.marginHeight = 12;
		shell.setLayout(gl_shell);
		
		Label lblParser = new Label(shell, SWT.NONE);
		lblParser.setText("Parser:");
		
		pgbParser = new ProgressBar(shell, SWT.NONE);
		pgbParser.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		
		Label lblCleaner = new Label(shell, SWT.NONE);
		lblCleaner.setText("Cleaner:");
		
		pgbCleaner = new ProgressBar(shell, SWT.NONE);
		pgbCleaner.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		
		Label lblComparer = new Label(shell, SWT.NONE);
		lblComparer.setText("Comparer:");
		
		pgbComparer = new ProgressBar(shell, SWT.NONE);
		pgbComparer.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		
		lblIntegrityTest = new Label(shell, SWT.NONE);
		lblIntegrityTest.setText("Integrity Test:");
		
		pgbIntegrityTest = new ProgressBar(shell, SWT.NONE);
		pgbIntegrityTest.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		new Label(shell, SWT.NONE);
		
		Button btnCancel = new Button(shell, SWT.NONE);
		btnCancel.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				result = false;
				if (job != null)
					job.cancel();
			}
		});
		btnCancel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		btnCancel.setText("&Cancel");

	}

   private void updateText(String text) {
      if (!shell.getText().equals(text))
         shell.setText(text);
   }

   private void progressChanged(JobProgress progress) {
      updateText(progress.getTitle());

      setProgressBar(pgbParser, progress.getPercentageOf(TaskType.PARSER));
      setProgressBar(pgbCleaner, progress.getPercentageOf(TaskType.CLEANER));
      setProgressBar(pgbComparer, progress.getPercentageOf(TaskType.COMPARER));
      setProgressBar(pgbIntegrityTest, progress.getPercentageOf(TaskType.INTEGRITY_TEST));
   }

   private void setProgressBar(ProgressBar pgb, int percentage) {
      if (pgb.getSelection() != percentage)
         pgb.setSelection(percentage);
   }
}
