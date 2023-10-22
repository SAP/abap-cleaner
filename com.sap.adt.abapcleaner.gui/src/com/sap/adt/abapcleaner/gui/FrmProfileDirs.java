package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Label;

import java.util.HashSet;

import org.eclipse.swt.SWT;
import org.eclipse.wb.swt.SWTResourceManager;

import com.sap.adt.abapcleaner.base.Cult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.FileType;
import com.sap.adt.abapcleaner.programbase.Persistency;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.ProfileDir;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;

public class FrmProfileDirs {
	private static final int READ_ONLY_DIR_COUNT = 3;
	
	private MainSettings settings;

	protected Shell shell;

	private Text txtOwnDir;
	private Text txtShortName_1;
	private Text txtShortName_2;
	private Text txtShortName_3;
	private Text txtReadOnlyDir_1;
	private Text txtReadOnlyDir_2;
	private Text txtReadOnlyDir_3;

	public FrmProfileDirs(MainSettings settings) {
		this.settings = settings;
	}

	/**
	 * @wbp.parser.entryPoint
	 */
	public void open() {
		Display display = Display.getDefault();
		createContents();

		// fill text fields from settings
		txtOwnDir.setText(settings.profilesDirectory);
		
		Text[] shortNameTexts = new Text[] { txtShortName_1, txtShortName_2, txtShortName_3 }; 
		Text[] readOnlyDirTexts = new Text[] { txtReadOnlyDir_1, txtReadOnlyDir_2, txtReadOnlyDir_3 }; 
		for (int i = 0; i < READ_ONLY_DIR_COUNT; ++i) {
			String shortName = "";
			String readOnlyDir = "";
			if (i < settings.readOnlyProfileDirs.size()) {
				ProfileDir profileDir = settings.readOnlyProfileDirs.get(i); 
				shortName = profileDir.shortName;
				readOnlyDir = profileDir.readOnlyDir;
			}
			shortNameTexts[i].setText(shortName);
			readOnlyDirTexts[i].setText(readOnlyDir);
		}
		
		shell.open();
		shell.layout();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}
	}

	private void createContents() {
		shell = new Shell(SWT.APPLICATION_MODAL | SWT.BORDER | SWT.CLOSE | SWT.RESIZE | SWT.TITLE);
		shell.setMinimumSize(new Point(800, 461));
		shell.setSize(800, 461);
		shell.setImage(SWTResourceManager.getImage(FrmInputBox.class, "/ShellImage.png"));
		shell.setText("Profile Folders");
		GridLayout gl_shell = new GridLayout(1, false);
		gl_shell.marginWidth = 20;
		gl_shell.marginHeight = 15;
		shell.setLayout(gl_shell);
		
		Label lblOwnDirTitle = new Label(shell, SWT.NONE);
		lblOwnDirTitle.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		lblOwnDirTitle.setText("Own Profiles (Read-Write)");
		
		Label lblOwnDirInfo = new Label(shell, SWT.WRAP);
		lblOwnDirInfo.setText("Your own profile folder allows you to create, copy and modify profiles:");
		
		Composite cpsOwnDir = new Composite(shell, SWT.NONE);
		cpsOwnDir.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		cpsOwnDir.setLayout(new GridLayout(2, false));
		
		txtOwnDir = new Text(cpsOwnDir, SWT.BORDER);
		txtOwnDir.setEditable(false);
		txtOwnDir.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		
		Button btnSelectOwnDir = new Button(cpsOwnDir, SWT.NONE);
		btnSelectOwnDir.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectOwnDir();
			}
		});
		btnSelectOwnDir.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		btnSelectOwnDir.setText("Move...");
		
		Label lblReadOnlyDirTitle = new Label(shell, SWT.NONE);
		GridData gd_lblReadOnlyDirTitle = new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1);
		gd_lblReadOnlyDirTitle.verticalIndent = 10;
		lblReadOnlyDirTitle.setLayoutData(gd_lblReadOnlyDirTitle);
		lblReadOnlyDirTitle.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		lblReadOnlyDirTitle.setText("Team Profiles (Read-Only)");
		
		Label lblReadOnlyDirInfo = new Label(shell, SWT.NONE);
		lblReadOnlyDirInfo.setText("You can specify up to 3 (typically synchronized) folders with team profiles which are used read-only.");
		
		Label lblShortNameInfo = new Label(shell, SWT.NONE);
		lblShortNameInfo.setText("Please provide a short name for each folder (e.g. \"team A\") for display in the profile list.");
		
		Composite cpsReadOnlyDirs = new Composite(shell, SWT.NONE);
		GridData gd_cpsReadOnlyDirs = new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1);
		gd_cpsReadOnlyDirs.verticalIndent = 10;
		cpsReadOnlyDirs.setLayoutData(gd_cpsReadOnlyDirs);
		cpsReadOnlyDirs.setLayout(new GridLayout(4, false));
		
		Label lblShortNameColHeader = new Label(cpsReadOnlyDirs, SWT.NONE);
		lblShortNameColHeader.setText("Short Name");
		
		Label lblReadOnlyDirColHeader = new Label(cpsReadOnlyDirs, SWT.NONE);
		lblReadOnlyDirColHeader.setText("Folder for Team Profiles");
		new Label(cpsReadOnlyDirs, SWT.NONE);
		new Label(cpsReadOnlyDirs, SWT.NONE);
		
		
		txtShortName_1 = new Text(cpsReadOnlyDirs, SWT.BORDER);
		GridData gd_txtShortName_1 = new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1);
		gd_txtShortName_1.widthHint = 100;
		txtShortName_1.setLayoutData(gd_txtShortName_1);
		
		txtReadOnlyDir_1 = new Text(cpsReadOnlyDirs, SWT.BORDER);
		txtReadOnlyDir_1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		
		Button btnSelectReadOnlyDir_1 = new Button(cpsReadOnlyDirs, SWT.NONE);
		btnSelectReadOnlyDir_1.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectReadOnlyDir(txtReadOnlyDir_1);
			}
		});
		btnSelectReadOnlyDir_1.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		btnSelectReadOnlyDir_1.setText("Select...");
		
		Button btnClearReadOnlyDir_1 = new Button(cpsReadOnlyDirs, SWT.NONE);
		btnClearReadOnlyDir_1.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				clearReadOnlyDir(txtShortName_1, txtReadOnlyDir_1);
			}
		});
		btnClearReadOnlyDir_1.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		btnClearReadOnlyDir_1.setText("Clear");
		
		
		txtShortName_2 = new Text(cpsReadOnlyDirs, SWT.BORDER);
		txtShortName_2.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		
		txtReadOnlyDir_2 = new Text(cpsReadOnlyDirs, SWT.BORDER);
		txtReadOnlyDir_2.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		
		Button btnSelectReadOnlyDir_2 = new Button(cpsReadOnlyDirs, SWT.NONE);
		btnSelectReadOnlyDir_2.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectReadOnlyDir(txtReadOnlyDir_2);
			}
		});
		btnSelectReadOnlyDir_2.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false, 1, 1));
		btnSelectReadOnlyDir_2.setText("Select...");
		
		Button btnClearReadOnlyDir_2 = new Button(cpsReadOnlyDirs, SWT.NONE);
		btnClearReadOnlyDir_2.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				clearReadOnlyDir(txtShortName_2, txtReadOnlyDir_2);
			}
		});
		btnClearReadOnlyDir_2.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		btnClearReadOnlyDir_2.setText("Clear");
		
		
		txtShortName_3 = new Text(cpsReadOnlyDirs, SWT.BORDER);
		txtShortName_3.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		
		txtReadOnlyDir_3 = new Text(cpsReadOnlyDirs, SWT.BORDER);
		txtReadOnlyDir_3.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		
		Button btnSelectReadOnlyDir_3 = new Button(cpsReadOnlyDirs, SWT.NONE);
		btnSelectReadOnlyDir_3.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectReadOnlyDir(txtReadOnlyDir_3);
			}
		});
		btnSelectReadOnlyDir_3.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		btnSelectReadOnlyDir_3.setText("Select...");
		
		Button btnClearReadOnlyDir_3 = new Button(cpsReadOnlyDirs, SWT.NONE);
		btnClearReadOnlyDir_3.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				clearReadOnlyDir(txtShortName_3, txtReadOnlyDir_3);
			}
		});
		btnClearReadOnlyDir_3.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		btnClearReadOnlyDir_3.setText("Clear");
		
		
		Composite cpsGrabVerticalSpace = new Composite(shell, SWT.NONE);
		cpsGrabVerticalSpace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, true, 1, 1));
		
		Composite cpsCancelOrOK = new Composite(shell, SWT.NONE);
		cpsCancelOrOK.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true, false, 1, 1));
		GridLayout gl_cpsCancelOrOK = new GridLayout(1, false);
		gl_cpsCancelOrOK.marginTop = 10;
		cpsCancelOrOK.setLayout(gl_cpsCancelOrOK);
		
		Button btnOk = new Button(cpsCancelOrOK, SWT.NONE);
		btnOk.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				updateSettings();
				shell.dispose();
			}
		});
		btnOk.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		btnOk.setText("OK");
	}

   private void selectOwnDir() {
   	final String title = "Select Folder for Own Profiles";
   	final String twoLineSeps = System.lineSeparator() + System.lineSeparator();
   	
   	// prepare dialog
   	Persistency persistency = Persistency.get();
		String oldDir = txtOwnDir.getText();
		if (StringUtil.isNullOrEmpty(oldDir)) 
			oldDir = persistency.getDirectoryName(persistency.getSavePath(FileType.PROFILE_TEXT, Profile.DEFAULT_NAME));

   	// display dialog for selection of new profiles folder
		String newDir = showDirDialog(oldDir, title);
		if (newDir == null || newDir.length() == 0) {
			return;
		} else if (persistency.addDirSep(oldDir).equals(persistency.addDirSep(newDir))) {
			// same folder was selected
			return;
		}

		// determine destination path
		newDir = persistency.addDirSep(newDir);

		// determine files that would be overwritten when moving profiles to the new folder
		String[] oldPaths = Profile.getLoadPaths(oldDir);
		StringBuilder sbExisting = new StringBuilder();
		int existingCount = 0;
		for (String oldPath : oldPaths) {
			String file = persistency.getFileName(oldPath);
			String newPath = persistency.combinePaths(newDir, file);
			if (persistency.fileExists(newPath)) {
				if (sbExisting.length() > 0)
					sbExisting.append(", ");
				sbExisting.append(persistency.getFileNameWithoutExtension(newPath));
				++existingCount;
			}
		}

		// ask whether existing files in the new folder shall be overwritten
		if (existingCount > 0) {
			String overwriteTitle = (existingCount == 1) ? "Overwrite file?" : "Overwrite " + Cult.format(existingCount) + " files?";  
			String existingInfo = (existingCount == 1) ? "the existing file" : Cult.format(existingCount) + " existing files";
			String msg = "Moving profiles to the new folder would overwrite " + existingInfo + " '" + sbExisting.toString() + "'." + twoLineSeps + "Continue?";
			if (Message.show(msg, overwriteTitle, SWT.YES | SWT.NO | SWT.CANCEL | SWT.ICON_QUESTION, shell) != SWT.YES) {
				Message.show("Action cancelled; keeping current profiles folder '" + oldDir + "'.", title, shell);
				return;
			}
		}
		
		// move profiles
		int moveCount = 0;
		int notMovedCount = 0;
		for (String oldPath : oldPaths) {
			String file = persistency.getFileName(oldPath);
			String newPath = persistency.combinePaths(newDir, file);
			if (persistency.moveFile(oldPath, newPath, true)) {
				++moveCount;
			} else {
				++notMovedCount;
			}
		} 

		// if no profile could be moved, do not change the directory
		if (moveCount == 0 && oldPaths.length > 0) {
			Message.show("Existing profile(s) could not be moved to the new folder; keeping current folder '" + oldDir + "'.", title, shell);
			return;	
		}

		txtOwnDir.setText(newDir);

		// change the profiles directory in the settings and save, so this change is persisted even if FrmProfiles is closed with the Cancel button
		settings.profilesDirectory = newDir;
		settings.save();
		
		// show result message
		String result; 
		result = "Profiles Folder was changed from '" + oldDir + "' to '" + newDir + "'." + twoLineSeps;
		result += (moveCount == 1) ? "1 profile was moved to the new folder." : Cult.format(moveCount) + " profiles were moved to the new folder.";
		if (notMovedCount > 0) {
			result += twoLineSeps + Cult.format(notMovedCount) + " profile(s) kept in the old folder, as they could not be moved.";
		}
		Message.show(result, title, shell);
   }

   private void selectReadOnlyDir(Text txtReadOnlyDir) {
   	final String title = "Select Folder with Team Profiles (Read-Only)";

   	// prepare dialog
   	Persistency persistency = Persistency.get();
   	String oldDir = txtReadOnlyDir.getText();
		if (StringUtil.isNullOrEmpty(oldDir))
			oldDir = "";

		String newDir = showDirDialog(oldDir, title);
		if (newDir == null)
			return;
		newDir = persistency.addDirSep(newDir);
		txtReadOnlyDir.setText(newDir);
	}
	
	private String showDirDialog(String filterPath, String message) {
		DirectoryDialog dirDialog = new DirectoryDialog(shell);
		dirDialog.setFilterPath(filterPath);
		dirDialog.setMessage(message);
		return dirDialog.open();
	}

	private void clearReadOnlyDir(Text txtShortName, Text txtReadOnlyDir) {
		txtShortName.setText("");
		txtReadOnlyDir.setText("");
	}
	
	private void updateSettings() {
		HashSet<String> existingShortNames = new HashSet<String>();
		HashSet<String> existingReadOnlyDirs = new HashSet<String>();
		String[] shortNames = new String[] { txtShortName_1.getText(), txtShortName_2.getText(), txtShortName_3.getText() }; 
		String[] readOnlyDirs = new String[] { txtReadOnlyDir_1.getText(), txtReadOnlyDir_2.getText(), txtReadOnlyDir_3.getText() }; 
		
		settings.readOnlyProfileDirs.clear();
		
   	Persistency persistency = Persistency.get();
		for (int i = 0; i < READ_ONLY_DIR_COUNT; ++i) {
			// ignore empty txtReadOnlyDir_... text fields (even if a txtShortName_... was entered)
			String readOnlyDir = readOnlyDirs[i];
			if (StringUtil.isNullOrEmpty(readOnlyDir))
				continue;
			readOnlyDir = persistency.addDirSep(readOnlyDir);
			
			// do not add the read-only folder if it is equal to the own folder or an existing read-only folder 
			if (readOnlyDir.equals(persistency.addDirSep(settings.profilesDirectory)) || existingReadOnlyDirs.contains(readOnlyDir)) 
				continue;
			existingReadOnlyDirs.add(readOnlyDir);
			
			// get or otherwise create the short name 
			String shortName = ProfileDir.standardizeShortName(shortNames[i]);
			if (StringUtil.isNullOrEmpty(shortName)) 
				shortName = ProfileDir.getDefaultShortName(i);
			
			// ensure the short name is unique (by adding a number "2" etc., if needed)
			String uniqueShortName = shortName;
			int num = 1;
			while (existingShortNames.contains(uniqueShortName)) {
				++num;
				uniqueShortName = shortName + String.valueOf(num);
			}
			shortName = uniqueShortName;
			existingShortNames.add(shortName);
			
			settings.readOnlyProfileDirs.add(new ProfileDir(shortName, readOnlyDir));
		}
	}
}
