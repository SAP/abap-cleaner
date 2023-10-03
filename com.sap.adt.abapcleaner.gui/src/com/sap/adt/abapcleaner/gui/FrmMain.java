package com.sap.adt.abapcleaner.gui;

import java.io.IOException;
import java.io.PrintStream;
import java.util.*;
import org.eclipse.wb.swt.SWTResourceManager;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.custom.BusyIndicator;

import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.GridData;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData; 

import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;
import com.sap.adt.abapcleaner.comparer.*;
import com.sap.adt.abapcleaner.comparer.ChangeTypes;

import org.eclipse.swt.graphics.Point;

public class FrmMain implements IUsedRulesDisplay, ISearchControls, IChangeTypeControls, IFallbackKeyListener {
	private static final String SEARCH_INFO = "(press Ctrl + F and type search string)";
	private static final String TYPE_SEARCH_TEXT = "type search text (Escape = exit)";
	private static final String RELEASE_RESTRICTION_PREFIX = "ABAP ";
	private static final String NO_RELEASE_RESTRICTION_DISPLAY = "Latest ABAP Release";
	private static final int WATCH_CLIPBOARD_INTERVAL_MS = 100;

	private static boolean isInitialized = false;

	private CodeDisplayColors codeDisplayColors; 
	private static final Color searchTextNotFoundColor = new Color(139, 0, 54); // dark red
	private static Color searchInfoColor;
	private static Color searchTextNormalColor;
	private Font searchInfoFont;
	private Font searchTextFont;

	private boolean isPlugin;
	private Code resultCode;
	private String resultErrorMessage;
	
	private MainSettings settings;

	protected Shell shell;

	private MenuItem mmuCodeFromClipboard;
	private MenuItem mmuCodeFromFile;
	private MenuItem mmuCodeWatchClipboard;
	private MenuItem mmuCodeClearDisplay;
	private MenuItem mmuCodeSeparator;
	private MenuItem mmuCodeCancel;
	private MenuItem mmuCodeApplyAndClose;
	private MenuItem mmuCodeExit;
	private MenuItem mmuHighlightDeclarationKeywords;
	private MenuItem mmuHighlightWritePositions;
	private MenuItem mmuExtras;

	private CodeDisplay codeDisplay;

	private Composite pnlRules;
	private Label lblWatchClipboardInfo;
	private Combo cboProfile;
	private Label lblCleanupRangeExpandMode;
	private Combo cboCleanupRangeExpandMode;
	private Label lblCleanupRangeExpandModeEmptyCell;
	private Combo cboReleaseRestriction;
	
	private Button chkHighlightIndentChanges;
	private Button chkHighlightInnerSpaceChanges;
	private Button chkHighlightCaseChanges;
	private Button chkHighlightContentChanges;

	private Label lblSearch;
	private Button chkSearchLeftDisplay;
	private Button chkSearchRightDisplay;
	private Button chkMatchCase;
	private Button chkMatchWholeWord;
	private Button chkSearchChangedLines;

	private Composite cpsApplyOrCancel;
	private Button btnApplyAndClose;
	private Button btnCancel;

	private Table lstUsedRules;

	private static String defaultCodeDirectory;

	private ArrayList<Profile> profiles = new ArrayList<Profile>();
	private Profile curProfile = Profile.createDefault();
	private CleanupRange originalCleanupRange;
	
	private RuleStats[] usedRules;

	private int suspendItemCheck;
	
	/**
	 * Launch the application.
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			initialize();

			// for the stand-alone application, arguments must be retrieved from the Platform
			Persistency persistency = Persistency.get();
			if (args == null || args.length == 0)
				args = Platform.getApplicationArgs();
			CommandLineArgs commandLineArgs = CommandLineArgs.create(persistency, args);

			if (commandLineArgs == null) {
				// start the interactive (stand-alone) UI 
				cleanInteractively(null, ABAP.NEWEST_RELEASE, null, false, null, null, false);

			} else {
				if (commandLineArgs.hasErrors()) {
					if (commandLineArgs.hasErrors()) {
						System.err.println(commandLineArgs.errors);
						System.err.println("");
						System.err.flush();
					}
					System.out.print(CommandLineArgs.getHelp(persistency));

				} else if (commandLineArgs.showHelp) {
					System.out.print(CommandLineArgs.getHelp(persistency));
				
				} else {
					// use application args for automatic cleanup
					cleanAutomatically(commandLineArgs, System.out);
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private static void initialize() {
		if (isInitialized)
			return;
		
		Program.initialize(null, "");
		isInitialized = true;
	}

	public static void cleanAutomatically(CommandLineArgs commandLineArgs, PrintStream out) throws CommandLineException {
		// create or read a profile
		Profile profile = null;
		if (StringUtil.isNullOrEmpty(commandLineArgs.profileData)) {
			profile = Profile.createDefault();
		} else {
			try (ISettingsReader reader = TextSettingsReader.createFromString(commandLineArgs.profileData, Program.TECHNICAL_VERSION)) {
				profile = Profile.createFromSettings(reader, "");
			} catch(IOException ex) {
				out.println(ex.getMessage());
				return;
			}
		}

		if (commandLineArgs.isInSingleSourceMode()) {
			cleanSingleSourceAutomatically(commandLineArgs, commandLineArgs.sourceCode, out, profile);
		} else {
			cleanMultiAutomatically(commandLineArgs, out, profile);
		}
	}
	
	private static void cleanMultiAutomatically(CommandLineArgs commandLineArgs, PrintStream out, Profile profile) {
		Persistency persistency = Persistency.get();
		int baseSourcePathLength = commandLineArgs.sourceDir.length();
		
	  	for (String sourcePath : commandLineArgs.sourcePaths) {
			String sourceCode = persistency.readAllTextFromFile(sourcePath);

			CleanupResult result = cleanAutomatically(sourceCode, commandLineArgs.abapRelease, commandLineArgs.cleanupRange, profile, commandLineArgs.showStats);
		  	if (result == null) {
				out.println("Cleanup for file " + sourcePath + " cancelled.");
				continue;
		  	} else if (result.hasErrorMessage()) {
		  		out.println("Errors during clean-up of file: " + sourcePath);
		  		out.println(result.errorMessage);
		  		continue;
		  	}

		  	String sourceFolderFile = sourcePath.substring(baseSourcePathLength);
			writeCleanUpResult(commandLineArgs, out, result, sourceFolderFile, persistency.combinePaths(commandLineArgs.targetDir, sourceFolderFile));
	  	}
	}

	private static void cleanSingleSourceAutomatically(CommandLineArgs commandLineArgs, String sourceCode, PrintStream out, Profile profile) {
		CleanupResult result = cleanAutomatically(commandLineArgs.sourceCode, commandLineArgs.abapRelease, commandLineArgs.cleanupRange, profile, commandLineArgs.showStats);
		if (result == null) {
			out.println("Cleanup cancelled.");
			return;
		} else if (result.hasErrorMessage()) {
			out.println(result.errorMessage);
			return;
		}
		
		writeCleanUpResult(commandLineArgs, out, result, null, commandLineArgs.targetPath);
	}

	private static void writeCleanUpResult(CommandLineArgs commandLineArgs, PrintStream out, CleanupResult result, String sourceFolderFile, String targetPath) {
	  	// the main output is either the whole code document or the cleanup result of the line selection 
	  	String output = null;
	  	if (result.hasCleanedCode()) {
		  	if (commandLineArgs.partialResult && result.hasLineSelection()) {
			  	output = result.getSelectedText(); 
		  	} else {
			  	output = result.getCleanedCode();
		  	}
	  	}
		if (output == null) {
			return;
		}

		// write statistics and/or used cleanup rules, if requested
		if (commandLineArgs.showStats || commandLineArgs.showUsedRules) {
			// in case of multiple files, start with the current folder and file
			if (!StringUtil.isNullOrEmpty(sourceFolderFile))
				out.println(sourceFolderFile);
			
			if (commandLineArgs.showStats) 
				out.println(result.getStatsSummary());
			if (commandLineArgs.showUsedRules) 
				out.print(result.getRuleStats());

			out.println();
		}
		
		if (commandLineArgs.writesResultCodeToOutput()) {
			out.print(output);
		} else {
			Persistency persistency = Persistency.get(); 
			if (commandLineArgs.overwrite || !persistency.fileExists(targetPath)) {
				persistency.ensureDirectoryExistsForPath(targetPath);
				// for abapGit, ensure a final line separator
				if (output.length() > 0 && ABAP.LINE_SEPARATOR.indexOf(output.charAt(output.length() - 1)) < 0)
					output += ABAP.LINE_SEPARATOR;
				persistency.writeAllTextToFile(targetPath, output);
			}
		}
	}

	public static CleanupResult cleanAutomatically(String sourceCode, String abapRelease, CleanupRange cleanupRange, Profile profile, boolean provideRuleStats) {
		initialize();

		MainSettings settings = new MainSettings();
		settings.load();
		if (profile == null)
			profile = getMostRecentlyUsedProfile(settings);
		
		BackgroundJob job = new BackgroundJob(ParseParams.createForCleanupRange("", sourceCode, abapRelease, cleanupRange, settings.cleanupRangeExpandMode),
													     CleanupParams.createForProfile(profile, false, settings.releaseRestriction));
		job.run();
		Task result = job.getResult();

		if (result.getSuccess()) {
			CleanupResult cleanupResult = result.getResultingCode().toCleanupResult(); 
			
			if (provideRuleStats) {
				StringBuilder stats = new StringBuilder();
				RuleStats[] ruleStats = result.getResultingDiffDoc().getRuleStats(profile);
				for (RuleStats ruleStat : ruleStats) {
					stats.append(ruleStat.toConsoleOutput()).append(System.lineSeparator());
				}
				cleanupResult.setStats(result, stats.toString());
			}
			return cleanupResult;
	
		} else if (job.wasCancelled()) {
			return null;
		
		} else {
			return CleanupResult.createError(result.getErrorMessage());
		}
	}
	
	public static CleanupResult cleanInteractively(String sourceCode, String abapRelease, CleanupRange cleanupRange, boolean isPlugin, String sourcePageTitle, CodeDisplayColors codeDisplayColors, boolean readOnly) {
		initialize();
		
		Persistency persistency = Persistency.get();
		
		String[] codeDirs = persistency.getExistingDirs(FileType.CODE);
		defaultCodeDirectory = (codeDirs == null || codeDirs.length == 0) ? persistency.getWorkDir() : codeDirs[0];

		FrmMain window = new FrmMain();
		window.codeDisplayColors = (codeDisplayColors != null) ? codeDisplayColors : CodeDisplayColors.createDefault();
		window.open(isPlugin, sourcePageTitle, sourceCode, abapRelease, cleanupRange, readOnly);

		if (window.resultCode != null)
			return window.resultCode.toCleanupResult(); 
		else if (!StringUtil.isNullOrEmpty(window.resultErrorMessage))
			return CleanupResult.createError(window.resultErrorMessage);
		else
			return null;
	}

	private static Profile getMostRecentlyUsedProfile(MainSettings settings) {
		ArrayList<Profile> profiles = Profile.loadProfiles(settings.profilesDirectory, settings.readOnlyProfileDirs);
		for (Profile profile : profiles) {
			if (profile.toString().equals(settings.curProfileName)) 
				return profile;
		}
		return Profile.createDefault();
	}

	public FrmMain() {
		settings = new MainSettings();
		settings.setDefault();
	}
	
	/**
	 * @wbp.parser.entryPoint
	 */
	public void open(boolean isPlugin, String sourcePageTitle, String sourceCode, String abapRelease, CleanupRange cleanupRange, boolean readOnly) {
		this.isPlugin = isPlugin;
		
		Display display = Display.getDefault();
		
		createContents();

		lblWatchClipboardInfo.setVisible(false);
		
		searchInfoColor = display.getSystemColor(SWT.COLOR_TITLE_INACTIVE_FOREGROUND); // SystemColors.ControlDark;
		searchTextNormalColor = display.getSystemColor(SWT.COLOR_LIST_FOREGROUND); // SystemColors.WindowText;
		FontData modelFont = lblSearch.getFont().getFontData()[0];
		searchInfoFont = new Font(display, modelFont.getName(), modelFont.getHeight(), SWT.NORMAL);
		searchTextFont = new Font(display, modelFont.getName(), modelFont.getHeight(), SWT.BOLD);

		loadSettings();

		codeDisplay.setUsedRulesDisplay(this);
		codeDisplay.setSearchControls(this);
		codeDisplay.setChangeTypeControls(this);
		codeDisplay.setFallbackKeyListener(this);
		refreshHighlight();
		
		if (isPlugin) {
			mmuCodeFromClipboard.dispose();
			mmuCodeFromFile.dispose();
			mmuCodeWatchClipboard.dispose();
			mmuCodeClearDisplay.dispose();
			mmuCodeSeparator.dispose();
			mmuCodeExit.dispose();
			
			if (readOnly) {
				mmuCodeApplyAndClose.dispose();
				mmuCodeCancel.setText(getMenuItemTextWithAccelerator("&Close Read-Only Preview", SWT.ESC));
				btnApplyAndClose.dispose();
				btnCancel.setText("Close Read-Only Preview");
				btnCancel.requestLayout();
			}
		} else {
			mmuCodeCancel.dispose();
			mmuCodeApplyAndClose.dispose();

			lblCleanupRangeExpandMode.dispose();
			cboCleanupRangeExpandMode.dispose();
			lblCleanupRangeExpandModeEmptyCell.dispose();
			
			btnApplyAndClose.dispose();
			btnCancel.dispose();
			cpsApplyOrCancel.dispose();
			pnlRules.layout();
		}

		boolean devFeatures = Program.showDevFeatures();
		if (!devFeatures || isPlugin)
			mmuExtras.dispose();

		// startup looks better if lstUsedRules is not visible
		updateChangedLineCount(ChangeStats.createEmpty());
		lstUsedRules.setVisible(false);

		shell.open();
		shell.layout();
		shell.update();
		
		if (!StringUtil.isNullOrEmpty(sourceCode)) {
			originalCleanupRange = cleanupRange;
			if (!refreshCode(sourcePageTitle, "", sourceCode, abapRelease, false, -1, -1, -1, cleanupRange, settings.cleanupRangeExpandMode) && isPlugin) {
				dispose();
				return;
			}

		} else {
			LastSession lastSession = LastSession.createEmpty();
			lastSession.load();
			if (!StringUtil.isNullOrEmpty(lastSession.getCodeText())) {
				lastSession.reloadCodeTextFromCodePath();
				refreshCode(lastSession.getSourceName(), lastSession.getSourcePath(), lastSession.getCodeText(), lastSession.getAbapRelease(), false, lastSession.getTopLineIndex(), lastSession.getCurLineIndex(),
						lastSession.getSelectionStartLine(), null, CleanupRangeExpandMode.FULL_DOCUMENT); 
			} else {
				refreshCode("", "", "", ABAP.NEWEST_RELEASE, false);
			}
		}
	
		lstUsedRules.setVisible(true);

		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}

		dispose();
	}

	private void dispose() {
		if (shell != null) {
			shell.dispose();
		}
		if (searchInfoFont != null) {
			searchInfoFont.dispose();
		}
		if (searchTextFont != null) {
			searchTextFont.dispose();
		}
	}

	/**
	 * Create contents of the window.
	 */
	protected void createContents() {
		Display display = null;
		int shellStyle = SWT.BORDER | SWT.CLOSE | SWT.MAX | SWT.RESIZE | SWT.TITLE;
		
		if (isPlugin) {
			// if opened from ADT, open with APPLICATION_MODAL to prevent conflicting code changes
			shellStyle |= SWT.APPLICATION_MODAL;
		} else {
			shellStyle |= SWT.MIN;
		}

		shell = new Shell(display, shellStyle);
		
		shell.setMinimumSize(new Point(880, 600));
		shell.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				codeDisplay.codeDisplayKeyPressed(e); // calls this.keyPressedInCodeDisplay() if the key could not be processed
			}
		});
		shell.addShellListener(new ShellAdapter() {
			@Override
			public void shellActivated(ShellEvent e) {
				// refresh the profile list, because new profiles could have been added after menu "Help / Open Profiles Folder" was used; 
				// however, if the current profile name is still in the list, suppress reloading the profile and reprocessing the code
				String profileName = (curProfile == null) ? Profile.DEFAULT_NAME : curProfile.name;
				boolean suppressReprocessingIfFound = (curProfile != null);
				refreshProfileList(profileName, suppressReprocessingIfFound);

				codeDisplay.formActivated();
			}

			@Override
			public void shellClosed(ShellEvent e) {
				if (isPlugin)
					cancelAndClose();
				else
					applyAndClose();
			}
		});
		shell.setImage(SWTResourceManager.getImage(FrmMain.class, "/ShellImage.png")); 
		shell.setSize(1008, 729);
		shell.setMaximized(true);
		shell.setText(Program.PRODUCT_NAME);
		shell.setLayout(new GridLayout(2, false));

		Menu mnsMain = new Menu(shell, SWT.BAR);
		shell.setMenuBar(mnsMain);

		MenuItem mmuCode = new MenuItem(mnsMain, SWT.CASCADE);
		mmuCode.setText("&Code");
		
		Menu menuCode = new Menu(mmuCode);
		mmuCode.setMenu(menuCode);

		mmuCodeFromClipboard = new MenuItem(menuCode, SWT.NONE);
		mmuCodeFromClipboard.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				codeFromClipboard(true);
				codeDisplay.focusDisplay();
			}
		});
		mmuCodeFromClipboard.setText("From &Clipboard");

		mmuCodeFromFile = new MenuItem(menuCode, SWT.NONE);
		mmuCodeFromFile.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				codeFromFile();
			}
		});
		mmuCodeFromFile.setText("From &File...");

		mmuCodeWatchClipboard = new MenuItem(menuCode, SWT.CHECK);
		mmuCodeWatchClipboard.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				lblWatchClipboardInfo.setVisible(mmuCodeWatchClipboard.getSelection());
				watchClipboard();
			}
		});
		mmuCodeWatchClipboard.setText("&Watch and Modify Clipboard");

		mmuCodeClearDisplay = new MenuItem(menuCode, SWT.NONE);
		mmuCodeClearDisplay.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				clearDisplay();
			}
		});
		mmuCodeClearDisplay.setText("Clear &Display");

		mmuCodeSeparator = new MenuItem(menuCode, SWT.SEPARATOR);
		
		MenuItem mmuCodeToClipboard = new MenuItem(menuCode, SWT.NONE);
		mmuCodeToClipboard.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				codeToClipboard(1);
			}
		});
		mmuCodeToClipboard.setText("Whole &Result to Clipboard");

		MenuItem mmuCodeSelectionLeftToClip = new MenuItem(menuCode, SWT.NONE);
		mmuCodeSelectionLeftToClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				codeDisplay.copySelectionToClipboard(DisplaySide.LEFT);
			}
		});
		mmuCodeSelectionLeftToClip.setText(getMenuItemTextWithAccelerator("Selection (&Left) to Clipboard", SWT.SHIFT + SWT.CTRL + 'C'));

		MenuItem mmuCodeSelectionToClip = new MenuItem(menuCode, SWT.NONE);
		mmuCodeSelectionToClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				codeDisplay.copySelectionToClipboard();
			}
		});
		mmuCodeSelectionToClip.setText(getMenuItemTextWithAccelerator("&Selection (Right) to Clipoard", SWT.CTRL + 'C'));

		new MenuItem(menuCode, SWT.SEPARATOR);

		mmuCodeApplyAndClose = new MenuItem(menuCode, SWT.NONE);
		mmuCodeApplyAndClose.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				applyAndClose();
			}
		});
		mmuCodeApplyAndClose.setText(getMenuItemTextWithAccelerator("&Apply and Close", SWT.CTRL + '\r'));

		mmuCodeCancel = new MenuItem(menuCode, SWT.NONE);
		mmuCodeCancel.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				cancelAndClose();
			}
		});
		mmuCodeCancel.setText(getMenuItemTextWithAccelerator("&Cancel", SWT.ESC));

		mmuCodeExit = new MenuItem(menuCode, SWT.NONE);
		mmuCodeExit.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				applyAndClose();
			}
		});
		mmuCodeExit.setText("E&xit");

		MenuItem mmuView = new MenuItem(mnsMain, SWT.CASCADE);
		mmuView.setText("&View");

		Menu menuView = new Menu(mmuView);
		mmuView.setMenu(menuView);

		MenuItem mmuViewZoomIn = new MenuItem(menuView, SWT.NONE);
		mmuViewZoomIn.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				codeDisplay.zoomIn();
			}
		});
		mmuViewZoomIn.setText(getMenuItemTextWithAccelerator("Zoom In", SWT.CTRL + '+'));

		MenuItem mmuViewZoomDefault = new MenuItem(menuView, SWT.NONE);
		mmuViewZoomDefault.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				codeDisplay.zoomDefault();
			}
		});
		mmuViewZoomDefault.setText("Zoom &Default");

		MenuItem mmuViewZoomOut = new MenuItem(menuView, SWT.NONE);
		mmuViewZoomOut.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				codeDisplay.zoomOut();
			}
		});
		mmuViewZoomOut.setText(getMenuItemTextWithAccelerator("Zoom Out", SWT.CTRL + '-'));

		new MenuItem(menuView, SWT.SEPARATOR);

		mmuHighlightDeclarationKeywords = new MenuItem(menuView, SWT.CHECK);
		mmuHighlightDeclarationKeywords.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean highlight = mmuHighlightDeclarationKeywords.getSelection();
				if (settings != null)
					settings.highlightDeclarationKeywords = highlight; 
				if (codeDisplay != null && !codeDisplay.isDisposed())
					codeDisplay.setHighlightDeclarationKeywords(highlight);
			}
		});
		mmuHighlightDeclarationKeywords.setText("Highlight Declarations");

		mmuHighlightWritePositions = new MenuItem(menuView, SWT.CHECK);
		mmuHighlightWritePositions.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean highlight = mmuHighlightWritePositions.getSelection();
				if (settings != null)
					settings.highlightWritePositions = highlight; 
				if (codeDisplay != null && !codeDisplay.isDisposed())
					codeDisplay.setHighlightWritePositions(highlight);
			}
		});
		mmuHighlightWritePositions.setText("Highlight &Write Positions");

		mmuExtras = new MenuItem(mnsMain, SWT.CASCADE);
		mmuExtras.setText("E&xtras");

		Menu menuExtras = new Menu(mmuExtras);
		mmuExtras.setMenu(menuExtras);

		MenuItem mmuExtrasGenerateUnitTest = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasGenerateUnitTest.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					codeDisplay.generateUnitTest(true, true);
				} catch (Exception ex) {
				}
			}
		});
		mmuExtrasGenerateUnitTest.setText(getMenuItemTextWithAccelerator("Generate &Unit Test from Code Selection", SWT.F10));
		
		MenuItem mmuExtrasGenerateUnitTestBuild = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasGenerateUnitTestBuild.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					codeDisplay.generateUnitTest(false, true);
				} catch (Exception ex) {
				}
			}
		});
		mmuExtrasGenerateUnitTestBuild.setText(getMenuItemTextWithAccelerator("Generate Unit Test (&Build Commands Only)", SWT.F11));
		
		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasImportTestCode = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasImportTestCode.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					FrmImportTestCode frmImportTestCode = new FrmImportTestCode();
					frmImportTestCode.open(defaultCodeDirectory, "");
				} catch (Exception ex) {
				}
			}
		});
		mmuExtrasImportTestCode.setText("Import Test Code from Clipboard...");
		
		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasTestParserOnFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasTestParserOnFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				testDirectory(new CleanupBatchJob(CleanupParams.createForParseOnly()));
			}
		});
		mmuExtrasTestParserOnFolder.setText("Test Parser on All Files in &Folder...");

		MenuItem mmuExtrasTestActiveRulesOnFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasTestActiveRulesOnFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				testDirectory(new CleanupBatchJob(CleanupParams.createForProfile(curProfile, false, ABAP.NO_RELEASE_RESTRICTION)));
			}
		});
		mmuExtrasTestActiveRulesOnFolder .setText("Test Active Rules on All Files in &Folder...");

		MenuItem mmuExtrasTestAllRulesOnFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasTestAllRulesOnFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				testDirectory(new CleanupBatchJob(CleanupParams.createForProfile(curProfile, true, ABAP.NO_RELEASE_RESTRICTION)));
			}
		});
		mmuExtrasTestAllRulesOnFolder.setText("Test All Rules on All Files in &Folder...");

		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasKeywordFreqAbapFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasKeywordFreqAbapFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				testDirectory(new AbapKeywordFreqBatchJob());
			}
		});
		mmuExtrasKeywordFreqAbapFolder.setText("Get ABAP Keyword Frequencies for Folder...");

		MenuItem mmuExtrasSepFreqAbapFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasSepFreqAbapFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				getSepFreqForFolder(CommentIdentifierMode.ABAP_CODE_ONLY);
			}
		});
		mmuExtrasSepFreqAbapFolder.setText("Get ABAP Separator Frequencies for Folder...");

		MenuItem mmuExtrasSepFreqEnglishFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasSepFreqEnglishFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				getSepFreqForFolder(CommentIdentifierMode.QUOT_MARK_COMMENTS_ONLY);
			}
		});
		mmuExtrasSepFreqEnglishFolder.setText("Get English Text Separator Frequencies for Folder...");

		MenuItem mmuExtrasWordFreqEnglishFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasWordFreqEnglishFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				getWordFreqForFolder(CommentIdentifierMode.ALL_COMMENT_LINES);
			}
		});
		mmuExtrasWordFreqEnglishFolder.setText("Get Comment Word Frequencies for Folder...");

		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasTestCommentIdentifier = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasTestCommentIdentifier.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				identifyComments(CommentIdentifierMode.ABAP_CODE_ONLY);
			}
		});
		mmuExtrasTestCommentIdentifier.setText("Test &Comment Identifier with ABAP Lines");

		MenuItem mmuExtrasTestCommentLines = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasTestCommentLines.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				identifyComments(CommentIdentifierMode.QUOT_MARK_COMMENTS_ONLY);
			}
		});
		mmuExtrasTestCommentLines.setText("Test Comment Identifier with \\\" Comment Lines");

		MenuItem mmuExtrasTestAsteriskCommentLines = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasTestAsteriskCommentLines.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				identifyComments(CommentIdentifierMode.ASTERISK_COMMENTS);
			}
		});
		mmuExtrasTestAsteriskCommentLines.setText("Test Comment Identifier with * Comment Lines");

		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasNextMatchingSample = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasNextMatchingSample.setToolTipText("Opens the next code file from the /user/code folder (and subfolders) in which code is changed with the current profile settings.");
		mmuExtrasNextMatchingSample.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				openNextMatchingSampleFile(true);
			}
		});
		mmuExtrasNextMatchingSample.setText(getMenuItemTextWithAccelerator("Open Next Match", SWT.SHIFT + SWT.CTRL + SWT.ARROW_RIGHT));

		MenuItem mmuExtrasPrevMatchingSample = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasPrevMatchingSample.setToolTipText("Opens the previous code file from the /user/code folder (and subfolders) in which code is changed with the current profile settings.");
		mmuExtrasPrevMatchingSample.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				openNextMatchingSampleFile(false);
			}
		});
		mmuExtrasPrevMatchingSample.setText(getMenuItemTextWithAccelerator("Open Previous Match", SWT.SHIFT + SWT.CTRL + SWT.ARROW_DOWN));

		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasNextPatternMatch = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasNextPatternMatch.setToolTipText("Opens the next code file from the /user/code folder (and subfolders) in which a Command satisfies the hard-coded Command.patternMatch().");
		mmuExtrasNextPatternMatch.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				openNextPatternMatchFile(true);
			}
		});
		mmuExtrasNextPatternMatch.setText(getMenuItemTextWithAccelerator("Open Next Pattern Match", SWT.CTRL + SWT.F6));

		MenuItem mmuExtrasPrevPatternMatch = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasPrevPatternMatch.setToolTipText("Opens the previous code file from the /user/code folder (and subfolders) in which a Command satisfies the hard-coded Command.patternMatch().");
		mmuExtrasPrevPatternMatch.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				openNextPatternMatchFile(false);
			}
		});
		mmuExtrasPrevPatternMatch.setText(getMenuItemTextWithAccelerator("Open Previous Pattern Match", SWT.CTRL + SWT.F5));

		MenuItem mmuExtrasPatternMatchesBeforeCleanupToClip = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasPatternMatchesBeforeCleanupToClip.setToolTipText("Fills the clipboard with all commands from the code files in the /user/code folder (and subfolders) that satisfy the hard-coded Command.patternMatch() before cleanup.");
		mmuExtrasPatternMatchesBeforeCleanupToClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				patternMatchesToClip(false);
			}
		});
		mmuExtrasPatternMatchesBeforeCleanupToClip.setText("Copy Pattern Matches Before Cleanup to Clipboard (all Files)");

		MenuItem mmuExtrasPatternMatchesAfterCleanupToClip = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasPatternMatchesAfterCleanupToClip.setToolTipText("Fills the clipboard with all commands from the code files in the /user/code folder (and subfolders) that satisfy the hard-coded Command.patternMatch() after cleanup with the current profile.");
		mmuExtrasPatternMatchesAfterCleanupToClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				patternMatchesToClip(true);
			}
		});
		mmuExtrasPatternMatchesAfterCleanupToClip.setText("Copy Pattern Matches After Cleanup to Clipboard (all Files)");

		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasCreateRulesDocumentation = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasCreateRulesDocumentation.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
				   createRulesDocumentation();
				} catch (Exception ex) {
				}
			}
		});
		mmuExtrasCreateRulesDocumentation.setText("Create Markdown Documentation for Rules");
		
		MenuItem mmuHelp = new MenuItem(mnsMain, SWT.CASCADE);
		mmuHelp.setText("&Help");

		Menu menuHelp = new Menu(mmuHelp);
		mmuHelp.setMenu(menuHelp);

		MenuItem mmuHelpDocumentation = new MenuItem(menuHelp, SWT.NONE);
		mmuHelpDocumentation.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ProgramLauncher.showHelp(HelpTopic.README);
			}
		});
		mmuHelpDocumentation.setText("&Documentation");

		MenuItem mmuHelpShortcuts = new MenuItem(menuHelp, SWT.NONE);
		mmuHelpShortcuts.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ProgramLauncher.showHelp(HelpTopic.MAIN);
			}
		});
		mmuHelpShortcuts.setText("Show &Navigation Shortcuts");

		MenuItem mmuHelpReleaseNotes = new MenuItem(menuHelp, SWT.NONE);
		mmuHelpReleaseNotes.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ProgramLauncher.showHelp(HelpTopic.RELEASE_NOTES);
			}
		});
		mmuHelpReleaseNotes.setText("Show &Release Notes");

		new MenuItem(menuHelp, SWT.SEPARATOR);

		MenuItem mmuHelpOpenErrorLog = new MenuItem(menuHelp, SWT.NONE);
		mmuHelpOpenErrorLog.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				String path = Program.getLog().path;
				if (Persistency.get().fileExists(path))
					ProgramLauncher.startProcess(path);
				else
					Message.show("The error log (" + path + ") is empty.");
			}
		});
		mmuHelpOpenErrorLog.setText("Open &Error Log");

		MenuItem mmuHelpOpenProfilesFolder = new MenuItem(menuHelp, SWT.NONE);
		mmuHelpOpenProfilesFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				openProfilesFolder();
			}
		});
		mmuHelpOpenProfilesFolder.setText("Open &Profiles Folder");

		MenuItem mmuHelpAbout = new MenuItem(menuHelp, SWT.NONE);
		mmuHelpAbout.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				helpAbout();
			}
		});
		mmuHelpAbout.setText("&About");

		codeDisplay = new CodeDisplay(shell, SWT.NONE);
		codeDisplay.setColors(codeDisplayColors);
		codeDisplay.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
		codeDisplay.setCodeFontSize(CodeDisplay.DEFAULT_FONT_SIZE);

		pnlRules = new Composite(shell, SWT.NONE);
		pnlRules.setLayoutData(new GridData(SWT.LEFT, SWT.FILL, false, true, 1, 1));
		GridLayout gl_pnlRules = new GridLayout(1, false);
		gl_pnlRules.marginTop = 5;
		gl_pnlRules.marginLeft = 5;
		gl_pnlRules.marginBottom = 5;
		gl_pnlRules.marginRight = 5;
		pnlRules.setLayout(gl_pnlRules);
		
		Composite cpsCleanupSettingsTitle = new Composite(pnlRules, SWT.NONE);
		GridLayout gl_cpsCleanupSettingsTitle = new GridLayout(2, false);
		gl_cpsCleanupSettingsTitle.marginWidth = 0;
		gl_cpsCleanupSettingsTitle.marginHeight = 0;
		cpsCleanupSettingsTitle.setLayout(gl_cpsCleanupSettingsTitle);
		cpsCleanupSettingsTitle.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		
		Label lblCleanupSettings = new Label(cpsCleanupSettingsTitle, SWT.NONE);
		lblCleanupSettings.setSize(37, 15);
		lblCleanupSettings.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		lblCleanupSettings.setText("Cleanup Settings");
		
		lblWatchClipboardInfo = new Label(cpsCleanupSettingsTitle, SWT.NONE);
		lblWatchClipboardInfo.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		lblWatchClipboardInfo.setForeground(SWTResourceManager.getColor(SWT.COLOR_RED));
		lblWatchClipboardInfo.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true, false, 1, 1));
		lblWatchClipboardInfo.setText("WATCHING AND MODIFYING CLIPBOARD");

		Composite cpsCleanupSettings = new Composite(pnlRules, SWT.NONE);
		GridLayout gl_cpsCleanupSettings = new GridLayout(3, false);
		gl_cpsCleanupSettings.marginWidth = 0;
		gl_cpsCleanupSettings.marginTop = 0;
		gl_cpsCleanupSettings.marginBottom = 5;
		gl_cpsCleanupSettings.horizontalSpacing = 10;
		cpsCleanupSettings.setLayout(gl_cpsCleanupSettings);
		cpsCleanupSettings.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));

		Label lblProfile = new Label(cpsCleanupSettings, SWT.NONE);
		lblProfile.setText("Profile:");
		
		cboProfile = new Combo(cpsCleanupSettings, SWT.READ_ONLY);
		cboProfile.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		cboProfile.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				profileChanged();
				codeDisplay.focusDisplay();
			}
		});

		Button btnEditProfiles = new Button(cpsCleanupSettings, SWT.NONE);
		btnEditProfiles.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnEditProfiles.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				editProfiles();
				codeDisplay.focusDisplay();
			}
		});
		btnEditProfiles.setText("Con&figure...");
		
		lblCleanupRangeExpandMode = new Label(cpsCleanupSettings, SWT.NONE);
		lblCleanupRangeExpandMode.setText("Default cleanup range:");
		
		cboCleanupRangeExpandMode = new Combo(cpsCleanupSettings, SWT.READ_ONLY);
		cboCleanupRangeExpandMode.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		for (CleanupRangeExpandMode expandMode : CleanupRangeExpandMode.values()) {
			cboCleanupRangeExpandMode.add(expandMode.displayText);
		}
		cboCleanupRangeExpandMode.setToolTipText("This cleanup range is used if the cursor is in the editor, but no code is selected.");
		cboCleanupRangeExpandMode.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				int selIndex = cboCleanupRangeExpandMode.getSelectionIndex();
				if (selIndex >= 0) {
					cleanupRangeExpandModeChanged(CleanupRangeExpandMode.forValue(selIndex));
					codeDisplay.focusDisplay();
				}
			}
		});
		
		lblCleanupRangeExpandModeEmptyCell = new Label(cpsCleanupSettings, SWT.NONE);
		
		Label lblReleaseRestriction = new Label(cpsCleanupSettings, SWT.NONE);
		lblReleaseRestriction.setText("Restrict rules to syntax of:");
		
		cboReleaseRestriction = new Combo(cpsCleanupSettings, SWT.READ_ONLY);
		cboReleaseRestriction.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		for (String releaseRestrictionName : ABAP.RELEASE_RESTRICTION_NAMES)
			cboReleaseRestriction.add(getReleaseRestrictionDisplay(releaseRestrictionName));
		cboReleaseRestriction.add(getReleaseRestrictionDisplay(ABAP.NO_RELEASE_RESTRICTION_NAME));
		cboReleaseRestriction.setToolTipText("Prevents cleanup rules from introducing newer syntax that is unknown to the specified ABAP release.");
		cboReleaseRestriction.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (cboReleaseRestriction.getSelectionIndex() >= 0) {
					releaseRestrictionChanged(getReleaseRestrictionName(cboReleaseRestriction.getText())); 
				}
				codeDisplay.focusDisplay();
			}
		});

		new Label(cpsCleanupSettings, SWT.NONE);

		Label lblDisplay = new Label(pnlRules, SWT.NONE);
		lblDisplay.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		GridData gd_lblDisplay = new GridData(SWT.LEFT, SWT.TOP, false, false, 1, 1);
		gd_lblDisplay.verticalIndent = 5;
		lblDisplay.setLayoutData(gd_lblDisplay);
		lblDisplay.setText("Display");

		Composite cpsDisplay = new Composite(pnlRules, SWT.NONE);
		cpsDisplay.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		GridLayout gl_cpsDisplay = new GridLayout(2, false);
		gl_cpsDisplay.marginWidth = 0;
		cpsDisplay.setLayout(gl_cpsDisplay);

		Composite cpsHighlight = new Composite(cpsDisplay, SWT.NONE);
		cpsHighlight.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		GridLayout gl_cpsHighlight = new GridLayout(1, false);
		gl_cpsHighlight.marginWidth = 0;
		gl_cpsHighlight.horizontalSpacing = 0;
		cpsHighlight.setLayout(gl_cpsHighlight);

		chkHighlightIndentChanges = new Button(cpsHighlight, SWT.CHECK);
		chkHighlightIndentChanges.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		chkHighlightIndentChanges.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				refreshHighlight();
			}
		});
		chkHighlightIndentChanges.setText("Highlight indent changes (00.000 lines)");
		chkHighlightIndentChanges.setSelection(true);

		chkHighlightInnerSpaceChanges = new Button(cpsHighlight, SWT.CHECK);
		chkHighlightInnerSpaceChanges.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		chkHighlightInnerSpaceChanges.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				refreshHighlight();
			}
		});
		chkHighlightInnerSpaceChanges.setText("Highlight inner space changes (00.000 lines)");
		chkHighlightInnerSpaceChanges.setSelection(true);

		chkHighlightCaseChanges = new Button(cpsHighlight, SWT.CHECK);
		chkHighlightCaseChanges.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		chkHighlightCaseChanges.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				refreshHighlight();
			}
		});
		chkHighlightCaseChanges.setText("Highlight upper/lower case changes (00.000 lines)");
		chkHighlightCaseChanges.setSelection(true);

		chkHighlightContentChanges = new Button(cpsHighlight, SWT.CHECK);
		chkHighlightContentChanges.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		chkHighlightContentChanges.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				refreshHighlight();
			}
		});
		chkHighlightContentChanges.setText("Highlight text and line changes (00.000 lines)");
		chkHighlightContentChanges.setSelection(true);

		Composite cpsReprocess = new Composite(cpsDisplay, SWT.NONE);
		cpsReprocess.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false, 1, 1));
		GridLayout gl_cpsReprocess = new GridLayout(1, false);
		gl_cpsReprocess.marginLeft = 5;
		gl_cpsReprocess.marginWidth = 0;
		cpsReprocess.setLayout(gl_cpsReprocess);

		Composite cpsSearch = new Composite(pnlRules, SWT.NONE);
		cpsSearch.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		GridLayout gl_cpsSearch = new GridLayout(2, false);
		gl_cpsSearch.marginWidth = 0;
		cpsSearch.setLayout(gl_cpsSearch);

		Composite cpsSearch1 = new Composite(cpsSearch, SWT.NONE);
		cpsSearch1.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 1, 1));
		GridLayout gl_cpsSearch1 = new GridLayout(1, false);
		gl_cpsSearch1.marginWidth = 0;
		gl_cpsSearch1.horizontalSpacing = 0;
		cpsSearch1.setLayout(gl_cpsSearch1);

		Label lblSearchTitle = new Label(cpsSearch1, SWT.NONE);
		lblSearchTitle.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		lblSearchTitle.setText("Incremental Search");

		chkSearchLeftDisplay = new Button(cpsSearch1, SWT.CHECK);
		GridData gd_chkSearchLeftDisplay = new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1);
		gd_chkSearchLeftDisplay.verticalIndent = 5;
		chkSearchLeftDisplay.setLayoutData(gd_chkSearchLeftDisplay);
		chkSearchLeftDisplay.setText("Search in left display");

		chkMatchCase = new Button(cpsSearch1, SWT.CHECK);
		chkMatchCase.setText("Match case");

		chkMatchWholeWord = new Button(cpsSearch1, SWT.CHECK);
		chkMatchWholeWord.setText("Match whole word");

		Composite cpsSearch2 = new Composite(cpsSearch, SWT.NONE);
		cpsSearch2.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		cpsSearch2.setLayout(new GridLayout(1, false));

		lblSearch = new Label(cpsSearch2, SWT.NONE);
		lblSearch.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		lblSearch.setText("(press Ctrl + F and type search string)");

		chkSearchRightDisplay = new Button(cpsSearch2, SWT.CHECK);
		GridData gd_chkSearchRightDisplay = new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1);
		gd_chkSearchRightDisplay.verticalIndent = 5;
		chkSearchRightDisplay.setLayoutData(gd_chkSearchRightDisplay);
		chkSearchRightDisplay.setText("Search in right display");
		chkSearchRightDisplay.setSelection(true);

		chkSearchChangedLines = new Button(cpsSearch2, SWT.CHECK);
		chkSearchChangedLines.setText("Search in changed lines only");
		
		Composite cpsUsedRulesTitle = new Composite(pnlRules, SWT.NONE);
		cpsUsedRulesTitle.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		GridLayout gl_cpsUsedRulesTitle = new GridLayout(2, false);
		gl_cpsUsedRulesTitle.marginRight = 9;
		gl_cpsUsedRulesTitle.marginWidth = 0;
		cpsUsedRulesTitle.setLayout(gl_cpsUsedRulesTitle);
				
		Label lblUsedRulesInfo = new Label(cpsUsedRulesTitle, SWT.NONE);
		lblUsedRulesInfo.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		lblUsedRulesInfo.setText("Rules Used in Current Selection");
		
		Label lblUsedRulesHint = new Label(cpsUsedRulesTitle, SWT.NONE);
		lblUsedRulesHint.setText("(uncheck to block rule locally)");

		CheckboxTableViewer checkboxTableViewer = CheckboxTableViewer.newCheckList(pnlRules, SWT.BORDER | SWT.FULL_SELECTION);
		lstUsedRules = checkboxTableViewer.getTable();
		lstUsedRules.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (suspendItemCheck == 0) {
					int itemIndex = findUsedRulesItem(e);
					if (itemIndex >= 0 && usedRules != null && usedRules.length > itemIndex) {
						boolean checked = lstUsedRules.getItem(itemIndex).getChecked();
						setBlockRule(itemIndex, !checked);
					}
				}
			}
		});
		lstUsedRules.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, true, 1, 1));
		
		cpsApplyOrCancel = new Composite(pnlRules, SWT.NONE);
		GridLayout gl_cpsApplyOrCancel = new GridLayout(2, false);
		gl_cpsApplyOrCancel.marginWidth = 0;
		cpsApplyOrCancel.setLayout(gl_cpsApplyOrCancel);
		cpsApplyOrCancel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		
		btnApplyAndClose = new Button(cpsApplyOrCancel, SWT.NONE);
		btnApplyAndClose.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		btnApplyAndClose.setText("Apply and Close");
		btnApplyAndClose.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				applyAndClose();
			}
		});
		
		btnCancel = new Button(cpsApplyOrCancel, SWT.NONE);
		btnCancel.setText("Cancel");
		btnCancel.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				cancelAndClose();
			}
		});
	}

	private String getMenuItemTextWithAccelerator(String text, int acceleratorKeyCode) {
		return text + '\t' + Action.convertAccelerator(acceleratorKeyCode);
	}

	private int findUsedRulesItem(SelectionEvent e) {
		for (int i = 0; i < lstUsedRules.getItemCount(); ++i) {
			if (lstUsedRules.getItem(i) == e.item) 
				return i;
		}
		return -1;
	}

	private boolean codeFromClipboard(boolean showMessages) {
		if (isPlugin)
			return false;
		
		if (!SystemClipboard.containsText()) {
			Message.show("The clipboard is empty!");
			return false;
		}
		String code = SystemClipboard.getText();
		if (Program.showDevFeatures()) {
			// simplify pasting code from unit tests
			if ((code.contains("\t\tbuildSrc(\"") || code.contains("\t\tbuildExp(\"")) && code.contains("\");")) {
				code = code.replace("\t\tbuildSrc(\"", "").replace("\t\tbuildExp(\"", "").replace("\");", "");
				code = StringUtil.getUnescapedText(code);
			}
		}
		String fileName = ABAP.inferFileNameFromCode(code);
		String sourceName = StringUtil.isNullOrEmpty(fileName) ? "Clipboard" : fileName;
		return refreshCode(sourceName, "", code, ABAP.NEWEST_RELEASE, showMessages);
	}

	private void codeFromFile() {
		FileDialog dialog = new FileDialog(shell, SWT.OPEN);
		dialog.setText("Open Code from File");
		dialog.setFilterPath(defaultCodeDirectory);
		String path = dialog.open();
		if (path == null || path.length() == 0)
			return;
		refreshFromFile(path);
	}

	private void clearDisplay() {
		refreshCode("", "", "", ABAP.NEWEST_RELEASE, true);
	}
	
	private void codeToClipboard(int addLfCount) {
		final String LF = "\n";
		String codeText = codeDisplay.getCodeToString();

		if (codeText != null && addLfCount > 0) {
			StringBuilder sb = new StringBuilder(codeText.length() + addLfCount * LF.length());
			sb.append(codeText);
			for (int i = 0; i < addLfCount; ++i)
				sb.append(LF);
			codeText = sb.toString();
		}
		
		if (!StringUtil.isNullOrEmpty(codeText)) 
			SystemClipboard.setText(codeText);
	}

	private void saveSettings() {
		settings.curProfileName = (curProfile != null && curProfile.name != null) ? curProfile.name : "";
		// settings.cleanupRangeExpandMode is directly updated in cleanupRangeExpandModeChange()

		settings.highlightIndentChanges = chkHighlightIndentChanges.getSelection();
		settings.highlightInnerSpaceChanges = chkHighlightInnerSpaceChanges.getSelection();
		settings.highlightCaseChanges = chkHighlightCaseChanges.getSelection();
		settings.highlightContentChanges = chkHighlightContentChanges.getSelection();

		settings.searchLeftDisplay = chkSearchLeftDisplay.getSelection();
		settings.searchRightDisplay = chkSearchRightDisplay.getSelection();
		settings.searchChangedLines = chkSearchChangedLines.getSelection();
		settings.matchCase = chkMatchCase.getSelection();
		settings.matchWholeWord = chkMatchWholeWord.getSelection();

		settings.codeFontSize = codeDisplay.getCodeFontSize();
		settings.showVerticalLine = codeDisplay.getShowVerticalLine();
		settings.verticalLinePos = codeDisplay.getVerticalLinePos();

		try {
			settings.shellMaximized = shell.getMaximized();
			org.eclipse.swt.graphics.Rectangle shellBounds = shell.getBounds();
			settings.shellLeft = shellBounds.x;
			settings.shellTop = shellBounds.y;
			settings.shellWidth = shellBounds.width;
			settings.shellHeight = shellBounds.height;
		} catch (SWTException ex) { // ERROR_WIDGET_DISPOSED, ERROR_THREAD_INVALID_ACCESS
			settings.setShellBoundsUnspecified();
		}
		
		// settings.releaseRestriction is directly updated in releaseRestrictionChange()
		// settings.editProfiles... is directly updated within FrmProfiles
				
		settings.save();
	}

	private void loadSettings() {
		settings.load();
		
		chkHighlightIndentChanges.setSelection(settings.highlightIndentChanges);
		chkHighlightInnerSpaceChanges.setSelection(settings.highlightInnerSpaceChanges);
		chkHighlightCaseChanges.setSelection(settings.highlightCaseChanges);
		chkHighlightContentChanges.setSelection(settings.highlightContentChanges);

		chkSearchLeftDisplay.setSelection(settings.searchLeftDisplay);
		chkSearchRightDisplay.setSelection(settings.searchRightDisplay);
		chkSearchChangedLines.setSelection(settings.searchChangedLines);
		chkMatchCase.setSelection(settings.matchCase);
		chkMatchWholeWord.setSelection(settings.matchWholeWord);

		codeDisplay.setCodeFontSize(settings.codeFontSize);
		codeDisplay.setVerticalLine(settings.showVerticalLine, settings.verticalLinePos);
		codeDisplay.setHighlightDeclarationKeywords(settings.highlightDeclarationKeywords);
		codeDisplay.setHighlightWritePositions(settings.highlightWritePositions);

		mmuHighlightDeclarationKeywords.setSelection(settings.highlightDeclarationKeywords);
		mmuHighlightWritePositions.setSelection(settings.highlightWritePositions);

		// restore shell bounds 
		try {
			shell.setMaximized(settings.shellMaximized);
			if (!settings.shellMaximized && settings.areShellBoundsSpecified()) {
				org.eclipse.swt.graphics.Rectangle displayBounds = shell.getDisplay().getBounds();
				if (displayBounds.contains(settings.shellLeft, settings.shellTop) && displayBounds.contains(settings.getShellRight() - 1, settings.getShellBottom() - 1)) {
					shell.setBounds(settings.shellLeft, settings.shellTop, settings.shellWidth, settings.shellHeight);
				}
			}
		} catch (SWTException ex) { // ERROR_WIDGET_DISPOSED, ERROR_THREAD_INVALID_ACCESS
		}
		
		if (!cboCleanupRangeExpandMode.isDisposed()) {
			cboCleanupRangeExpandMode.select(settings.cleanupRangeExpandMode.getValue());
		}
		
		String releaseRestrictionName = ABAP.getReleaseRestrictionName(settings.releaseRestriction);
		if (!cboReleaseRestriction.isDisposed()) {
			int itemIndex = cboReleaseRestriction.indexOf(getReleaseRestrictionDisplay(releaseRestrictionName));
			if (itemIndex >= 0) {
	      	cboReleaseRestriction.select(itemIndex);
			}
		}

		// create a profile in which only the 'essential' rules are activated, i.e. those rules that are explicitly  
		// demanded by the Clean ABAP style guide; this is done only once to avoid re-creating the profile again and again  
		if (!settings.wasEssentialProfileCreated) {
			if (Profile.addAndSaveEssentialProfile(settings.profilesDirectory)) {
            settings.wasEssentialProfileCreated = true;
            settings.save();
			}
		}
		
		refreshProfileList(StringUtil.isNullOrEmpty(settings.curProfileName) ? Profile.DEFAULT_NAME : settings.curProfileName, false);
	}

	private boolean refreshCode() {
		// keep source name, path, code and abapRelease, and also try to keep the position
		return refreshCode(null, null, null, null, true, codeDisplay.getTopLineIndex(), codeDisplay.getCurLineIndex(), codeDisplay.getSelectionStartLine(), originalCleanupRange, settings.cleanupRangeExpandMode);
	}
	
	private boolean refreshCode(String newSourceName, String newSourcePath, String newCodeText, String newAbapRelease, boolean showMessages) {
		return refreshCode(newSourceName, newSourcePath, newCodeText, newAbapRelease, showMessages, 0, 0, 0, null, CleanupRangeExpandMode.FULL_DOCUMENT);
	}

	private boolean refreshCode(String newSourceName, String newSourcePath, String newCodeText, String newAbapRelease, boolean showMessages, int topLineIndex, int curLineIndex, int selectionStartLine, CleanupRange cleanupRange, CleanupRangeExpandMode cleanupRangeExpandMode) {
		String sourceName = (newSourceName != null) ? newSourceName : codeDisplay.getSourceName();
		String sourcePath = (newSourcePath != null) ? newSourcePath : codeDisplay.getSourcePath();
		String sourceCode = (newCodeText != null) ? newCodeText : codeDisplay.getSourceCode();
		String abapRelease = (newAbapRelease != null) ? newAbapRelease : codeDisplay.getAbapRelease();
		
		BackgroundJob job = new BackgroundJob(ParseParams.createForCleanupRange(sourceName, sourceCode, abapRelease, cleanupRange, cleanupRangeExpandMode), 
														  CleanupParams.createForProfile(curProfile, false, settings.releaseRestriction));
		Task result = runJobWithProgressUiIfNeeded(job);

		resultCode = null;
		resultErrorMessage = null;

		if (!result.getSuccess()) {
			if (!job.wasCancelled()) { 
				if (showMessages) // TODO: otherwise, display it on a Label? (for 'Watch and Modify Clipboard' function)
					Message.show(result.getErrorMessage());
				else
					resultErrorMessage = result.getErrorMessage();
			}
			return false;
		}

		// if only a certain range of lines shall be cleaned up and the code is first shown, scroll to the beginning of that range
		if (topLineIndex < 0) {
			DiffDoc diffDoc = result.getResultingDiffDoc(); 
			curLineIndex = (diffDoc == null) ? 0 : diffDoc.getFirstLineInCleanupRange(); 
			selectionStartLine = curLineIndex;
			topLineIndex = Math.max(0, curLineIndex - Math.max(codeDisplay.getVisibleLineCount() / 4, 4));
		}
		
		// show result
		String abapReleaseInfo = getReleaseInfo(newAbapRelease);
		shell.setText(Program.PRODUCT_NAME + " - " + sourceName + abapReleaseInfo);
		codeDisplay.setInfo(sourceName, sourcePath, sourceCode, abapRelease, curProfile.getSingleActiveRule());
		codeDisplay.refreshCode(result.getResultingCode(), result.getResultingDiffDoc(), topLineIndex, curLineIndex, selectionStartLine);
		if (Program.showDevFeatures()) {
			shell.setText(Program.PRODUCT_NAME + " - " + sourceName + abapReleaseInfo + " - " + result.getCalculationTimeInfo());
		}
		// remember the resulting Code instance; the CleanupResult will only be created from it when the window is closed 
		resultCode = result.getResultingCode(); 

		if (result.getLogSummary() != null) { // even with result.getSuccess() == true, there may be warnings in the log
			if (showMessages) { // TODO: otherwise, display it on a Label? (for 'Watch and Modify Clipboard' function)
				Message.show(result.getLogSummary());
			}
		}
		return true;
	}

	private String getReleaseInfo(String abapRelease) {
		if (StringUtil.isNullOrEmpty(abapRelease)) 
			return "";
		
		// convert "757" into "7.57"
		String abapReleaseDisplay = abapRelease;
		if (ABAP.consistsOfDigitsOnly(abapRelease) && abapRelease.length() == 3) 
			abapReleaseDisplay = abapRelease.substring(0, 1) + "." + abapRelease.substring(1);	

		return " (ABAP " + abapReleaseDisplay + ")";
	}
	
	private Task runJobWithProgressUiIfNeeded(BackgroundJob job) {
		return runJobWithProgressUiIfNeeded(job, Job.CODE_LENGTH_TO_SHOW_PROGRESS_FORM);
	}

	private Task runJobWithProgressUiIfNeeded(BackgroundJob job, int codeLengthToShowProgressForm) {
		if (job.getCodeTextLength() >= codeLengthToShowProgressForm) {
			FrmProgress frmProgress = new FrmProgress();
			frmProgress.open(job);
		} else {
			BusyIndicator.showWhile(shell.getDisplay(), new Runnable() {
				@Override
				public void run() {
					job.run();
				}
			});
		}
		return job.getResult();
	}

	private int selectionChangedId;
	public final void selectionChanged() {
		// start timer to refresh list of used rules 250 ms after the last change 
		// (i.e. not at every keystroke or scrollbar movement, but only if there was no other keystroke in the meantime)
		++selectionChangedId;
		int selectionChangedIdAtStart = selectionChangedId;
		Display.getDefault().timerExec(250, new Runnable() {
			public void run() {
				if (selectionChangedId == selectionChangedIdAtStart)
					refreshUsedRules();
			}
		});
	}

	private void refreshUsedRules() {
		RuleStats[] newUsedRules = codeDisplay.getRuleStats(curProfile);
		if (RuleStats.equals(usedRules, newUsedRules))
			return;
		usedRules = newUsedRules;
		
		++suspendItemCheck;
		try {
			lstUsedRules.setRedraw(false);
			lstUsedRules.removeAll();
			if (usedRules != null && usedRules.length > 0) {
				lstUsedRules.setItemCount(usedRules.length);
				for (int i = 0; i < usedRules.length; ++i) {
					String keyInfo = (i < 26) ? "(" + String.valueOf((char) ('a' + i)) + ") " : "";
					lstUsedRules.getItem(i).setText(keyInfo + usedRules[i].toString());
					lstUsedRules.getItem(i).setChecked(usedRules[i].isUsed());
				}
			}
			lstUsedRules.setRedraw(true);
		} catch (java.lang.Exception e) {
		}
		--suspendItemCheck;
	}

	private boolean refreshFromFile(String path) {
		return refreshFromFile(path, false);
	}

	private boolean refreshFromFile(String path, boolean keepPositionIfSameFile) {
		Persistency persistency = Persistency.get();
		if (!persistency.fileExists(path)) {
			Message.show("File '" + path + "' not found!");
			return false;
		}
		String sourceName = persistency.getFileNameWithoutExtension(path);
		String codeText = persistency.readAllTextFromFile(path);
		if (keepPositionIfSameFile && codeDisplay != null && StringUtil.equalsIgnoreCaseCheckingForNull(path, codeDisplay.getSourcePath()))
			return refreshCode(sourceName, path, codeText, ABAP.NEWEST_RELEASE, true, codeDisplay.getTopLineIndex(), codeDisplay.getCurLineIndex(), codeDisplay.getSelectionStartLine(), null, CleanupRangeExpandMode.FULL_DOCUMENT);
		else
			return refreshCode(sourceName, path, codeText, ABAP.NEWEST_RELEASE, true);
	}

	private void testDirectory(IBatchJob batchJob) {
		String dir = showDirDialog(defaultCodeDirectory, batchJob.getDescription());
		String[] paths = getAllPaths(dir, FileType.CODE, true, true);
		if (paths == null)
			return;
		Arrays.sort(paths);

		StringBuilder detailedResult = new StringBuilder();
		String codeFileInfo = Cult.format(paths.length) + " code files in folder '" + dir + "'";
		String title = batchJob.getTitle(codeFileInfo);
		detailedResult.append(title + System.lineSeparator() + System.lineSeparator());

		BackgroundJob job = new BackgroundJob(batchJob, dir, paths);
		FrmProgress frmProgress = new FrmProgress();
		frmProgress.open(job);
		detailedResult.append(job.getBatchDetails());

		SystemClipboard.setText(detailedResult.toString());
		String summary = job.getBatchSummary() + System.lineSeparator() + "Details were copied to the clipboard.";
		Message.show(summary, title);
	}

	private void getSepFreqForFolder(CommentIdentifierMode mode) {
		String dir = showDirDialog(defaultCodeDirectory, "Test all text files in folder");
		String[] paths = getAllPaths(dir, FileType.CODE, false, true);
		if (paths == null)
			return;

		final CommentIdentifier commentIdentifier = new CommentIdentifier();
		BusyIndicator.showWhile(shell.getDisplay(), new Runnable() {
			@Override
			public void run() {
				Persistency persistency = Persistency.get(); 
				for (String path : paths)
					commentIdentifier.identifyComments(persistency.readAllTextFromFile(path), null, mode);
			}
		});

		String result = commentIdentifier.getSeparatorFrequencies();
		if (!StringUtil.isNullOrEmpty(result)) {
			SystemClipboard.setText(result);
			Message.show("Separator frequencies for " + CommentIdentifier.getScopeDescription(mode) + " copied to Clipboard.");
		}
	}

	private void getWordFreqForFolder(CommentIdentifierMode mode) {
		String dir = showDirDialog(defaultCodeDirectory, "Get comment word frequencies from all text files in folder");
		String[] paths = getAllPaths(dir, FileType.CODE, false, true);
		if (paths == null) 
			return;

		final CommentIdentifier commentIdentifier = new CommentIdentifier();
		BusyIndicator.showWhile(shell.getDisplay(), new Runnable() {
			@Override
			public void run() {
				Persistency persistency = Persistency.get();
				for (String path : paths) {
					String text = persistency.readAllTextFromFile(path);
					String[] lines = StringUtil.split(text, new char[] { '\r', '\n' }, true);
					commentIdentifier.addCommentSamples(lines, mode);
				}
			}
		});
		
		String oldTable = SystemClipboard.getText();
		String result = commentIdentifier.getWordFrequencies(oldTable);
		if (!StringUtil.isNullOrEmpty(result)) {
			SystemClipboard.setText(result);
			Message.show("Word frequencies for " + CommentIdentifier.getScopeDescription(mode) + " copied to Clipboard.");
		}
	}

	private void identifyComments(CommentIdentifierMode mode) {
		CommentIdentifier commentIdentifier = new CommentIdentifier();
		StringBuilder output = new StringBuilder();

		String code = codeDisplay.getCodeToString();
		int lineCount = commentIdentifier.identifyComments(code, output, mode);

		SystemClipboard.setText(output.toString());
		Message.show(Cult.format(lineCount) + " lines processed; result copied to Clipboard.");
	}

	private void cancelAndClose() {
		// even when canceling, save the settings, because it would be confusing if shell bounds etc. were different when the shell is opened again 
		saveSettings();

		resultCode = null;
		resultErrorMessage = null;
		
		shell.dispose();
	}
	
	private void applyAndClose() {
		saveSettings();

		// keep current resultCode and resultErrorMessage 
		
		if (!isPlugin) {
			LastSession lastSession = LastSession.create(codeDisplay.getSourceName(), codeDisplay.getSourcePath(), codeDisplay.getSourceCode(), codeDisplay.getAbapRelease(), codeDisplay.getTopLineIndex(),
					codeDisplay.getCurLineIndex(), codeDisplay.getSelectionStartLine());
			lastSession.save();
		}

		shell.dispose();
	}

	private void refreshProfileList(String profileNameToSelect, boolean suppressReprocessingIfFound) {
		if (settings == null)
			profiles = Profile.loadProfiles(null, null);
		else
			profiles = Profile.loadProfiles(settings.profilesDirectory, settings.readOnlyProfileDirs);
		
		cboProfile.removeAll();

		boolean profileNameFound = false;
		int selectIndex = 0;
		for (Profile profile : profiles) {
			String name = profile.toString();
			if (name.equals(profileNameToSelect)) {
				selectIndex = cboProfile.getItemCount();
				profileNameFound = true;
			}
			cboProfile.add(name);
		}
		cboProfile.select(selectIndex);
		if (!profileNameFound || !suppressReprocessingIfFound)
			profileChanged();
	}

	private void helpAbout() {
		Message.show(Program.getAboutText(), Program.PRODUCT_NAME, SWT.OK);
	}

	public final void searchModeChanged(boolean searchMode) {
		if (searchMode) {
			lblSearch.setFont(searchTextFont);
			lblSearch.setText(TYPE_SEARCH_TEXT);
			lblSearch.setForeground(searchTextNormalColor);
		} else {
			lblSearch.setFont(searchInfoFont);
			lblSearch.setText(SEARCH_INFO);
			lblSearch.setForeground(searchInfoColor);
		}
	}

	public final void searchTextChanged(String text, boolean found) {
		lblSearch.setText(text);
		lblSearch.setForeground(found ? searchTextNormalColor : searchTextNotFoundColor);
	}

	public final boolean searchLeftDisplay() {
		return chkSearchLeftDisplay.getSelection();
	}

	public final boolean searchRightDisplay() {
		return chkSearchRightDisplay.getSelection();
	}

	public final boolean searchChangedLines() {
		return chkSearchChangedLines.getSelection();
	}

	public final boolean matchCase() {
		return chkMatchCase.getSelection();
	}

	public final boolean matchWholeWord() {
		return chkMatchWholeWord.getSelection();
	}

	public final void updateChangedLineCount(ChangeStats changeStats) {
		chkHighlightIndentChanges.setText("Highlight indent changes (" + Cult.format(changeStats.indentCount) + " lines)");
		chkHighlightInnerSpaceChanges.setText("Highlight inner space changes (" + Cult.format(changeStats.innerSpaceCount) + " lines)");
		chkHighlightCaseChanges.setText("Highlight upper/lower case changes (" + Cult.format(changeStats.caseCount) + " lines)");
		chkHighlightContentChanges.setText("Highlight text and line changes (" + Cult.format(changeStats.contentCount) + " lines)");
	}

	private void refreshHighlight() {
		codeDisplay.setHighlight(ChangeTypes.create(chkHighlightIndentChanges.getSelection(), chkHighlightInnerSpaceChanges.getSelection(), chkHighlightCaseChanges.getSelection(), chkHighlightContentChanges.getSelection()));
	}

	private void editProfiles() {
		String curProfileName = (curProfile == null) ? null : curProfile.name;
		FrmProfiles frmProfiles = new FrmProfiles();
		EditProfilesResult profileResult = frmProfiles.open(curProfileName, settings, codeDisplay.getShowVerticalLine(), codeDisplay.getVerticalLinePos(), codeDisplayColors);
		
		// after returning from FrmProfiles, ensure FrmMain appears in the foreground, 
		// even if another window was meanwhile activated and is higher in the Z order
		if (!shell.getMinimized()) {
			shell.forceActive();
		}
		
		if (profileResult != null) {
			// lastProfileName.saved is false if FrmProfiles was closed with "Cancel" or with the red X
			if (profileResult.saved && profileResult.lastProfileName != null) {
				refreshProfileList(profileResult.lastProfileName, false);
			}
		}
	}

	private void profileChanged() {
		if (cboProfile.getSelectionIndex() >= 0) {
			curProfile = profiles.get(cboProfile.getSelectionIndex());
			if (!StringUtil.isNullOrEmpty(codeDisplay.getSourceCode())) {
				refreshCode();
			}
		}
	}

	private void cleanupRangeExpandModeChanged(CleanupRangeExpandMode newValue) {
		settings.cleanupRangeExpandMode = newValue;
		if (!StringUtil.isNullOrEmpty(codeDisplay.getSourceCode()) && originalCleanupRange != null) {
			refreshCode();
		}
	}

	private void releaseRestrictionChanged(String newReleaseRestrictionName) {
		settings.releaseRestriction = ABAP.getReleaseRestrictionNumber(newReleaseRestrictionName);
		if (!StringUtil.isNullOrEmpty(codeDisplay.getSourceCode())) {
			refreshCode();
		}
	}

	private String getReleaseRestrictionDisplay(String releaseRestrictionName) {
		if (releaseRestrictionName.equals(ABAP.NO_RELEASE_RESTRICTION_NAME)) {
			return NO_RELEASE_RESTRICTION_DISPLAY;
		} else {
			return RELEASE_RESTRICTION_PREFIX + releaseRestrictionName;
		}
	}

	private String getReleaseRestrictionName(String releaseRestrictionDisplay) {
		if (releaseRestrictionDisplay.equals(NO_RELEASE_RESTRICTION_DISPLAY)) {
			return ABAP.NO_RELEASE_RESTRICTION_NAME;
		} else {
			return StringUtil.removePrefix(releaseRestrictionDisplay, RELEASE_RESTRICTION_PREFIX, true);
		}
	}

	@Override
	public void keyPressedInCodeDisplay(KeyEvent e) {
		boolean controlPressed = ((e.stateMask & SWT.CONTROL) != 0);
		boolean shiftPressed = ((e.stateMask & SWT.SHIFT) != 0);

		if (e.keyCode == SWT.F1) {
			ProgramLauncher.showHelp(HelpTopic.MAIN);
			e.doit = false;
		} else if (!isPlugin && ((e.stateMask & SWT.CTRL) != 0) && e.keyCode == 'v') {
			codeFromClipboard(true);
			e.doit = false;
		} else if (isPlugin && e.keyCode == SWT.ESC) {
			cancelAndClose();
			e.doit = false;
		} else if (isPlugin && ((e.stateMask & SWT.CTRL) != 0) && e.keyCode == '\r') {
			applyAndClose();
			e.doit = false;
		} else if (e.keyCode >= 'a' && e.keyCode <= 'z') {
			// toggle the used rule at the given position
			int itemIndex = (int)e.keyCode - (int)'a';
			if (itemIndex >= 0 && usedRules != null && usedRules.length > itemIndex) {
				try {
					TableItem item = lstUsedRules.getItem(itemIndex);
					boolean newValue = !item.getChecked();
					item.setChecked(newValue); // this does NOT trigger the event
					setBlockRule(itemIndex, !newValue);
				} catch (IllegalArgumentException | SWTException ex) { 
				}
				e.doit = false;
			}
		} else if (controlPressed && shiftPressed && e.keyCode == SWT.ARROW_RIGHT && Program.showDevFeatures()) {
			openNextMatchingSampleFile(true);
			e.doit = false;
		} else if (controlPressed && shiftPressed && e.keyCode == SWT.ARROW_LEFT && Program.showDevFeatures()) {
			openNextMatchingSampleFile(false);
			e.doit = false;
		} else if (controlPressed && e.keyCode == SWT.F6 && Program.showDevFeatures()) {
			openNextPatternMatchFile(true);
			e.doit = false;
		} else if (controlPressed && e.keyCode == SWT.F5 && Program.showDevFeatures()) {
			openNextPatternMatchFile(false);
			e.doit = false;
		}
	}
	
	private void openNextMatchingSampleFile(boolean ascending) {
		String dir = defaultCodeDirectory;

		String[] paths = getAllPaths(dir, FileType.CODE, true, true);
		if (paths == null) 
			return;
		paths = sortPaths(paths, ascending, codeDisplay.getSourcePath());

		// find the next file in which at least one Command is changed with the current profile settings
		Persistency persistency = Persistency.get(); 
		for (String testPath : paths) {
			String sourceName = persistency.getFileNameWithoutExtension(testPath);
			String codeText = persistency.readAllTextFromFile(testPath);
			BackgroundJob job = new BackgroundJob(ParseParams.createForWholeCode(sourceName, codeText, ABAP.NEWEST_RELEASE), 
															  CleanupParams.createForProfile(curProfile, false, settings.releaseRestriction));
			Task result = runJobWithProgressUiIfNeeded(job);

			RuleStats[] ruleStats = result.getResultingDiffDoc().getRuleStats(curProfile);
			for (RuleStats ruleStat : ruleStats) {
				if (ruleStat.isUsed()) {
					refreshCode(sourceName, testPath, codeText, ABAP.NEWEST_RELEASE, true);
					codeDisplay.moveToFirstChange();
					return;
				}
			}
		}
		
		Message.show("None of the " + String.valueOf(paths.length) + " files is changed with the current profile settings.");
	}

	private void openNextPatternMatchFile(boolean ascending) {
		String dir = defaultCodeDirectory;

		String[] paths = getAllPaths(dir, FileType.CODE, true, true);
		if (paths == null) 
			return;
		paths = sortPaths(paths, ascending, codeDisplay.getSourcePath());

		// find the next file in which at least one Command satisfies the hard-coded Command.matchesPattern()
		Persistency persistency = Persistency.get(); 
		for (String testPath : paths) {
			String sourceName = persistency.getFileNameWithoutExtension(testPath);
			String codeText = persistency.readAllTextFromFile(testPath);
			BackgroundJob job = new BackgroundJob(ParseParams.createForWholeCode(sourceName, codeText, ABAP.NEWEST_RELEASE), 
															  CleanupParams.createForProfile(curProfile, false, settings.releaseRestriction));
			Task result = runJobWithProgressUiIfNeeded(job);

			Command command = result.getResultingCode().getFirstPatternMatch();
			if (command != null) {
				refreshCode(sourceName, testPath, codeText, ABAP.NEWEST_RELEASE, true);
				codeDisplay.moveToFirstPatternMatch();
				return;
			}
		}
		
		Message.show("None of the " + String.valueOf(paths.length) + " files contains a Command that satisfies the Command.matchesPattern() condition.");
	}

	private void patternMatchesToClip(boolean doCleanup) {
		String dir = defaultCodeDirectory;
		String LINE_SEP = System.lineSeparator();
		
		String[] paths = getAllPaths(dir, FileType.CODE, true, true);
		if (paths == null) 
			return;
		paths = sortPaths(paths, true, codeDisplay.getSourcePath());

		int fileCount = 0;
		int matchCount = 0;
		StringBuilder sb = new StringBuilder();
		Persistency persistency = Persistency.get(); 
		for (String testPath : paths) {
			String sourceName = persistency.getFileNameWithoutExtension(testPath);
			String codeText = persistency.readAllTextFromFile(testPath);
			CleanupParams cleanupParams = doCleanup ? CleanupParams.createForProfile(curProfile, false, settings.releaseRestriction) : null; 
			BackgroundJob job = new BackgroundJob(ParseParams.createForWholeCode(sourceName, codeText, ABAP.NEWEST_RELEASE), cleanupParams);
			Task result = runJobWithProgressUiIfNeeded(job);

			boolean matchInFile = false;
			Code code = result.getResultingCode();
			Command command = code.firstCommand;
			while (command != null) {
				if (command.matchesPattern()) {
					++matchCount;
					if (!matchInFile) {
						++fileCount;
						matchInFile = true;
					}
					String folderFile = testPath.substring(dir.length());
					folderFile = StringUtil.removeSuffix(folderFile, persistency.getExtension(testPath), true);
					sb.append(LINE_SEP);
					sb.append(ABAP.COMMENT_SIGN_STRING + " " + folderFile + ": line " + Cult.format(command.getSourceLineNumStart()) + LINE_SEP);

					// append the Command itself and the corresponding opening / closing Command
					addPatternMatch(sb, command, false);
					
					// append related commands, if any
					ArrayList<Command> relatedCommands = command.getCommandsRelatedToPatternMatch();
					if (relatedCommands != null) {
						for (Command relatedCommand : relatedCommands) {
							sb.append(System.lineSeparator());
							addPatternMatch(sb, relatedCommand, true);
						}
					}
					
					sb.append(System.lineSeparator());
				}
				command = command.getNext();
			}
		}

		if (sb.length() > 0) 
			SystemClipboard.setText(sb.toString());

		String taskInfo = "Pattern matches " + (doCleanup ? "after cleanup" : "before cleanup");
		String matchInfo = " found for " + Cult.format(matchCount) + " commands in " + Cult.format(fileCount) + " files";
		String clipInfo = (sb.length() > 0) ? " (details see clipboard)" : "";
		Message.show(taskInfo + matchInfo + clipInfo);
	}

	private void addPatternMatch(StringBuilder sb, Command command, boolean addLineNumber) {
		// for Commands like [END]LOOP, [END]IF etc., add the corresponding opening and/or closing Command, too, 
		// so the resulting text can more easily be used for testing
		Command openingCommand = command.getOpeningCommand();
		if (openingCommand != null) {
			sb.append(removeMultipleLineBreaks(openingCommand.toString()));
		}

		if (addLineNumber) {
			sb.append(System.lineSeparator());
			sb.append(StringUtil.repeatChar(' ', command.getFirstToken().spacesLeft));
			sb.append(ABAP.COMMENT_SIGN_STRING + " line " + String.valueOf(command.getSourceLineNumStart()));
		}
		sb.append(removeMultipleLineBreaks(command.toString()));
		
		Command closingCommand = command.getClosingCommand();
		if (closingCommand != null) {
			sb.append(removeMultipleLineBreaks(closingCommand.toString()));
		}
	}

	private String removeMultipleLineBreaks(String codeText) {
		return System.lineSeparator() + StringUtil.removePrefixRecursively(codeText, System.lineSeparator(), false);
	}

	private void setBlockRule(int usedRulesIndex, boolean blocked) {
		if (usedRulesIndex >= 0 && usedRules != null && usedRules.length > usedRulesIndex) {
			codeDisplay.setBlockRuleInSelection(usedRules[usedRulesIndex].getRuleID(), blocked);
			codeDisplay.reprocessSelection(curProfile, settings.releaseRestriction);
		}
		codeDisplay.focusDisplay();
	}

	private void watchClipboard() {
		if (!mmuCodeWatchClipboard.getSelection()) 
			return;
		
		mmuCodeWatchClipboard.getDisplay().timerExec(WATCH_CLIPBOARD_INTERVAL_MS, new Runnable() {
			String lastClipboard = SystemClipboard.getText();
			
			public void run() {
				if (!mmuCodeWatchClipboard.getSelection())
					return;

				String curClipboard = SystemClipboard.getText();
				if (curClipboard != null && (lastClipboard == null || !lastClipboard.equals(curClipboard))) {
					int finalLfCount = StringUtil.suffixCount(curClipboard, "\n", false);
					if (codeFromClipboard(false)) {
						// clipboard was successfully processed 
						codeToClipboard(finalLfCount);
						Display.getCurrent().beep(); // TODO: this is a bit obtrusive ... - alternatives?
						lastClipboard = SystemClipboard.getText();

					} else { 
						// clipboard could NOT be processed successfully
						
						// TODO: play an error sound, e.g. 
						// InputStream inputStream = getClass().getResourceAsStream("/sounds/error.wav"); 
						// AudioStream audioStream = new AudioStream(inputStream);
						// AudioPlayer.player.start(audioStream);
						lastClipboard = curClipboard;
					}
				}
				
				if (!shell.isDisposed()) {
					mmuCodeWatchClipboard.getDisplay().timerExec(WATCH_CLIPBOARD_INTERVAL_MS, this);
				}
			}
		});
	}
	
	private void createRulesDocumentation() {
		RuleDocumentation ruleDoc = new RuleDocumentation(Profile.createDefault().getRulesSortedByGroup());

		Persistency persistency = Persistency.get();
		String docsDir = persistency.combinePaths(persistency.getWorkDir(), RuleDocumentation.DOCS_FOLDER);

		ruleDoc.deleteOldRuleDocs(docsDir, persistency);
		ruleDoc.create(docsDir, persistency);
		
		Message.show("Documentation saved to '" + docsDir + "'. Press OK to open folder.");
		if (persistency.directoryExists(docsDir))
			ProgramLauncher.startProcess(docsDir);
	}
	
	private void openProfilesFolder() {
		Persistency persistency = Persistency.get();

		String savePath = Profile.getSavePath(settings.profilesDirectory, Profile.DEFAULT_NAME);
		String saveDir = persistency.getDirectoryName(savePath);
		if (!persistency.directoryExists(saveDir)) {
			// if no directory is found, create one ...  
			persistency.createDirectory(saveDir);
			// ... and try again
			if (!persistency.directoryExists(saveDir)) {
				Message.show("Local profiles folder not found; failed to create folder '" + saveDir + "'");
				return;
			}
		}
		ProgramLauncher.startProcess(saveDir);
	}

	private String showDirDialog(String filterPath, String message) {
		DirectoryDialog dirDialog = new DirectoryDialog(shell);
		dirDialog.setFilterPath(filterPath);
		dirDialog.setMessage(message);
		return dirDialog.open();
	}

	private String[] getAllPaths(String dir, FileType fileType, boolean recursive, boolean showNotFoundMessage) {
		if (dir == null || dir.length() == 0)
			return null;
		
		Persistency persistency = Persistency.get();
		String extension = "*" + persistency.getExtension(FileType.CODE);
		String[] paths = persistency.getFilesInDirectory(dir, extension, recursive);
		if (paths == null || paths.length == 0) {
			if (showNotFoundMessage)
				Message.show("No " + extension + " files found in folder " + dir);
			return null;
		}
		return paths;
	}
	
	private String[] sortPaths(String[] paths, boolean ascending, String startAfterPath) {
		if (paths == null)
			return paths;
		
		Arrays.sort(paths);
		
		// identify the current file in the (sorted) list
		int readIndex = 0;
		if (!StringUtil.isNullOrEmpty(startAfterPath)) {
			for (int i = 0; i < paths.length; ++i) {
				if (startAfterPath.equals(paths[i])) {
					readIndex = i;
					break;
				}
			}
		}
		if (readIndex == 0 && ascending)
			return paths;
		
		String[] result = new String[paths.length];
		int writeIndex = 0;
		for (int i = 0; i < paths.length; ++i) {
			readIndex = (ascending ? (readIndex + 1) : (readIndex + paths.length - 1)) % paths.length;
			result[writeIndex] = paths[readIndex];
			++writeIndex;
		}
		
		return result;
	}
}
