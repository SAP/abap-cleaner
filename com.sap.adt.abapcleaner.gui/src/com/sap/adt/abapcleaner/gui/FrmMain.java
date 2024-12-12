package com.sap.adt.abapcleaner.gui;

import java.io.IOException;
import java.io.PrintStream;
import java.time.LocalDate;
import java.time.LocalDateTime;
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

	private CodeDisplayColors codeDisplayColors; // is set to either codeDisplayColorsADT or codeDisplayColorsStrong  
	private CodeDisplayColors codeDisplayColorsADT;
	private CodeDisplayColors codeDisplayColorsClassic;
	private static final Color searchTextNotFoundColor = new Color(139, 0, 54); // dark red
	private static Color searchInfoColor;
	private static Color searchTextNormalColor;
	private Font searchInfoFont;
	private Font searchTextFont;

	private boolean isPlugin;
	private boolean isReadOnly;
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
	private MenuItem mmuUseAdtColors;
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
				cleanInteractively(null, ABAP.NEWEST_RELEASE, null, persistency.getStartupPath(), false, null, null, null, false);

			} else {
				if (commandLineArgs.action == CommandLineAction.SHOW_HELP) {
					System.out.print(CommandLineArgs.getHelp(persistency));

				} else if (commandLineArgs.action == CommandLineAction.SHOW_VERSION) {
					System.out.print(Program.PRODUCT_NAME + " " + Program.getVersion());

				} else if (commandLineArgs.hasErrors()) {
					if (commandLineArgs.hasErrors()) {
						System.err.println(commandLineArgs.errors);
						System.err.println("");
						System.err.flush();
					}
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
		if (!Program.wasInitialized()) {
			Program.initialize(null, "");
		}
	}

	public static void cleanAutomatically(CommandLineArgs commandLineArgs, PrintStream out) throws CommandLineException {
		// create or read a profile
		Profile profile = null;
		if (StringUtil.isNullOrEmpty(commandLineArgs.profileData)) {
			profile = Profile.createDefault();
		} else {
			try (ISettingsReader reader = TextSettingsReader.createFromString(commandLineArgs.profileData, Program.TECHNICAL_VERSION)) {
				profile = Profile.createFromSettings(reader, "");
			} catch (IOException ex) {
				out.println(ex.getMessage());
				return;
			}
		}

		if (commandLineArgs.isInSingleSourceMode()) {
			cleanSingleSourceAutomatically(commandLineArgs, commandLineArgs.sourceCode, out, profile);
		} else {
			cleanMultiSourceAutomatically(commandLineArgs, out, profile);
		}
	}

	private static void cleanMultiSourceAutomatically(CommandLineArgs commandLineArgs, PrintStream out, Profile profile) {
		Persistency persistency = Persistency.get();
		int baseSourcePathLength = commandLineArgs.sourceDir.length();

		for (String sourcePath : commandLineArgs.sourcePaths) {
			String sourceCode = persistency.readAllTextFromFile(sourcePath);

			CleanupResult result = cleanAutomatically(sourceCode, commandLineArgs.abapRelease, commandLineArgs.cleanupRange, null, profile, commandLineArgs.showStats, commandLineArgs.lineSeparator);
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
		CleanupResult result = cleanAutomatically(commandLineArgs.sourceCode, commandLineArgs.abapRelease, commandLineArgs.cleanupRange, null, profile, commandLineArgs.showStats, commandLineArgs.lineSeparator);
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
				if (output.length() > 0 && commandLineArgs.lineSeparator.indexOf(output.charAt(output.length() - 1)) < 0)
					output += commandLineArgs.lineSeparator;
				persistency.writeAllTextToFile(targetPath, output);
			}
		}
	}

	public static CleanupResult cleanAutomatically(String sourceCode, String abapRelease, CleanupRange cleanupRange, String workspaceDir, Profile profile, boolean provideRuleStats, String lineSeparator) {
		initialize();

		MainSettings settings = new MainSettings(workspaceDir);
		settings.initialize(workspaceDir);
		settings.load();

		if (profile == null) {
			StringBuilder errorMessages = new StringBuilder();
			profile = getMostRecentlyUsedProfile(settings, errorMessages);
			// if the profile was not found, notify the caller that a fallback profile will be used for the next attempt;
			// if no profile exists at all, this fallback will be Profile.DEFAULT_NAME, with which the message will not come up again
			String lastProfileName = settings.getLastProfileName();
			if (!StringUtil.isNullOrEmpty(lastProfileName) && !lastProfileName.equals(profile.name)) {
				String oldProfileName = lastProfileName;
				String fallbackProfileName = profile.name;
				settings.setLastProfileName(fallbackProfileName);
				settings.save();
				String warning = "Cleanup cancelled: Profile '" + oldProfileName + "' was not found anymore. For the next attempt, profile '" + fallbackProfileName + "' will be used.";
				if (errorMessages.length() > 0) {
					warning += System.lineSeparator() + System.lineSeparator() + errorMessages.toString();
				}
				return CleanupResult.createError(warning);
			}
		}
		
		BackgroundJob job = new BackgroundJob(ParseParams.createForCleanupRange("", sourceCode, abapRelease, cleanupRange, settings.getCleanupRangeExpandMode()),
				CleanupParams.createForProfile(profile, false, settings.getReleaseRestriction()));
		job.run();
		Task result = job.getResult();

		if (result.getSuccess()) {
			CleanupResult cleanupResult = result.getResultingCode().toCleanupResult(lineSeparator);

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

	public static CleanupResult cleanInteractively(String sourceCode, String abapRelease, CleanupRange cleanupRange, String workspaceDir, boolean isPlugin, String sourcePageTitle,
			CodeDisplayColors codeDisplayColorsADT, CodeDisplayColors codeDisplayColorsClassic, boolean isReadOnly) {
		initialize();

		Persistency persistency = Persistency.get();

		String[] codeDirs = persistency.getExistingDirs(FileType.CODE);
		defaultCodeDirectory = (codeDirs == null || codeDirs.length == 0) ? persistency.getWorkDir() : codeDirs[0];

		FrmMain window = new FrmMain();
		window.codeDisplayColorsADT = (codeDisplayColorsADT != null) ? codeDisplayColorsADT : CodeDisplayColors.createDefault(ColorProfile.ADT);
		window.codeDisplayColorsClassic = (codeDisplayColorsClassic != null) ? codeDisplayColorsClassic : CodeDisplayColors.createDefault(ColorProfile.CLASSIC);
		window.codeDisplayColors = window.codeDisplayColorsADT;
		window.open(isPlugin, sourcePageTitle, sourceCode, abapRelease, cleanupRange, workspaceDir, isReadOnly);

		if (window.resultCode != null) {
			return window.resultCode.toCleanupResult(ABAP.LINE_SEPARATOR);
		} else if (!StringUtil.isNullOrEmpty(window.resultErrorMessage)) {
			return CleanupResult.createError(window.resultErrorMessage);
		} else {
			return null;
		}
	}

	/** returns the profile that was last used, or a fallback profile from the available profiles, or a newly created default profile */
	private static Profile getMostRecentlyUsedProfile(MainSettings settings, StringBuilder errorMessages) {
		// find the profile that was last used according to the settings
		ArrayList<Profile> profiles = Profile.loadProfiles(settings.profilesDirectory, settings.readOnlyProfileDirs, errorMessages);
		String lastProfileName = settings.getLastProfileName();
		for (Profile profile : profiles) {
			if (profile.toString().equals(lastProfileName)) {
				return profile;
			}
		}
		// if no profile was found for the supplied name, use the 'default' profile (cp. refreshProfileList())
		for (Profile profile : profiles) {
			if (profile.toString().equals(Profile.DEFAULT_NAME)) {
				return profile;
			}
		}
		// if no such profile exists, either, use the first profile; otherwise create a new default profile (but don't save it)
		if (profiles.size() > 0) {
			return profiles.get(0);
		} else { 
			return Profile.createDefault();
		}
	}

	public FrmMain() {
		settings = new MainSettings(null); // the workspaceDir will be supplied in .open()
		settings.setDefault();
	}

	/**
	 * @wbp.parser.entryPoint
	 */
	public void open(boolean isPlugin, String sourcePageTitle, String sourceCode, String abapRelease, CleanupRange cleanupRange, String workspaceDir, boolean isReadOnly) {
		this.isPlugin = isPlugin;
		this.isReadOnly = isReadOnly;
		settings.initialize(workspaceDir);
		
		Display display = Display.getDefault();

		createContents();

		lblWatchClipboardInfo.setVisible(false);

		searchInfoColor = display.getSystemColor(SWT.COLOR_TITLE_INACTIVE_FOREGROUND); // SystemColors.ControlDark;
		searchTextNormalColor = display.getSystemColor(SWT.COLOR_LIST_FOREGROUND); // SystemColors.WindowText;
		FontData modelFont = lblSearch.getFont().getFontData()[0];
		searchInfoFont = new Font(display, modelFont.getName(), modelFont.getHeight(), SWT.NORMAL);
		searchTextFont = new Font(display, modelFont.getName(), modelFont.getHeight(), SWT.BOLD);

		loadSettings();
		String lastProfileName = settings.getLastProfileName();
		String selectProfileName = StringUtil.isNullOrEmpty(lastProfileName) ? Profile.DEFAULT_NAME : lastProfileName;
		StringBuilder errorMessages = new StringBuilder();
		boolean lastProfileFound = refreshProfileList(selectProfileName, false, errorMessages);

		codeDisplay.setUsedRulesDisplay(this);
		codeDisplay.setSearchControls(this);
		codeDisplay.setChangeTypeControls(this);
		codeDisplay.setFallbackKeyListener(this);
		codeDisplay.setColors(codeDisplayColors);
		refreshHighlight();

		if (isPlugin) {
			mmuCodeWatchClipboard.dispose();
			if (!isReadOnly) {
				mmuCodeFromClipboard.dispose();
				mmuCodeFromFile.dispose();
				mmuCodeClearDisplay.dispose();
				mmuCodeSeparator.dispose();
			}
			mmuCodeExit.dispose();

			if (isReadOnly) {
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
			if (!refreshCode(sourcePageTitle, "", sourceCode, abapRelease, false, -1, -1, -1, cleanupRange, settings.getCleanupRangeExpandMode()) && isPlugin) {
				dispose();
				return;
			}

		} else {
			LastSession lastSession = LastSession.createEmpty();
			lastSession.load();
			if (!StringUtil.isNullOrEmpty(lastSession.getCodeText())) {
				lastSession.reloadCodeTextFromCodePath();
				refreshCode(lastSession.getSourceName(), lastSession.getSourcePath(), lastSession.getCodeText(), lastSession.getAbapRelease(), false, lastSession.getTopLineIndex(),
						lastSession.getCurLineIndex(), lastSession.getSelectionStartLine(), null, CleanupRangeExpandMode.FULL_DOCUMENT);
			} else {
				refreshCode("", "", "", ABAP.NEWEST_RELEASE, false);
			}
		}

		lstUsedRules.setVisible(true);

		if (lastProfileName != null && !lastProfileFound && curProfile != null) {
			Message.show(getProfileFallbackMessage(lastProfileName, curProfile.name, errorMessages), "Profile not found", shell);
		}

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
		SWTResourceManager.dispose(); // for fonts, colors, images created in .createContents()
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
				String lastProfileName = (curProfile == null) ? null : curProfile.name;
				String profileName = (curProfile == null) ? Profile.DEFAULT_NAME : curProfile.name;
				StringBuilder errorMessages = new StringBuilder();
				boolean suppressReprocessingIfFound = (curProfile != null);
				boolean profileFound = refreshProfileList(profileName, suppressReprocessingIfFound, errorMessages);
				if (lastProfileName != null && !profileFound && curProfile != null) {
					Message.show(getProfileFallbackMessage(lastProfileName, curProfile.name, errorMessages), "Profile not found", shell);
				}

				codeDisplay.formActivated();
			}

			@Override
			public void shellClosed(ShellEvent e) {
				if (isPlugin) {
					cancelAndClose();
				} else {
					applyAndClose();
				}
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

		mmuUseAdtColors = new MenuItem(menuView, SWT.CHECK);
		mmuUseAdtColors.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean useAdtColors = mmuUseAdtColors.getSelection();
				if (settings != null)
					settings.colorProfile = useAdtColors ? ColorProfile.ADT : ColorProfile.CLASSIC;
				codeDisplayColors = useAdtColors ? codeDisplayColorsADT : codeDisplayColorsClassic; 
				if (codeDisplay != null && !codeDisplay.isDisposed()) {
					codeDisplay.setColors(codeDisplayColors);
				}
			}
		});
		mmuUseAdtColors.setText("Use ADT-Style Colors");

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
		mmuExtrasTestActiveRulesOnFolder.setText("Test Active Rules on All Files in &Folder...");

		MenuItem mmuExtrasTestAllRulesOnFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasTestAllRulesOnFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				testDirectory(new CleanupBatchJob(CleanupParams.createForProfile(curProfile, true, ABAP.NO_RELEASE_RESTRICTION)));
			}
		});
		mmuExtrasTestAllRulesOnFolder.setText("Test All Rules on All Files in &Folder...");

		MenuItem mmuExtrasStressTestAllRulesOnFolder = new MenuItem(menuExtras, SWT.CASCADE);
		mmuExtrasStressTestAllRulesOnFolder.setText("Stress-Test All Rules on All Files in Folder...");

		Menu menuExtrasStressTestAllRulesOnFolder = new Menu(shell, SWT.DROP_DOWN);
		mmuExtrasStressTestAllRulesOnFolder.setMenu(menuExtrasStressTestAllRulesOnFolder);

		MenuItem mmuExtrasStressTestAllRulesOnFolder4 = new MenuItem(menuExtrasStressTestAllRulesOnFolder, SWT.PUSH);
		mmuExtrasStressTestAllRulesOnFolder4.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				testDirectory(new CleanupBatchJob(getCleanupParamsForStressTest(), StressTestParams.create(0, 3, StressTestType.getAll())));
			}
		});
		mmuExtrasStressTestAllRulesOnFolder4.setText("Insert Comment/Pragma/Colon After Token 0..3");

		MenuItem mmuExtrasStressTestAllRulesOnFolder8 = new MenuItem(menuExtrasStressTestAllRulesOnFolder, SWT.PUSH);
		mmuExtrasStressTestAllRulesOnFolder8.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				testDirectory(new CleanupBatchJob(getCleanupParamsForStressTest(), StressTestParams.create(0, 7, StressTestType.getAll())));
			}
		});
		mmuExtrasStressTestAllRulesOnFolder8.setText("Insert Comment/Pragma/Colon After Token 0..7");

		MenuItem mmuExtrasStressTestAllRulesOnFolder16 = new MenuItem(menuExtrasStressTestAllRulesOnFolder, SWT.PUSH);
		mmuExtrasStressTestAllRulesOnFolder16.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				testDirectory(new CleanupBatchJob(getCleanupParamsForStressTest(), StressTestParams.create(0, 15, StressTestType.getAll())));
			}
		});
		mmuExtrasStressTestAllRulesOnFolder16.setText("Insert Comment/Pragma/Colon After Token 0..15");

		MenuItem mmuExtrasStressTestAllRulesOnFolder32 = new MenuItem(menuExtrasStressTestAllRulesOnFolder, SWT.PUSH);
		mmuExtrasStressTestAllRulesOnFolder32.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				testDirectory(new CleanupBatchJob(getCleanupParamsForStressTest(), StressTestParams.create(0, 31, StressTestType.getAll())));
			}
		});
		mmuExtrasStressTestAllRulesOnFolder32.setText("Insert Comment/Pragma/Colon After Token 0..31");

		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasKeywordFreqCodeFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasKeywordFreqCodeFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				testDirectory(new AbapKeywordFreqBatchJob());
			}
		});
		mmuExtrasKeywordFreqCodeFolder.setText("Get Code Keyword Frequencies for Folder...");

		MenuItem mmuExtrasSepFreqCodeFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasSepFreqCodeFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				getSepFreqForFolder(CommentIdentifierMode.CODE_ONLY);
			}
		});
		mmuExtrasSepFreqCodeFolder.setText("Get Code Separator Frequencies for Folder...");

		MenuItem mmuExtrasSepFreqEnglishFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasSepFreqEnglishFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				getSepFreqForFolder(CommentIdentifierMode.NON_LINE_START_COMMENTS);
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
				identifyComments(CommentIdentifierMode.CODE_ONLY);
			}
		});
		mmuExtrasTestCommentIdentifier.setText("Test &Comment Identifier with Code Lines");

		MenuItem mmuExtrasTestLineStartCommentLines = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasTestLineStartCommentLines.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				identifyComments(CommentIdentifierMode.LINE_START_COMMENTS);
			}
		});
		mmuExtrasTestLineStartCommentLines.setText("Test Comment Identifier with Line-Start Comment Lines");

		MenuItem mmuExtrasTestNonLineStartCommentLines = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasTestNonLineStartCommentLines.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				identifyComments(CommentIdentifierMode.NON_LINE_START_COMMENTS);
			}
		});
		mmuExtrasTestNonLineStartCommentLines.setText("Test Comment Identifier with Non-Line-Start Comment Lines");

		MenuItem mmuExtrasTestAllCommentLines = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasTestAllCommentLines.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				identifyComments(CommentIdentifierMode.ALL_COMMENT_LINES);
			}
		});
		mmuExtrasTestAllCommentLines.setText("Test Comment Identifier with All Comment Lines");

		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasApplyCamelCaseToClipboard = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasApplyCamelCaseToClipboard.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				applyCamelCaseToClipboard();
			}
		});
		mmuExtrasApplyCamelCaseToClipboard.setText("Apply CamelCase VDM View and Field Names to Clipboard");

		MenuItem mmuExtrasAnalyzeDdlSemanticsInFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasAnalyzeDdlSemanticsInFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				analyzeCdsViewsInFolder(DdlAnalyzer.semanticsElemRefAnnotationPaths, null, false);
			}
		});
		mmuExtrasAnalyzeDdlSemanticsInFolder.setText("Analyze Semantic Refs for CDS Views in Folder...");

		MenuItem mmuExtrasAnalyzeDdlValueHelpInFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasAnalyzeDdlValueHelpInFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				analyzeCdsViewsInFolder(DdlAnalyzer.valueHelpDefAnnotationPaths, null, true);
			}
		});
		mmuExtrasAnalyzeDdlValueHelpInFolder.setText("Analyze Value Help for CDS Views in Folder...");

		MenuItem mmuExtrasAnalyzeDdlFieldDataSources = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasAnalyzeDdlFieldDataSources.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
		      FrmInputBox inputBox = new FrmInputBox();
		      String commaSeparatedfieldNames = inputBox.open("", "Comma-Separated Field Names to Analyze", true, shell);
		      String[] fieldNames = StringUtil.split(commaSeparatedfieldNames, ",", true, true);
				analyzeCdsViewsInFolder(null, fieldNames, true); 
			}
		});
		mmuExtrasAnalyzeDdlFieldDataSources.setText("Analyze Data Sources for Fields of CDS Views in Folder...");

		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasNextMatchingSample = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasNextMatchingSample
				.setToolTipText("Opens the next code file from the /user/code folder (and subfolders) in which code is changed with the current profile settings.");
		mmuExtrasNextMatchingSample.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				openNextMatchingSampleFile(true);
			}
		});
		mmuExtrasNextMatchingSample.setText(getMenuItemTextWithAccelerator("Open Next Match", SWT.SHIFT + SWT.CTRL + SWT.ARROW_RIGHT));

		MenuItem mmuExtrasPrevMatchingSample = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasPrevMatchingSample
				.setToolTipText("Opens the previous code file from the /user/code folder (and subfolders) in which code is changed with the current profile settings.");
		mmuExtrasPrevMatchingSample.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				openNextMatchingSampleFile(false);
			}
		});
		mmuExtrasPrevMatchingSample.setText(getMenuItemTextWithAccelerator("Open Previous Match", SWT.SHIFT + SWT.CTRL + SWT.ARROW_DOWN));

		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasNextPatternMatch = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasNextPatternMatch
				.setToolTipText("Opens the next code file from the /user/code folder (and subfolders) in which a Command satisfies the hard-coded Command.patternMatch().");
		mmuExtrasNextPatternMatch.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				openNextPatternMatchFile(true);
			}
		});
		mmuExtrasNextPatternMatch.setText(getMenuItemTextWithAccelerator("Open Next Pattern Match", SWT.CTRL + SWT.F6));

		MenuItem mmuExtrasPrevPatternMatch = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasPrevPatternMatch
				.setToolTipText("Opens the previous code file from the /user/code folder (and subfolders) in which a Command satisfies the hard-coded Command.patternMatch().");
		mmuExtrasPrevPatternMatch.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				openNextPatternMatchFile(false);
			}
		});
		mmuExtrasPrevPatternMatch.setText(getMenuItemTextWithAccelerator("Open Previous Pattern Match", SWT.CTRL + SWT.F5));

		MenuItem mmuExtrasPatternMatchesToClip = new MenuItem(menuExtras, SWT.CASCADE);
		mmuExtrasPatternMatchesToClip.setText("Copy Pattern Matches to Clipboard (all Files)");

		Menu menuExtrasPatternMatchesToClip = new Menu(shell, SWT.DROP_DOWN);
		mmuExtrasPatternMatchesToClip.setMenu(menuExtrasPatternMatchesToClip);

		MenuItem mmuExtrasPatternMatchesBeforeCleanupToClip = new MenuItem(menuExtrasPatternMatchesToClip, SWT.PUSH);
		mmuExtrasPatternMatchesBeforeCleanupToClip.setToolTipText(
				"Fills the clipboard with all commands from the code files in the /user/code folder (and subfolders) that satisfy the hard-coded Command.patternMatch() before cleanup.");
		mmuExtrasPatternMatchesBeforeCleanupToClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				patternMatchesToClip(false, false);
			}
		});
		mmuExtrasPatternMatchesBeforeCleanupToClip.setText("Pattern Matches Before Cleanup");

		MenuItem mmuExtrasPatternMatchesAfterCleanupToClip = new MenuItem(menuExtrasPatternMatchesToClip, SWT.PUSH);
		mmuExtrasPatternMatchesAfterCleanupToClip.setToolTipText(
				"Fills the clipboard with all commands from the code files in the /user/code folder (and subfolders) that satisfy the hard-coded Command.patternMatch() after cleanup with the current profile.");
		mmuExtrasPatternMatchesAfterCleanupToClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				patternMatchesToClip(true, false);
			}
		});
		mmuExtrasPatternMatchesAfterCleanupToClip.setText("Pattern Matches After Cleanup");

		MenuItem mmuExtrasPatternMatchOriginalsAfterCleanupToClip = new MenuItem(menuExtrasPatternMatchesToClip, SWT.PUSH);
		mmuExtrasPatternMatchOriginalsAfterCleanupToClip.setToolTipText(
				"Fills the clipboard with all (original) commands from the code files in the /user/code folder (and subfolders) that satisfy the hard-coded Command.patternMatch() after cleanup with the current profile.");
		mmuExtrasPatternMatchOriginalsAfterCleanupToClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				patternMatchesToClip(true, true);
			}
		});
		mmuExtrasPatternMatchOriginalsAfterCleanupToClip.setText("Original Code Of Pattern Matches After Cleanup");

		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasObfuscateCodeToClip = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasObfuscateCodeToClip.setToolTipText("Obfuscates the code, but not the literals.");
		mmuExtrasObfuscateCodeToClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				obfuscateToClip(false, false);
			}
		});
		mmuExtrasObfuscateCodeToClip.setText("Obfuscate Code to Clipboard");

		MenuItem mmuExtrasObfuscateCodeAndLiteralsToClip = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasObfuscateCodeAndLiteralsToClip.setToolTipText("Obfuscates code and literals.");
		mmuExtrasObfuscateCodeAndLiteralsToClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				obfuscateToClip(false, true);
			}
		});
		mmuExtrasObfuscateCodeAndLiteralsToClip.setText("Obfuscate Code and Literals to Clipboard");

		MenuItem mmuExtrasObfuscateCodeLiteralsCommentsToClip = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasObfuscateCodeLiteralsCommentsToClip.setToolTipText("Obfuscates code and literals and removes comments.");
		mmuExtrasObfuscateCodeLiteralsCommentsToClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				obfuscateToClip(true, true);
			}
		});
		mmuExtrasObfuscateCodeLiteralsCommentsToClip.setText("Obfuscate Code and Literals and Remove Comments");

		MenuItem mmuExtrasCommandStrucFrequencyToClip = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasCommandStrucFrequencyToClip.setToolTipText(
				"Fills the clipboard with a frequency list of structure-identical commands (after obfuscation).");
		mmuExtrasCommandStrucFrequencyToClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				commandStrucFrequencyToClip();
			}
		});
		mmuExtrasCommandStrucFrequencyToClip.setText("Copy Command Structure Frequency to Clipboard");

		new MenuItem(menuExtras, SWT.SEPARATOR);

		MenuItem mmuExtrasPreprocessCamelCaseNamesFromClip = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasPreprocessCamelCaseNamesFromClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					preprocessCamelCaseNamesFromClip();
				} catch (Exception ex) {
				}
			}
		});
		mmuExtrasPreprocessCamelCaseNamesFromClip.setText("Preprocess Known CamelCase Names From Clipboard");

		MenuItem mmuExtrasUpdateCamelCaseNamesFromFolder = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasUpdateCamelCaseNamesFromFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					updateCamelCaseNamesFromFolder();
				} catch (Exception ex) {
				}
			}
		});
		mmuExtrasUpdateCamelCaseNamesFromFolder.setText("Update Resources of Known CamelCase Names From Folder");

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

		MenuItem mmuExtrasCreateReleaseNoteDocu = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasCreateReleaseNoteDocu.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					createReleaseNoteDocumentation();
				} catch (Exception ex) {
				}
			}
		});
		mmuExtrasCreateReleaseNoteDocu.setText("Create Release Note Documentation From Git Log");

		MenuItem mmuExtrasCreateRuleCountTimeline = new MenuItem(menuExtras, SWT.NONE);
		mmuExtrasCreateRuleCountTimeline.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					createRuleTimeline();
				} catch (Exception ex) {
				}
			}
		});
		mmuExtrasCreateRuleCountTimeline.setText("Create Timeline of Rule and Option Count");

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
					Message.show("The error log (" + path + ") is empty.", shell);
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

		codeDisplay = new CodeDisplay(shell, SWT.NONE, false);
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
						// the next time FrmProfile is opened, preselect this rule
						if (settings != null) { 
							settings.profilesLastRuleID = usedRules[itemIndex].getRuleID();
						}
						if (e.detail == SWT.CHECK) {
							// if check state was changed, (un)block the rule and reprocess the selection
							setBlockRule(itemIndex, !lstUsedRules.getItem(itemIndex).getChecked());
						} else if ((e.stateMask & SWT.CONTROL) != 0) {
							// open FrmProfiles to configure this rule
							editProfiles();
							codeDisplay.focusDisplay();
						}
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

		// depending on OS, create buttons "Apply and Close - Cancel" (Windows) or "Cancel - Apply and Close" (macOS, Linux)
		if (SystemInfo.putOKBeforeCancel()) {
			btnApplyAndClose = new Button(cpsApplyOrCancel, SWT.NONE);
			btnCancel = new Button(cpsApplyOrCancel, SWT.NONE);
		} else {
			btnCancel = new Button(cpsApplyOrCancel, SWT.NONE);
			btnApplyAndClose = new Button(cpsApplyOrCancel, SWT.NONE);
		}
		
		btnApplyAndClose.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		btnApplyAndClose.setText("Apply and Close");
		btnApplyAndClose.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				applyAndClose();
			}
		});

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
		if (isPlugin && !isReadOnly)
			return false;

		if (!SystemClipboard.containsText()) {
			Message.show("The clipboard is empty!", shell);
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
		String lastProfileName = (curProfile != null && curProfile.name != null) ? curProfile.name : "";
		settings.setLastProfileName(lastProfileName);
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

		mmuUseAdtColors.setSelection(settings.colorProfile == ColorProfile.ADT);
		mmuHighlightDeclarationKeywords.setSelection(settings.highlightDeclarationKeywords);
		mmuHighlightWritePositions.setSelection(settings.highlightWritePositions);

		codeDisplayColors = (settings.colorProfile == ColorProfile.CLASSIC) ? codeDisplayColorsClassic : codeDisplayColorsADT; 
		
		// restore shell bounds
		try {
			// allow saved shell bounds to exceed screen size for a few pixels, which happens if the shell was 
			// aligned to the left-hand or right-hand half of the screen, e.g. with .shellLeft = -8
			final int TOLERANCE = 12;
			shell.setMaximized(settings.shellMaximized);
			if (!settings.shellMaximized && settings.areShellBoundsSpecified()) {
				org.eclipse.swt.graphics.Rectangle displayBounds = shell.getDisplay().getBounds();
				if (displayBounds.contains(settings.shellLeft + TOLERANCE, settings.shellTop + TOLERANCE) 
						&& displayBounds.contains(settings.getShellRight() - 1 - TOLERANCE, settings.getShellBottom() - 1 - TOLERANCE)) {
					shell.setBounds(settings.shellLeft, settings.shellTop, settings.shellWidth, settings.shellHeight);
				}
			}
		} catch (SWTException ex) { // ERROR_WIDGET_DISPOSED, ERROR_THREAD_INVALID_ACCESS
		}

		if (!cboCleanupRangeExpandMode.isDisposed()) {
			cboCleanupRangeExpandMode.select(settings.getCleanupRangeExpandMode().getValue());
		}

		String releaseRestrictionName = ABAP.getReleaseRestrictionName(settings.getReleaseRestriction());
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
	}

	private boolean refreshCode() {
		// keep source name, path, code and abapRelease, and also try to keep the position
		return refreshCode(null, null, null, null, true, codeDisplay.getTopLineIndex(), codeDisplay.getCurLineIndex(), codeDisplay.getSelectionStartLine(), originalCleanupRange,
				settings.getCleanupRangeExpandMode());
	}

	private boolean refreshCode(String newSourceName, String newSourcePath, String newCodeText, String newAbapRelease, boolean showMessages) {
		return refreshCode(newSourceName, newSourcePath, newCodeText, newAbapRelease, showMessages, 0, 0, 0, null, CleanupRangeExpandMode.FULL_DOCUMENT);
	}

	private boolean refreshCode(String newSourceName, String newSourcePath, String newCodeText, String newAbapRelease, boolean showMessages, int topLineIndex, int curLineIndex,
			int selectionStartLine, CleanupRange cleanupRange, CleanupRangeExpandMode cleanupRangeExpandMode) {
		String sourceName = (newSourceName != null) ? newSourceName : codeDisplay.getSourceName();
		String sourcePath = (newSourcePath != null) ? newSourcePath : codeDisplay.getSourcePath();
		String sourceCode = (newCodeText != null) ? newCodeText : codeDisplay.getSourceCode();
		String abapRelease = (newAbapRelease != null) ? newAbapRelease : codeDisplay.getAbapRelease();

		BackgroundJob job = new BackgroundJob(ParseParams.createForCleanupRange(sourceName, sourceCode, abapRelease, cleanupRange, cleanupRangeExpandMode),
				CleanupParams.createForProfile(curProfile, false, settings.getReleaseRestriction()));
		Task result = runJobWithProgressUiIfNeeded(job);

		resultCode = null;
		resultErrorMessage = null;

		if (!result.getSuccess()) {
			if (!job.wasCancelled()) {
				if (showMessages) { // TODO: otherwise, display it on a Label? (for 'Watch and Modify Clipboard' function)
					Message.show(result.getErrorMessage(), shell);
				} else {
					resultErrorMessage = result.getErrorMessage();
				}
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
		updateShellText(sourceName, newAbapRelease, null);
		shell.setText(Program.PRODUCT_NAME + " - " + sourceName + abapReleaseInfo);
		codeDisplay.setInfo(sourceName, sourcePath, sourceCode, abapRelease, curProfile.getSingleActiveRule());
		codeDisplay.refreshCode(result.getResultingCode(), result.getResultingDiffDoc(), curProfile, topLineIndex, curLineIndex, selectionStartLine);
		if (Program.showDevFeatures()) {
			updateShellText(sourceName, newAbapRelease, result);
		}
		// remember the resulting Code instance; the CleanupResult will only be created from it when the window is closed
		resultCode = result.getResultingCode();

		if (result.getLogSummary() != null) { // even with result.getSuccess() == true, there may be warnings in the log
			if (showMessages) { // TODO: otherwise, display it on a Label? (for 'Watch and Modify Clipboard' function)
				Message.show(result.getLogSummary(), shell);
			}
		}
		return true;
	}

	private void updateShellText(String sourceName, String abapRelease, Task result) {
		String abapReleaseInfo = getReleaseInfo(abapRelease);
		String shellText = Program.PRODUCT_NAME + " - " + sourceName + abapReleaseInfo;
		if (result != null  && Program.showDevFeatures()) {
			shellText += " - " + result.getCalculationTimeInfo();
		}
		shell.setText(shellText);
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
				if (selectionChangedId == selectionChangedIdAtStart) {
					refreshUsedRules();
				}
			}
		});
	}

	private void refreshUsedRules() {
		RuleStats[] newUsedRules = codeDisplay.getRuleStatsOfSelection(curProfile);
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
			Message.show("File '" + path + "' not found!", shell);
			return false;
		}
		String sourceName = persistency.getFileNameWithoutExtension(path);
		String codeText = persistency.readAllTextFromFile(path);
		if (keepPositionIfSameFile && codeDisplay != null && StringUtil.equalsIgnoreCaseCheckingForNull(path, codeDisplay.getSourcePath())) {
			return refreshCode(sourceName, path, codeText, ABAP.NEWEST_RELEASE, true, codeDisplay.getTopLineIndex(), codeDisplay.getCurLineIndex(),
					codeDisplay.getSelectionStartLine(), null, CleanupRangeExpandMode.FULL_DOCUMENT);
		} else {
			return refreshCode(sourceName, path, codeText, ABAP.NEWEST_RELEASE, true);
		}
	}

	private CleanupParams getCleanupParamsForStressTest() { // TODO Auto-generated method stub
		// if a profile is named "stress test", use that profile, otherwise the current profile
		Profile useProfile = curProfile;
		for (Profile profile : profiles) {
			if (profile.name.equalsIgnoreCase("stress test")) {
				useProfile = profile;
				break;
			}
		}
		return CleanupParams.createForProfile(useProfile, true, ABAP.NO_RELEASE_RESTRICTION);
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

		Persistency persistency = Persistency.get();
		String fileName = Program.PRODUCT_NAME + " testDirectory " + Cult.getReverseDateTime(LocalDateTime.now(), true) + ".txt";
		String path = persistency.combinePaths(persistency.getTempDir(), fileName);
		persistency.writeAllTextToFile(path, detailedResult.toString());

		String clipInfo = "Details were saved to '" + path + "'";
		try {
			SystemClipboard.setText(detailedResult.toString());
			clipInfo += " and copied to the clipboard.";
		} catch (IllegalStateException ex) {
			// cannot open system clipboard
		}
		String summary = job.getBatchSummary() + System.lineSeparator() + clipInfo;
		Message.show(summary, title, shell);
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
				for (String path : paths) {
					commentIdentifier.identifyComments(persistency.readAllTextFromFile(path), null, mode);
				}
			}
		});

		String result = commentIdentifier.getSeparatorFrequencies();
		if (!StringUtil.isNullOrEmpty(result)) {
			SystemClipboard.setText(result);
			Message.show("Separator frequencies for " + CommentIdentifier.getScopeDescription(mode) + " copied to Clipboard.", shell);
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
					Language codeLanguage = Language.preview(text);
					String[] lines = StringUtil.split(text, new char[] { '\r', '\n' }, true);
					commentIdentifier.addCommentSamples(lines, mode, codeLanguage);
				}
			}
		});

		String oldTable = SystemClipboard.getText();
		String result = commentIdentifier.getWordFrequencies(oldTable);
		if (!StringUtil.isNullOrEmpty(result)) {
			SystemClipboard.setText(result);
			Message.show("Word frequencies for " + CommentIdentifier.getScopeDescription(mode) + " copied to Clipboard.", shell);
		}
	}

	private void applyCamelCaseToClipboard() {
		if (!SystemClipboard.containsText())
			return;
		
		CamelCaseNames viewNames = CamelCaseNames.getViewNames();
		CamelCaseNames fieldNames = CamelCaseNames.getFieldNames();

		int totalViewNameCount = 0;
		int totalFieldNameCount = 0;
		int replacedViewNameCount = 0;
		int replacedFieldNameCount = 0;
		
		String text = SystemClipboard.getText();
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < text.length(); ++i) {
			if (ABAP.isCharAllowedForVariableNames(text, i, true, false, true)) {
				String name = ABAP.readTillEndOfVariableName(text, i, false, true);
				String camelCase = viewNames.applyCamelCaseTo(name, false, false, curProfile);
				if (camelCase != null) {
					++totalViewNameCount;
					if (!name.equals(camelCase)) {
						++replacedViewNameCount;
					}
				} else {
					camelCase = fieldNames.applyCamelCaseTo(name, false, false, curProfile);
					if (camelCase != null) {
						++totalFieldNameCount;
						if (!name.equals(camelCase)) {
							++replacedFieldNameCount;
						}
					}
				}
				sb.append((camelCase != null) ? camelCase : name);
				i += name.length() - 1;
			} else {
				sb.append(text.charAt(i));
			}
		}
		SystemClipboard.setText(sb.toString());
		
		String totalViewNames = StringUtil.getCountAndUnit(totalViewNameCount, "view name", "view names");
		String totalFieldNames = StringUtil.getCountAndUnit(totalFieldNameCount, "field name", "field names");
		
		String msg = "Replaced " + Cult.format(replacedViewNameCount) + " / " + totalViewNames 
						+ " and " + Cult.format(replacedFieldNameCount) + " / " + totalFieldNames + " in clipboard.";
		Message.show(msg, shell);
	}
	
	private void analyzeCdsViewsInFolder(String[] annotationPaths, String[] fieldNames, boolean considerIgnorePropagation) {
		String dir = showDirDialog(defaultCodeDirectory, "Analyze CDS views in folder");
		String[] paths = getAllPaths(dir, FileType.CODE, false, true);
		if (paths == null)
			return;

		final DdlAnalyzer ddlAnalyzer = new DdlAnalyzer();
		BusyIndicator.showWhile(shell.getDisplay(), new Runnable() {
			@Override
			public void run() {
				Persistency persistency = Persistency.get();
				for (String path : paths) {
					String sourceName = persistency.getFileNameWithoutExtension(path);
					String codeText = persistency.readAllTextFromFile(path);
					Language codeLanguage = Language.preview(codeText); 
					if (codeLanguage == Language.DDL) {
						BackgroundJob job = new BackgroundJob(ParseParams.createForWholeCode(sourceName, codeText, ABAP.NEWEST_RELEASE), null);
						Task result = runJobWithProgressUiIfNeeded(job);
						ddlAnalyzer.addFile(sourceName, result.getResultingCode());
					}
				}
			}
		});
		ddlAnalyzer.finishBuild();

		if (annotationPaths != null)
			ddlAnalyzer.analyzeAnnotations(annotationPaths, considerIgnorePropagation);
		else if (fieldNames != null)
			ddlAnalyzer.analyzeFieldDataSources(fieldNames);
		
		String result = ddlAnalyzer.getResult(annotationPaths, fieldNames);
		if (!StringUtil.isNullOrEmpty(result)) {
			SystemClipboard.setText(result);
			Message.show("CDS View analysis copied to Clipboard.", shell);
		}
	}

	private void identifyComments(CommentIdentifierMode mode) {
		CommentIdentifier commentIdentifier = new CommentIdentifier();
		StringBuilder output = new StringBuilder();

		String code = codeDisplay.getCodeToString();
		int lineCount = commentIdentifier.identifyComments(code, output, mode);

		SystemClipboard.setText(output.toString());
		Message.show(Cult.format(lineCount) + " lines processed; result copied to Clipboard.", shell);
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
			LastSession lastSession = LastSession.create(codeDisplay.getSourceName(), codeDisplay.getSourcePath(), codeDisplay.getSourceCode(), codeDisplay.getAbapRelease(),
					codeDisplay.getTopLineIndex(), codeDisplay.getCurLineIndex(), codeDisplay.getSelectionStartLine());
			lastSession.save();
		}

		shell.dispose();
	}

	/** returns true if the supplied profileNameToSelect was found, otherwise false */
	private boolean refreshProfileList(String profileNameToSelect, boolean suppressReprocessingIfFound, StringBuilder errorMessages) {
		if (settings == null)
			profiles = Profile.loadProfiles(null, null, errorMessages);
		else
			profiles = Profile.loadProfiles(settings.profilesDirectory, settings.readOnlyProfileDirs, errorMessages);

		cboProfile.removeAll();

		boolean profileNameFound = false;
		int selectIndex = 0;
		int defaultIndex = -1;
		for (Profile profile : profiles) {
			String name = profile.toString();
			if (name.equals(profileNameToSelect)) {
				selectIndex = cboProfile.getItemCount();
				profileNameFound = true;
			}
			if (name.equals(Profile.DEFAULT_NAME)) {
				defaultIndex = cboProfile.getItemCount();
			}
			cboProfile.add(name);
		}
		// if no profile was found with the supplied profile name, fallback to the 'default' profile (cp. getMostRecentlyUsedProfile()), 
		// or otherwise, simply to the first profile (Profile.loadProfiles() ensures that there is at least one of them)
		if (!profileNameFound && defaultIndex >= 0)
			selectIndex = defaultIndex;
		
		cboProfile.select(selectIndex);
		if (!profileNameFound || !suppressReprocessingIfFound)
			profileChanged();
		return profileNameFound;
	}

	private void helpAbout() {
		Message.show(Program.getAboutText(), Program.PRODUCT_NAME, SWT.OK, shell);
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
		lblSearch.setText(StringUtil.getLabelText(text));
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
		codeDisplay.setHighlight(ChangeTypes.create(chkHighlightIndentChanges.getSelection(), chkHighlightInnerSpaceChanges.getSelection(), chkHighlightCaseChanges.getSelection(),
				chkHighlightContentChanges.getSelection()));
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
				refreshProfileList(profileResult.lastProfileName, false, null);
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
		settings.setCleanupRangeExpandMode(newValue);
		if (!StringUtil.isNullOrEmpty(codeDisplay.getSourceCode()) && originalCleanupRange != null) {
			refreshCode();
		}
	}

	private void releaseRestrictionChanged(String newReleaseRestrictionName) {
		settings.setReleaseRestriction(ABAP.getReleaseRestrictionNumber(newReleaseRestrictionName));
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
		} else if ((!isPlugin || isReadOnly) && ((e.stateMask & SWT.CTRL) != 0) && e.keyCode == 'v') {
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
			int itemIndex = (int) e.keyCode - (int) 'a';
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
		} else if (!controlPressed && !shiftPressed && e.keyCode == SWT.F8 && Program.showDevFeatures()) {
			openNextMatchingSampleFile(true);
			e.doit = false;
		} else if (!controlPressed && !shiftPressed && e.keyCode == SWT.F7 && Program.showDevFeatures()) {
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
					CleanupParams.createForProfile(curProfile, false, settings.getReleaseRestriction()));
			Task result = runJobWithProgressUiIfNeeded(job);

			String errorMsg = result.getErrorMessage();
			if (!StringUtil.isNullOrEmpty(errorMsg)) {
				String msg = "Error in " + sourceName + ": " + errorMsg + System.lineSeparator() + System.lineSeparator() + "Do you want to open the source file in a text editor?"; 
				if (Message.show(msg, "Error in " + sourceName, SWT.YES | SWT.NO | SWT.ICON_QUESTION, shell) == SWT.YES) 
					ProgramLauncher.startProcess(testPath);
				continue;
			}
			RuleStats[] ruleStats = result.getResultingDiffDoc().getRuleStats(curProfile);
			for (RuleStats ruleStat : ruleStats) {
				if (ruleStat.isUsed()) {
					refreshCode(sourceName, testPath, codeText, ABAP.NEWEST_RELEASE, true);
					codeDisplay.moveToFirstChange();
					return;
				}
			}
		}

		Message.show("None of the " + String.valueOf(paths.length) + " files is changed with the current profile settings.", shell);
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
					CleanupParams.createForProfile(curProfile, false, settings.getReleaseRestriction()));
			Task result = runJobWithProgressUiIfNeeded(job);

			Command command = result.getResultingCode().getFirstPatternMatch();
			if (command != null) {
				refreshCode(sourceName, testPath, codeText, ABAP.NEWEST_RELEASE, true);
				codeDisplay.moveToFirstPatternMatch();
				return;
			}
		}

		Message.show("None of the " + String.valueOf(paths.length) + " files contains a Command that satisfies the Command.matchesPattern() condition.", shell);
	}

	private void patternMatchesToClip(boolean doCleanup, boolean useOriginalCode) {
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
			CleanupParams cleanupParams = doCleanup ? CleanupParams.createForProfile(curProfile, false, settings.getReleaseRestriction()) : null;
			BackgroundJob job = new BackgroundJob(ParseParams.createForWholeCode(sourceName, codeText, ABAP.NEWEST_RELEASE), cleanupParams);
			Task result = runJobWithProgressUiIfNeeded(job);

			boolean matchInFile = false;
			Code code = result.getResultingCode();
			Code originalCode = null;
			Command command = code.firstCommand;
			while (command != null) {
				if (command.matchesPattern()) {
					++matchCount;
					if (!matchInFile) {
						++fileCount;
						matchInFile = true;
					}
					// if the original code before cleanup shall be used, try to find the corresponding Command in the original code
					if (doCleanup && useOriginalCode && originalCode == null) {
						// lazy parsing 
						BackgroundJob parseOnlyJob = new BackgroundJob(ParseParams.createForWholeCode(sourceName, codeText, ABAP.NEWEST_RELEASE), null);
						originalCode = runJobWithProgressUiIfNeeded(parseOnlyJob).getResultingCode();
					}
					Command useCommand = command;
					boolean isAfterCleanup = doCleanup;
					if (doCleanup && useOriginalCode) {
						int sourceLine = (command.originalCommand == null) ? command.getSourceLineNumStart() : command.originalCommand.getSourceLineNumStart();
						Command originalCommand = originalCode.searchCommandAt(sourceLine);
						if (originalCommand != null) {
							useCommand = originalCommand;
							isAfterCleanup = false;
						}
					}

					String folderFile = testPath.substring(dir.length());
					folderFile = StringUtil.removeSuffix(folderFile, persistency.getExtension(testPath), true);
					sb.append(LINE_SEP);
					sb.append(ABAP.COMMENT_SIGN_STRING + " " + folderFile + ": line " + Cult.format(useCommand.getSourceLineNumStart()));
					sb.append(isAfterCleanup ? " (after cleanup)" : " (before cleanup)").append(LINE_SEP);

					// append the Command itself and the corresponding opening / closing Command
					addPatternMatch(sb, useCommand, false);

					// append related commands, if any
					ArrayList<Command> relatedCommands = useCommand.getCommandsRelatedToPatternMatch();
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
		Message.show(taskInfo + matchInfo + clipInfo, shell);
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
			RuleID ruleID = usedRules[usedRulesIndex].getRuleID();
			// the next time FrmProfile is opened, preselect this rule  
			if (settings != null) { 
				settings.profilesLastRuleID = ruleID;
			}
			boolean changed = codeDisplay.setBlockRuleInSelection(ruleID, blocked);
			if (changed) {
				Task result = codeDisplay.reprocessSelection(curProfile, settings.getReleaseRestriction());
				if (result != null && Program.showDevFeatures()) {
					updateShellText(codeDisplay.getSourceName(), codeDisplay.getAbapRelease(), result);
				}
			}
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

	private void preprocessCamelCaseNamesFromClip() {
		final String GTNC_SUFFIX = "__GTNC"; // use two underscores, so this file will be processed first
		
		String clip = SystemClipboard.getText();
		if (StringUtil.isNullOrEmpty(clip)) {
			Message.show("Clipboard is empty", shell);
			return;
		}
		String targetDir = showDirDialog(defaultCodeDirectory, "Target directory for known CamelCase names of CDS views and components");
		if (targetDir == null || targetDir.length() == 0)
			return;

		CamelCaseNameType nameType;
		if (clip.startsWith("CDS View Name"))
			nameType = CamelCaseNameType.VIEW;
		else if (clip.startsWith("Field Name"))
			nameType = CamelCaseNameType.FIELD;
		else {
			Message.show("Expected clipboard to start with cell 'CDS View Name' or 'Field Name'", shell);
			return;
		}
		
		String targetFilePrefix = (nameType == CamelCaseNameType.VIEW) ? CamelCaseNames.VIEW_NAMES_SOURCE_PREFIX : CamelCaseNames.FIELD_NAMES_SOURCE_PREFIX;
		String targetPath = Persistency.get().combinePaths(targetDir, targetFilePrefix + GTNC_SUFFIX + ".csv");
		StringBuilder sbResult = new StringBuilder();
		String resultMsg = CamelCaseNames.preprocessFromGTNC(nameType, clip, sbResult);
		if (sbResult.length() > 0) {
			Persistency.get().writeAllTextToFile(targetPath, sbResult.toString(), false);
		}
		Message.show(resultMsg, shell);
	}

	private void updateCamelCaseNamesFromFolder() {
		String sourceDir = showDirDialog(defaultCodeDirectory, "Source directory for known CamelCase names of CDS views and components");
		if (sourceDir == null || sourceDir.length() == 0)
			return;

		String targetDir = showDirDialog(defaultCodeDirectory, "Target directory for resources");
		if (targetDir == null || targetDir.length() == 0)
			return;
		
		updateCamelCaseNames(CamelCaseNameType.VIEW, sourceDir, CamelCaseNames.VIEW_NAMES_SOURCE_PREFIX, targetDir, CamelCaseNames.VIEW_NAMES_RESOURCE);
		updateCamelCaseNames(CamelCaseNameType.FIELD, sourceDir, CamelCaseNames.FIELD_NAMES_SOURCE_PREFIX, targetDir, CamelCaseNames.FIELD_NAMES_RESOURCE);

		Message.show("Please refresh resources folder in IDE with F5 and restart.", shell);
	}

	private void updateCamelCaseNames(CamelCaseNameType camelCaseNameType, String sourceDir, String prefix, String targetDir, String resourceFile) {
		Persistency persistency = Persistency.get();
		String searchPattern = prefix + "*.csv;" + prefix + "*.txt";
		String[] paths = persistency.getFilesInDirectory(sourceDir, searchPattern, false);
		if (paths == null || paths.length == 0) {
			return;
		}
		StringBuilder sbSummary = new StringBuilder();
		StringBuilder sbDetails = new StringBuilder();
		boolean checkStartsWithZ = (camelCaseNameType == CamelCaseNameType.VIEW);
		boolean checkUnknownPrefixesOrSuffixes = true;
		CamelCaseNames camelCaseNames = CamelCaseNames.createFromTextFiles(camelCaseNameType, paths, sbSummary, sbDetails, checkStartsWithZ, checkUnknownPrefixesOrSuffixes);
		String targetPath = persistency.combinePaths(targetDir, resourceFile);
		try {
			camelCaseNames.saveAsResource(targetPath);
			sbSummary.append("For details, paste the clipboard to Excel.").append(System.lineSeparator());
			sbSummary.append(System.lineSeparator());
			sbSummary.append("Resource file saved successfully to '" + targetPath + "'");
		} catch (IOException e) {
			sbSummary.append(e.getMessage());
		}
		SystemClipboard.setText(sbDetails.toString());
		Message.show(sbSummary.toString(), "Update CamelCase Names in Resources: " + prefix, shell);
	}
	
	private void createRulesDocumentation() {
		RuleDocumentation ruleDoc = new RuleDocumentation(Profile.createDefault().getRulesSortedByGroup());

		Persistency persistency = Persistency.get();
		String docsDir = persistency.combinePaths(persistency.getWorkDir(), RuleDocumentation.DOCS_FOLDER);

		ruleDoc.deleteOldRuleDocs(docsDir, persistency);
		ruleDoc.create(docsDir, persistency);

		Message.show("Documentation saved to '" + docsDir + "'. Press OK to open folder.", shell);
		if (persistency.directoryExists(docsDir))
			ProgramLauncher.startProcess(docsDir);
	}

	private void createReleaseNoteDocumentation() {
		final String[] thanks = new String[] { "**Thank you** very much", "**Great thanks** to", "Great **thanks** to", "**Thanks a lot**,", "**Thanks** a lot to", "**Thank you**",
				"Many **thanks** to" };
		final String[] reasonsWithFixes = new String[] { "for your contributions, ideas and bug reports that led to these improvements!",
				"for reporting the bugs behind this release!", "for the reporting the bugs behind these fixes!", "for the bug reports behind these fixes!",
				"for their ideas and bug reports!", "for the ideas and bug reports behind these improvements!", "for inspiring these improvements and fixes!",
				"for opening the issues that led to these enhancements and fixes!" };
		final String[] reasonsWithoutFixes = new String[] { "for your contributions and ideas that led to these improvements!", "for the issues behind these improvements!",
				"for opening the issues behind these improvements!", "for all your ideas!", "for inspiring these improvements!",
				"for opening the issues that led to these improvements!" };
		final String SEP = System.lineSeparator();
		final int HASH_LENGTH_MIN = 7;
		final int HASH_LENGTH_MAX = 12;

		String text = SystemClipboard.getText();
		if (StringUtil.isNullOrEmpty(text)) {
			Message.show("Please run 'git log --oneline' and copy relevant lines to the clipboard first.", shell);
			SystemClipboard.setText("git log --oneline");
			return;
		}

		String[] lines = StringUtil.split(text, new char[] { '\r', '\n' }, true);
		StringBuilder result = new StringBuilder();
		HashSet<String> issueNums = new HashSet<>();
		boolean containsFix = false;
		for (String line : lines) {
			line = line.trim();

			// remove hash
			int spacePos = line.indexOf(' ');
			if (spacePos >= HASH_LENGTH_MIN && spacePos <= HASH_LENGTH_MAX && StringUtil.consistsOf(line.substring(0, spacePos), "0123456789abcdef")) {
				line = line.substring(spacePos + 1).trim();
			}

			// remove initial parenthesis like (HEAD -> v#.#.#), (upstream/main), (tag: v#.#.#)
			if (StringUtil.startsWith(line, "(", false)) {
				int closePos = line.indexOf(')');
				if (closePos > 0) {
					line = line.substring(closePos + 1).trim();
				}
			}

			// replace first word
			spacePos = line.indexOf(' ');
			if (spacePos > 1) {
				String[] replacements = new String[] { "add->Added", "allow->Allowed", "change->Changed", "create->Created", "enhance->Enhanced", "fix->Fixed", "improve->Improved",
						"include->Included", "move->Moved", "replace->Replaced", "update->Updated" };
				String firstWord = line.substring(0, spacePos);
				boolean found = false;
				for (String replacement : replacements) {
					String[] replaceBits = StringUtil.split(replacement, "->", true);
					if (firstWord.equals(replaceBits[0])) {
						firstWord = replaceBits[1];
						break;
					}
				}
				if (!found) {
					firstWord = firstWord.substring(0, 1).toUpperCase() + firstWord.substring(1);
				}
				line = "* " + firstWord + line.substring(spacePos);
				if (firstWord.equals("Fixed")) {
					containsFix = true;
				}
			}

			// replace Rule names
			Rule[] rules = curProfile.getAllRules();
			for (Rule rule : rules) {
				String ruleName = rule.getClass().getSimpleName();
				int namePos = line.indexOf(ruleName);
				int nameEnd = namePos + ruleName.length();
				if (namePos >= 0 && (namePos == 0 || !Character.isLetterOrDigit(line.charAt(namePos - 1)))
						&& (nameEnd == line.length() || !Character.isLetterOrDigit(line.charAt(nameEnd)))) {
					line = line.substring(0, namePos) + "rule '**" + rule.getDisplayName() + "**'" + line.substring(nameEnd);
				}
			}
			
			// replace 'Added new rule "..." ...' with 'Added new rule '**...**' ...'
			String newRulePrefix = "new rule \"";
			String newRuleSuffix = "\"";
			int newRulePos = line.indexOf(newRulePrefix);
			if (newRulePos >= 0) {
				newRulePos += newRulePrefix.length();
				int newRuleEnd = line.indexOf(newRuleSuffix, newRulePos);
				if (newRuleEnd >= 0) {
					line = line.substring(0, newRulePos - 1) + "'**" + line.substring(newRulePos, newRuleEnd) + "**'" + line.substring(newRuleEnd + 1);
				}
			}

			// replace issue reference
			final String ISSUE_REF_START = " (#";
			final String ISSUE_REF_END = ")";
			int issueRefPos = line.indexOf(ISSUE_REF_START);
			int issueNumEnd = line.indexOf(ISSUE_REF_END, issueRefPos + ISSUE_REF_START.length());
			if (issueRefPos >= 0 && issueNumEnd >= 0) {
				int issueNumStart = issueRefPos + ISSUE_REF_START.length();
				String issueNum = line.substring(issueNumStart, issueNumEnd);
				issueNums.add(issueNum);
				line = line.substring(0, issueRefPos) + " ([#" + issueNum + "](../../../issues/" + issueNum + "))" + line.substring(issueNumEnd + ISSUE_REF_END.length());
			}

			result.append(line).append(SEP);
		}

		StringBuilder header = new StringBuilder();
		header.append("## " + Cult.getReverseDate(LocalDateTime.now(), "-"));
		Release[] releases = Program.getReleases();
		if (releases != null && releases.length > 0 && !releases[0].releaseDate.isBefore(LocalDate.now())) {
			header.append(" (version " + releases[0].version.toString() + ")");
		} else {
			header.append(" (version #.#.#)");
		}
		header.append(SEP + SEP);
		header.append(thanks[new Random().nextInt(thanks.length)]);
		int contributorCount = 0;
		for (String issueNum : issueNums) {
			if (contributorCount > 0)
				header.append((contributorCount + 1 < issueNums.size()) ? "," : " and");
			if ((contributorCount + 1) % 3 == 0)
				header.append(SEP);
			header.append(" [**" + issueNum + "**](https://github.com/" + issueNum + ")");
			++contributorCount;
		}
		String[] reasons = containsFix ? reasonsWithFixes : reasonsWithoutFixes;
		header.append(" " + reasons[new Random().nextInt(reasons.length)]);
		header.append(SEP + SEP);

		if (result.length() == 0) {
			Message.show("No log line found.", shell);
		} else {
			SystemClipboard.setText(header.toString() + result.toString());
			Message.show("The release note documentation was copied to the clipboard.", shell);
		}
	}

	private class RuleTimeline {
		public final LocalDate date;
		public int addedRuleCount;
		public int addedConfigCount;
		public int totalRuleCount;
		public int totalConfigCount;
		public String addedRuleNames = "";
		public String addedTechnicalRuleNames = "";
		public String addedConfigNames = "";
		public RuleTimeline(LocalDate date) {
			this.date = date;
		}
		public void addRule(Rule rule) {
			String ruleName = rule.getDisplayName();
			String technicalRuleName = rule.getClass().getSimpleName();
			++addedRuleCount;
			if (StringUtil.isNullOrEmpty(addedRuleNames)) {
				addedRuleNames = ruleName;
				addedTechnicalRuleNames = technicalRuleName;
			} else {
				addedRuleNames += ", " + ruleName;
				addedTechnicalRuleNames += ", " + technicalRuleName;
			}
		}
		public void addConfigValue(Rule rule, ConfigValue configValue) {
			String technicalRuleName = rule.getClass().getSimpleName();
			String configName = configValue.settingName;
			++addedConfigCount;
			String addName = technicalRuleName + "_" + configName;
			if (StringUtil.isNullOrEmpty(addedConfigNames)) {
				addedConfigNames = addName;
			} else {
				addedConfigNames += ", " + addName;
			}
		}
		public void finalize(RuleTimeline prevRuleTimeline) {
			totalRuleCount = addedRuleCount + ((prevRuleTimeline == null) ? 0 : prevRuleTimeline.totalRuleCount);
			totalConfigCount = addedConfigCount + ((prevRuleTimeline == null) ? 0 : prevRuleTimeline.totalConfigCount);
		}
		public String toHeaderLine() {
			StringBuilder sb = new StringBuilder();
			sb.append("date");
			sb.append('\t').append("added rule count");
			sb.append('\t').append("added configuration count");
			sb.append('\t').append("total rule count");
			sb.append('\t').append("total configuration count");
			sb.append('\t').append("options per rule");
			sb.append('\t').append("added rule names");
			sb.append('\t').append("added technical rule names");
			sb.append('\t').append("added configuration names");
			return sb.toString();
		}
		public String toLine() {
			StringBuilder sb = new StringBuilder();
			sb.append(date.toString());
			sb.append('\t').append(String.valueOf(addedRuleCount));
			sb.append('\t').append(String.valueOf(addedConfigCount));
			sb.append('\t').append(String.valueOf(totalRuleCount));
			sb.append('\t').append(String.valueOf(totalConfigCount));
			double configPerRule = (totalRuleCount == 0) ? 0 : totalConfigCount / (double)totalRuleCount; 
			sb.append('\t').append(Cult.format(configPerRule, 3));
			sb.append('\t').append(String.valueOf(addedRuleNames));
			sb.append('\t').append(String.valueOf(addedTechnicalRuleNames));
			sb.append('\t').append(String.valueOf(addedConfigNames));
			return sb.toString();
		}
	}
	private class RuleTimelineComparator implements Comparator<RuleTimeline> {
		@Override
		public int compare(RuleTimeline o1, RuleTimeline o2) {
			if (o1.date.isBefore(o2.date)) {
				return -1;
			} else if (o1.date.isAfter(o2.date)) {
				return 1;
			} else {
				return 0;
			}
		}
	}
	private void createRuleTimeline() {
		HashMap<LocalDate, RuleTimeline> ruleTimeline = new HashMap<>();
		LocalDate projectStart = LocalDate.of(2020, 12, 19);
		ruleTimeline.put(projectStart, new RuleTimeline(projectStart));
		for (Rule rule : curProfile.getAllRules()) {
			LocalDate dateCreated = rule.getDateCreated();
			if (!ruleTimeline.containsKey(dateCreated))
				ruleTimeline.put(dateCreated, new RuleTimeline(dateCreated));
			ruleTimeline.get(dateCreated).addRule(rule);

			for (ConfigValue configValue : rule.getConfigValues()) {
				if (configValue instanceof ConfigInfoValue)
					continue;
				LocalDate dateConfigCreated = (configValue.dateCreated == null) ? dateCreated : configValue.dateCreated;
				if (!ruleTimeline.containsKey(dateConfigCreated))
					ruleTimeline.put(dateConfigCreated, new RuleTimeline(dateConfigCreated));
				ruleTimeline.get(dateConfigCreated).addConfigValue(rule, configValue);
			}
		}
		ArrayList<RuleTimeline> ruleTimelineSorted = new ArrayList<>(ruleTimeline.values());
		ruleTimelineSorted.sort(new RuleTimelineComparator());
		RuleTimeline prevRuleStat = null;
		for (RuleTimeline ruleStat : ruleTimelineSorted) {
			ruleStat.finalize(prevRuleStat);
			prevRuleStat = ruleStat;
		}
		StringBuilder sbResult = new StringBuilder();
		sbResult.append(ruleTimelineSorted.get(0).toHeaderLine()).append(System.lineSeparator());
		for (RuleTimeline ruleStat : ruleTimelineSorted) {
			sbResult.append(ruleStat.toLine()).append(System.lineSeparator());
		}
		SystemClipboard.setText(sbResult.toString());
		Message.show("The rule and configuration count timeline was copied to the clipboard", shell);
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
				Message.show("Local profiles folder not found; failed to create folder '" + saveDir + "'", shell);
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
				Message.show("No " + extension + " files found in folder " + dir, shell);
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
		if (readIndex == paths.length - 1 && ascending)
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
	
	private void obfuscateToClip(boolean removeComments, boolean obfuscateLiterals) {
   	Obfuscator obfuscator = Obfuscator.createFor(codeDisplay.getCodeLanguage(), false, false, true, removeComments, removeComments, obfuscateLiterals);
   	Code code;
   	try {
   		code = obfuscator.obfuscate(resultCode.toString());
		} catch (ParseException | UnexpectedSyntaxAfterChanges e) {
			Message.show(e.getMessage(), shell);
			return;
		}
   	SystemClipboard.setText(code.toString());
   	Message.show("The code was obfuscated and the result copied to the clipboard.", "Obfuscate code", shell);
	}
	
	private void commandStrucFrequencyToClip() {
   	Obfuscator obfuscator = Obfuscator.createFor(codeDisplay.getCodeLanguage(), true, false, true, true, true, true);
   	Code code;
   	try {
   		code = obfuscator.obfuscate(resultCode.toString());
		} catch (ParseException | UnexpectedSyntaxAfterChanges e) {
			Message.show(e.getMessage(), shell);
			return;
		}
   	
   	HashMap<String, Integer> freqOfTexts = new HashMap<>();
   	Command command = code.firstCommand;
   	while (command != null) {
   		String text = command.toStringForErrorMessage(true);
   		if (!freqOfTexts.containsKey(text)) {
   			freqOfTexts.put(text, 1);
   		} else {
   			freqOfTexts.put(text, freqOfTexts.get(text) + 1);
   		}
   		command = command.getNext();
   	}
   	
   	StringBuilder sb = new StringBuilder();
   	for (String text : freqOfTexts.keySet()) {
   		int count = freqOfTexts.get(text);
   		sb.append(String.valueOf(count)).append('\t').append(text).append(System.lineSeparator());
   	}
   	SystemClipboard.setText(sb.toString());
   	String summary = Cult.format(freqOfTexts.size()) + " different structures were found in " + Cult.format(code.commandCount) + " commands.";
   	Message.show(summary + " The result was copied to the clipboard.", "frequency list of structure-identical commands", shell);
	}

	private static String getProfileFallbackMessage(String oldProfileName, String fallbackProfileName, StringBuilder errorMessages) {
		String msg = "Profile '" + oldProfileName + "' was not found anymore.";
		msg += System.lineSeparator() + "Profile '" + fallbackProfileName + "' was selected instead.";
		if (errorMessages.length() > 0) {
			msg += System.lineSeparator() + System.lineSeparator() + errorMessages.toString(); 
			// msg += System.lineSeparator() + System.lineSeparator() + "Please update " + Program.PRODUCT_NAME + "."; 
		}
		return msg;
	}
}
