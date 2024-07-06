package com.sap.adt.abapcleaner.gui;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.ISettingsReader;
import com.sap.adt.abapcleaner.base.ISettingsWriter;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.base.TextSettingsReader;
import com.sap.adt.abapcleaner.base.TextSettingsWriter;
import com.sap.adt.abapcleaner.parser.CleanupRangeExpandMode;
import com.sap.adt.abapcleaner.programbase.FileType;
import com.sap.adt.abapcleaner.programbase.Persistency;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.ProfileDir;
import com.sap.adt.abapcleaner.rulebase.RuleID;

public class MainSettings {
	private final static int REQUIRED_VERSION = 1;

	private static final String KEY_CUR_PROFILE_NAME = "curProfileName";
	private static final String KEY_CLEANUP_RANGE_EXPAND_MODE = "cleanupRangeExpandMode";
	private static final String KEY_HIGHLIGHT_INDENT_CHANGES = "highlightIndentChanges";
	private static final String KEY_HIGHLIGHT_INNER_SPACE_CHANGES = "highlightInnerSpaceChanges";
	private static final String KEY_HIGHLIGHT_CASE_CHANGES = "highlightCaseChanges";
	private static final String KEY_HIGHLIGHT_CONTENT_CHANGES = "highlightContentChanges";

	private static final String KEY_SEARCH_LEFT_DISPLAY = "searchLeftDisplay";
	private static final String KEY_SEARCH_RIGHT_DISPLAY = "searchRightDisplay";
	private static final String KEY_SEARCH_CHANGED_LINES = "searchChangedLines";
	private static final String KEY_MATCH_CASE = "matchCase";
	private static final String KEY_MATCH_WHOLE_WORD = "matchWholeWord";

	private static final String KEY_CODE_FONT_SIZE = "codeFontSize";
	private static final String KEY_COLOR_PROFILE = "colorProfile";
	private static final String KEY_HIGHLIGHT_DECLARATION_KEYWORDS = "highlightDeclarationKeywords";
	private static final String KEY_HIGHLIGHT_WRITE_POS = "highlightWritePositions";
	private static final String KEY_SHOW_VERTICAL_LINE = "showVerticalLine";
	private static final String KEY_VERTICAL_LINE_POS = "verticalLinePos";
	
	private static final String KEY_SHELL_MAXIMIZED = "shellMaximized";
	private static final String KEY_SHELL_LEFT = "shellLeft";
	private static final String KEY_SHELL_TOP = "shellTop";
	private static final String KEY_SHELL_WIDTH = "shellWidth";
	private static final String KEY_SHELL_HEIGHT = "shellHeight";

	private static final String KEY_RELEASE_RESTRICTION = "releaseRestriction";
	
	private static final String KEY_PROFILES_HIGHLIGHT_RELEASE_INDEX = "profilesHighlightReleaseIndex";
	private static final String KEY_PROFILES_HIGHLIGHT_RELEASE_VERSION = "profilesHighlightReleaseVersion";
	private static final String KEY_PROFILES_LAST_RULE_ID = "profilesLastRuleID";
	private static final String KEY_PROFILES_LAST_EXPORT_DIR = "profilesLastExportDir";
	private static final String KEY_PROFILES_LAST_IMPORT_DIR = "profilesLastImportDir";
	private static final String KEY_PROFILES_HIGHLIGHT_ITEM = "profilesHighlightItem";
	private static final String KEY_PROFILES_HIGHLIGHT_DECLARATION_KEYWORDS = "profilesHighlightDeclarationKeywords";
	private static final String KEY_PROFILES_HIGHLIGHT_WRITE_POS = "profilesHighlightWritePositions";

	private static final String KEY_ESSENTIAL_PROFILE_CREATED = "wasEssentialProfileCreated";
	private static final String KEY_PROFILES_DIRECTORY = "profilesDirectory";
	private static final String KEY_READ_ONLY_PROFILE_DIR_COUNT = "readOnlyProfileDirCount";
	private static final String KEY_READ_ONLY_PROFILE_NAME_ = "readOnlyProfileName";
	private static final String KEY_READ_ONLY_PROFILE_DIR_ = "readOnlyProfileDir";

	// workspace-specific cleanup settings
	private static final String KEY_WORKSPACE_COUNT = "workspaceCount";
	private static final String KEY_WORKSPACE_DIR_ = "workspaceDir";
	private static final String KEY_LAST_PROFILE_NAME_ = "lastProfileName";
	private static final String KEY_CLEANUP_RANGE_EXPAND_MODE_ = "cleanupRangeExpandMode";
	private static final String KEY_RELEASE_RESTRICTION_ = "releaseRestriction";

	// -------------------------------------------------------------------------

	private class CleanupSettings {
		private String lastProfileName;
		private CleanupRangeExpandMode cleanupRangeExpandMode;
		private int releaseRestriction;
		
		public CleanupSettings() {
			this.lastProfileName = Profile.DEFAULT_NAME;
			this.cleanupRangeExpandMode = CleanupRangeExpandMode.getDefault();
			this.releaseRestriction = ABAP.NO_RELEASE_RESTRICTION;
		}

		public CleanupSettings(CleanupSettings template) {
			this.lastProfileName = template.lastProfileName;
			this.cleanupRangeExpandMode = template.cleanupRangeExpandMode;
			this.releaseRestriction = template.releaseRestriction;
		}

		public CleanupSettings(String lastProfileName, CleanupRangeExpandMode cleanupRangeExpandMode, int releaseRestriction) {
			this.lastProfileName = lastProfileName;
			this.cleanupRangeExpandMode = cleanupRangeExpandMode;
			this.releaseRestriction = releaseRestriction;
		}
	}

	private String workspaceDir;
	private CleanupSettings fallbackCleanupSettings = new CleanupSettings();
	HashMap<String, CleanupSettings> cleanupSettingsOfWorkspace = new HashMap<>();
	
	boolean highlightIndentChanges;
	boolean highlightInnerSpaceChanges;
	boolean highlightCaseChanges;
	boolean highlightContentChanges;

	boolean searchLeftDisplay;
	boolean searchRightDisplay;
	boolean searchChangedLines;
	boolean matchCase;
	boolean matchWholeWord;

	float codeFontSize;
	ColorProfile colorProfile;
	boolean highlightDeclarationKeywords;
	boolean highlightWritePositions;
	boolean showVerticalLine;
	int verticalLinePos;
	
	boolean shellMaximized;
	int shellLeft;
	int shellTop;
	int shellWidth;
	int shellHeight;

	// settings for FrmProfiles:
	RuleID profilesLastRuleID;
	String profilesLastExportDir;
	String profilesLastImportDir;
	String profilesHighlightItem;
	boolean profilesHighlightDeclarationKeywords;
	boolean profilesHighlightWritePositions;
	
	boolean wasEssentialProfileCreated;
	String profilesDirectory;
	ArrayList<ProfileDir> readOnlyProfileDirs = new ArrayList<>();
	
	int getShellRight() { return shellLeft + shellWidth; }
	int getShellBottom() { return shellTop + shellHeight; }

	// -------------------------------------------------------------------------

	/***
	 * 
	 * @param workspaceDir - the directory of the Eclipse workspace or the stand-alone application .exe
	 * (if unknown during instantiation, it can be supplied later with .initialize(); otherwise, the fallback cleanup settings will be used)
	 */
	MainSettings(String workspaceDir) {
		this.workspaceDir = workspaceDir;
	}

	void initialize(String workspaceDir) {
		this.workspaceDir = workspaceDir;
	}

	void save() {
		Persistency persistency = Persistency.get();
		String path = persistency.getSavePath(FileType.SETTINGS_MAIN_TEXT);
		try (ISettingsWriter writer = TextSettingsWriter.createForFile(persistency, path, Program.TECHNICAL_VERSION, REQUIRED_VERSION)) {
			save(writer);
		} catch (IOException e) {
		}
	}

	private void save(ISettingsWriter writer) throws IOException {
		writer.write(KEY_CUR_PROFILE_NAME, fallbackCleanupSettings.lastProfileName);

		writer.write(KEY_HIGHLIGHT_INDENT_CHANGES, highlightIndentChanges);
		writer.write(KEY_HIGHLIGHT_INNER_SPACE_CHANGES, highlightInnerSpaceChanges);
		writer.write(KEY_HIGHLIGHT_CASE_CHANGES, highlightCaseChanges);
		writer.write(KEY_HIGHLIGHT_CONTENT_CHANGES, highlightContentChanges);

		writer.write(KEY_SEARCH_LEFT_DISPLAY, searchLeftDisplay);
		writer.write(KEY_SEARCH_RIGHT_DISPLAY, searchRightDisplay);
		writer.write(KEY_SEARCH_CHANGED_LINES, searchChangedLines);
		writer.write(KEY_MATCH_CASE, matchCase);
		writer.write(KEY_MATCH_WHOLE_WORD, matchWholeWord);

		writer.write(KEY_CODE_FONT_SIZE, codeFontSize);
		writer.write(KEY_SHOW_VERTICAL_LINE, showVerticalLine);
		writer.write(KEY_VERTICAL_LINE_POS, verticalLinePos);

		writer.write(KEY_SHELL_MAXIMIZED, shellMaximized);
		writer.write(KEY_SHELL_LEFT, shellLeft);
		writer.write(KEY_SHELL_TOP, shellTop);
		writer.write(KEY_SHELL_WIDTH, shellWidth);
		writer.write(KEY_SHELL_HEIGHT, shellHeight);

		writer.write(KEY_RELEASE_RESTRICTION, fallbackCleanupSettings.releaseRestriction);
		
		// obsolete profilesHighlightRelease... information:
		writer.write(KEY_PROFILES_HIGHLIGHT_RELEASE_INDEX, -1);
		writer.write(KEY_PROFILES_HIGHLIGHT_RELEASE_VERSION, "");
		
		writer.write(KEY_PROFILES_LAST_RULE_ID, profilesLastRuleID == null ? "" : profilesLastRuleID.toString());

		writer.write(KEY_PROFILES_LAST_EXPORT_DIR, profilesLastExportDir == null ? "" : profilesLastExportDir); 
		writer.write(KEY_PROFILES_LAST_IMPORT_DIR, profilesLastImportDir == null ? "" : profilesLastImportDir); 

		writer.write(KEY_PROFILES_HIGHLIGHT_ITEM, profilesHighlightItem == null ? "" : profilesHighlightItem);
		
		writer.write(KEY_HIGHLIGHT_WRITE_POS, highlightWritePositions);
		writer.write(KEY_PROFILES_HIGHLIGHT_WRITE_POS, profilesHighlightWritePositions);
		writer.write(KEY_ESSENTIAL_PROFILE_CREATED, wasEssentialProfileCreated);
		writer.write(KEY_PROFILES_DIRECTORY, profilesDirectory);

		writer.write(KEY_HIGHLIGHT_DECLARATION_KEYWORDS, highlightDeclarationKeywords);
		writer.write(KEY_PROFILES_HIGHLIGHT_DECLARATION_KEYWORDS, profilesHighlightDeclarationKeywords);
		
		writer.write(KEY_CLEANUP_RANGE_EXPAND_MODE, fallbackCleanupSettings.cleanupRangeExpandMode.getValue());
		
		writer.write(KEY_READ_ONLY_PROFILE_DIR_COUNT, readOnlyProfileDirs.size());
		for (int i = 0; i < readOnlyProfileDirs.size(); ++i) {
			String indexSuffix = String.valueOf(i);
			ProfileDir profileDir = readOnlyProfileDirs.get(i);
			writer.write(KEY_READ_ONLY_PROFILE_NAME_ + indexSuffix, profileDir.shortName);
			writer.write(KEY_READ_ONLY_PROFILE_DIR_ + indexSuffix, profileDir.readOnlyDir);
		}
		
		// save workspace-specific cleanup settings
		writer.write(KEY_WORKSPACE_COUNT, cleanupSettingsOfWorkspace.size());
		int index = 0;
		for (String workspaceDir : cleanupSettingsOfWorkspace.keySet()) {
			String indexSuffix = String.valueOf(index);
			CleanupSettings cleanupSettings = cleanupSettingsOfWorkspace.get(workspaceDir);
			writer.write(KEY_WORKSPACE_DIR_ + indexSuffix, workspaceDir);
			writer.write(KEY_LAST_PROFILE_NAME_ + indexSuffix, cleanupSettings.lastProfileName);
			writer.write(KEY_CLEANUP_RANGE_EXPAND_MODE_ + indexSuffix, cleanupSettings.cleanupRangeExpandMode.getValue());
			writer.write(KEY_RELEASE_RESTRICTION_ + indexSuffix, cleanupSettings.releaseRestriction);
			++index;
		}

		writer.write(KEY_COLOR_PROFILE, colorProfile.getValue());
	}

	void load() {
		Persistency persistency = Persistency.get();
		String path = persistency.getLoadPath(FileType.SETTINGS_MAIN_TEXT);
		if (path == null || !persistency.fileExists(path)) {
			setDefault();
			return;
		}

		try {
			try (ISettingsReader reader = TextSettingsReader.createFromFile(persistency, path, Program.TECHNICAL_VERSION)) {
				load(reader);
			}
		} catch (java.lang.Exception e) {
			setDefault();
		}
	}
	
	private void load(ISettingsReader reader) throws IOException {
		// last profile name for fallbackCleanupSettings (see below)
		String lastProfileName = reader.readString(KEY_CUR_PROFILE_NAME);

		highlightIndentChanges = reader.readBool(KEY_HIGHLIGHT_INDENT_CHANGES);
		highlightInnerSpaceChanges = reader.readBool(KEY_HIGHLIGHT_INNER_SPACE_CHANGES);
		highlightCaseChanges = reader.readBool(KEY_HIGHLIGHT_CASE_CHANGES);
		highlightContentChanges = reader.readBool(KEY_HIGHLIGHT_CONTENT_CHANGES);

		searchLeftDisplay = reader.readBool(KEY_SEARCH_LEFT_DISPLAY);
		searchRightDisplay = reader.readBool(KEY_SEARCH_RIGHT_DISPLAY);
		searchChangedLines = reader.readBool(KEY_SEARCH_CHANGED_LINES);
		matchCase = reader.readBool(KEY_MATCH_CASE);
		matchWholeWord = reader.readBool(KEY_MATCH_WHOLE_WORD);

		codeFontSize = reader.readFloat(KEY_CODE_FONT_SIZE);
		showVerticalLine = reader.readBool(KEY_SHOW_VERTICAL_LINE);
		verticalLinePos = reader.readInt32(KEY_VERTICAL_LINE_POS);

		if (reader.getFileVersion() >= 11) {
			shellMaximized = reader.readBool(KEY_SHELL_MAXIMIZED);
			shellLeft = reader.readInt32(KEY_SHELL_LEFT);
			shellTop = reader.readInt32(KEY_SHELL_TOP);
			shellWidth = reader.readInt32(KEY_SHELL_WIDTH);
			shellHeight = reader.readInt32(KEY_SHELL_HEIGHT);
		} else {
			setShellBoundsUnspecified();
		}
		
		// release restriction for fallbackCleanupSettings (see below)
		int releaseRestriction;
		if (reader.getFileVersion() >= 13) {
			releaseRestriction = reader.readInt32(KEY_RELEASE_RESTRICTION);
		} else {
			releaseRestriction = ABAP.NO_RELEASE_RESTRICTION;
		}
		
		// profilesHighlightRelease... information is only used to upgrade to profilesHighlightIndex/Item 
		String profilesHighlightReleaseVersion = ""; 
		if (reader.getFileVersion() >= 15) {
			reader.readInt32(KEY_PROFILES_HIGHLIGHT_RELEASE_INDEX); // ignore value
			profilesHighlightReleaseVersion = reader.readString(KEY_PROFILES_HIGHLIGHT_RELEASE_VERSION);
		}

		if (reader.getFileVersion() >= 15) {
			try {
				profilesLastRuleID = RuleID.valueOf(reader.readString(KEY_PROFILES_LAST_RULE_ID));
			} catch (IllegalArgumentException e) {
				// RuleID strings may have changed
				profilesLastRuleID = RuleID.forValue(0);
			}
		} else {
			profilesLastRuleID = RuleID.forValue(0);
		}

		if (reader.getFileVersion() >= 17) {
			profilesLastExportDir = reader.readString(KEY_PROFILES_LAST_EXPORT_DIR); 
			profilesLastImportDir = reader.readString(KEY_PROFILES_LAST_IMPORT_DIR); 
		} else {
			profilesLastExportDir = ""; 
			profilesLastImportDir = ""; 
		}
		
		if (reader.getFileVersion() >= 19) {
			profilesHighlightItem = reader.readString(KEY_PROFILES_HIGHLIGHT_ITEM);
		} else {
			profilesHighlightItem = ProfileHighlightItem.getPersistentStringOfRelease(profilesHighlightReleaseVersion);
		}

		if (reader.getFileVersion() >= 20) {
			highlightWritePositions = reader.readBool(KEY_HIGHLIGHT_WRITE_POS);
			profilesHighlightWritePositions = reader.readBool(KEY_PROFILES_HIGHLIGHT_WRITE_POS);
			wasEssentialProfileCreated = reader.readBool(KEY_ESSENTIAL_PROFILE_CREATED);
			profilesDirectory = reader.readString(KEY_PROFILES_DIRECTORY);
		} else {
			highlightWritePositions = false;
			profilesHighlightWritePositions = false;
			wasEssentialProfileCreated = false;
			profilesDirectory = "";
		}

		if (reader.getFileVersion() >= 21) {
			highlightDeclarationKeywords = reader.readBool(KEY_HIGHLIGHT_DECLARATION_KEYWORDS);
			profilesHighlightDeclarationKeywords = reader.readBool(KEY_PROFILES_HIGHLIGHT_DECLARATION_KEYWORDS);
		} else {
			highlightDeclarationKeywords = false;
			profilesHighlightDeclarationKeywords = true;
		}

		// cleanup range expand mode for fallbackCleanupSettings (see below)
		CleanupRangeExpandMode cleanupRangeExpandMode;
		if (reader.getFileVersion() >= 22) {
			try {
				cleanupRangeExpandMode = CleanupRangeExpandMode.forValue(reader.readInt32(KEY_CLEANUP_RANGE_EXPAND_MODE));
			} catch (IllegalArgumentException e) {
				cleanupRangeExpandMode = CleanupRangeExpandMode.getDefault();
			}
		} else {
			cleanupRangeExpandMode = CleanupRangeExpandMode.getDefault();
		}
		fallbackCleanupSettings = new CleanupSettings(lastProfileName, cleanupRangeExpandMode, releaseRestriction);
		
		readOnlyProfileDirs.clear();		
		if (reader.getFileVersion() >= 23) {
			int readOnlyProfileDirCount = reader.readInt32(KEY_READ_ONLY_PROFILE_DIR_COUNT);
			for (int i = 0; i < readOnlyProfileDirCount ; ++i) {
				String indexSuffix = String.valueOf(i);
				String shortName = reader.readString(KEY_READ_ONLY_PROFILE_NAME_ + indexSuffix);
				String readOnlyDir = reader.readString(KEY_READ_ONLY_PROFILE_DIR_ + indexSuffix);
				readOnlyProfileDirs.add(new ProfileDir(shortName, readOnlyDir));
			}
		}

		// workspace-specific CleanupSettings
		cleanupSettingsOfWorkspace.clear();
		if (reader.getFileVersion() >= 25) {
			int workspaceCount = reader.readInt32(KEY_WORKSPACE_COUNT);
			for (int i = 0; i < workspaceCount; ++i) {
				String indexSuffix = String.valueOf(i);
				String workspaceDir = reader.readString(KEY_WORKSPACE_DIR_ + indexSuffix);
				lastProfileName = reader.readString(KEY_LAST_PROFILE_NAME_ + indexSuffix);
				try {
					cleanupRangeExpandMode = CleanupRangeExpandMode.forValue(reader.readInt32(KEY_CLEANUP_RANGE_EXPAND_MODE_ + indexSuffix));
				} catch (IllegalArgumentException e) {
					cleanupRangeExpandMode = CleanupRangeExpandMode.getDefault();
				}
				releaseRestriction = reader.readInt32(KEY_RELEASE_RESTRICTION_ + indexSuffix);
				cleanupSettingsOfWorkspace.put(workspaceDir, new CleanupSettings(lastProfileName, cleanupRangeExpandMode, releaseRestriction));
			}
		}
		
		if (reader.getFileVersion() >= 26) {
			try {
				colorProfile = ColorProfile.forValue(reader.readInt32(KEY_COLOR_PROFILE));
			} catch (IllegalArgumentException e) {
				colorProfile = ColorProfile.getDefault();
			}
		} else {
			colorProfile = ColorProfile.getDefault();
		}
	}

	void setDefault() {
		fallbackCleanupSettings = new CleanupSettings(Profile.DEFAULT_NAME, CleanupRangeExpandMode.getDefault(), ABAP.NO_RELEASE_RESTRICTION);
		cleanupSettingsOfWorkspace.clear();

		highlightIndentChanges = true;
		highlightInnerSpaceChanges = true;
		highlightCaseChanges = true;
		highlightContentChanges = true;

		searchLeftDisplay = false;
		searchRightDisplay = true;
		searchChangedLines = false;
		matchCase = false;
		matchWholeWord = false;

		codeFontSize = CodeDisplay.DEFAULT_FONT_SIZE;
		colorProfile = ColorProfile.getDefault();
		highlightDeclarationKeywords = false;
		highlightWritePositions = false;
		showVerticalLine = true;
		verticalLinePos = 120;

		// set shell to maximized, but otherwise unspecified shell size
		shellMaximized = true;
		setShellBoundsUnspecified();

		// obsolete:
		// profilesHighlightReleaseIndex = -1;
		// profilesHighlightReleaseVersion = "";
		
		profilesLastRuleID = RuleID.forValue(0);
		profilesLastExportDir = ""; 
		profilesLastImportDir = ""; 
		profilesHighlightItem = "";
		profilesHighlightDeclarationKeywords = true;
		profilesHighlightWritePositions = false;
		
		wasEssentialProfileCreated = false;
		profilesDirectory = "";
		readOnlyProfileDirs.clear();		
	}
	
	/*** returns the workspace-specific cleanup settings; if missing, either creates them, or returns the fallback cleanup settings (depending on createIfMissing) */
	private CleanupSettings getCleanupSettings(boolean createIfMissing) {
		if (StringUtil.isNullOrEmpty(workspaceDir))
			return fallbackCleanupSettings;
		CleanupSettings cleanupSettings = cleanupSettingsOfWorkspace.get(workspaceDir);
		if (cleanupSettings == null) {
			if (createIfMissing) {
				cleanupSettings = new CleanupSettings(fallbackCleanupSettings);
				cleanupSettingsOfWorkspace.put(workspaceDir, cleanupSettings);
			} else {
				cleanupSettings = fallbackCleanupSettings;
			}
		}
		return cleanupSettings;
	}
	
	String getLastProfileName() {
		return getCleanupSettings(false).lastProfileName;
	}
	
	CleanupRangeExpandMode getCleanupRangeExpandMode() {
		return getCleanupSettings(false).cleanupRangeExpandMode;
	}
	
	int getReleaseRestriction() {
		return getCleanupSettings(false).releaseRestriction;
	}
	
	void setLastProfileName(String lastProfileName) {
		CleanupSettings cleanupSettings = getCleanupSettings(true);
		cleanupSettings.lastProfileName = lastProfileName;
		fallbackCleanupSettings.lastProfileName = lastProfileName;
	}
	
	void setCleanupRangeExpandMode(CleanupRangeExpandMode cleanupRangeExpandMode) {
		CleanupSettings cleanupSettings = getCleanupSettings(true);
		cleanupSettings.cleanupRangeExpandMode = cleanupRangeExpandMode;
		fallbackCleanupSettings.cleanupRangeExpandMode = cleanupRangeExpandMode;
	}

	void setReleaseRestriction(int releaseRestriction) {
		CleanupSettings cleanupSettings = getCleanupSettings(true);
		cleanupSettings.releaseRestriction = releaseRestriction;
		fallbackCleanupSettings.releaseRestriction = releaseRestriction;
	}

	boolean areShellBoundsSpecified() { 
		// note that shellLeft and shellTop may be negative on multi-monitor setups
		return (shellWidth > 0 || shellHeight > 0); 
	}
	
	void setShellBoundsUnspecified() {
		shellLeft = -1;
		shellTop = -1;
		shellWidth = -1;
		shellHeight = -1;
	}
}
