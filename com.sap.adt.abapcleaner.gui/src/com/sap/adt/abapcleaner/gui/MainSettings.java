package com.sap.adt.abapcleaner.gui;

import java.io.IOException;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.ISettingsReader;
import com.sap.adt.abapcleaner.base.ISettingsWriter;
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
	
	// -------------------------------------------------------------------------

	String curProfileName;
	CleanupRangeExpandMode cleanupRangeExpandMode;
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
	boolean highlightDeclarationKeywords;
	boolean highlightWritePositions;
	boolean showVerticalLine;
	int verticalLinePos;
	
	boolean shellMaximized;
	int shellLeft;
	int shellTop;
	int shellWidth;
	int shellHeight;

	int releaseRestriction;
	
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
	
	void save() {
		Persistency persistency = Persistency.get();
		String path = persistency.getSavePath(FileType.SETTINGS_MAIN_TEXT);
		try (ISettingsWriter writer = TextSettingsWriter.createForFile(persistency, path, Program.TECHNICAL_VERSION, REQUIRED_VERSION)) {
			save(writer);
		} catch (IOException e) {
		}
	}

	private void save(ISettingsWriter writer) throws IOException {
		writer.write(KEY_CUR_PROFILE_NAME, curProfileName);

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

		writer.write(KEY_RELEASE_RESTRICTION, releaseRestriction);
		
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
		
		writer.write(KEY_CLEANUP_RANGE_EXPAND_MODE, cleanupRangeExpandMode.getValue());
		
		writer.write(KEY_READ_ONLY_PROFILE_DIR_COUNT, readOnlyProfileDirs.size());
		for (int i = 0; i < readOnlyProfileDirs.size(); ++i) {
			String indexSuffix = String.valueOf(i);
			ProfileDir profileDir = readOnlyProfileDirs.get(i);
			writer.write(KEY_READ_ONLY_PROFILE_NAME_ + indexSuffix, profileDir.shortName);
			writer.write(KEY_READ_ONLY_PROFILE_DIR_ + indexSuffix, profileDir.readOnlyDir);
		}
	}

	void load() {
		Persistency persistency = Persistency.get();
		String path = persistency.getLoadPath(FileType.SETTINGS_MAIN_TEXT);
		if (path == null || !persistency.fileExists(path))
			return;

		try {
			try (ISettingsReader reader = TextSettingsReader.createFromFile(persistency, path, Program.TECHNICAL_VERSION)) {
				load(reader);
			}
		} catch (java.lang.Exception e) {
			setDefault();
		}
	}
	
	private void load(ISettingsReader reader) throws IOException {
		curProfileName = reader.readString(KEY_CUR_PROFILE_NAME);

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

		if (reader.getFileVersion() >= 22) {
			try {
				cleanupRangeExpandMode = CleanupRangeExpandMode.forValue(reader.readInt32(KEY_CLEANUP_RANGE_EXPAND_MODE));
			} catch (IllegalArgumentException e) {
				cleanupRangeExpandMode = CleanupRangeExpandMode.getDefault();
			}
		} else {
			cleanupRangeExpandMode = CleanupRangeExpandMode.getDefault();
		}
		
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
	}

	void setDefault() {
		curProfileName = Profile.DEFAULT_NAME;
		cleanupRangeExpandMode = CleanupRangeExpandMode.getDefault();

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
		highlightDeclarationKeywords = false;
		highlightWritePositions = false;
		showVerticalLine = true;
		verticalLinePos = 120;

		// set shell to maximized, but otherwise unspecified shell size
		shellMaximized = true;
		setShellBoundsUnspecified();

		releaseRestriction = ABAP.NO_RELEASE_RESTRICTION;
		
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
