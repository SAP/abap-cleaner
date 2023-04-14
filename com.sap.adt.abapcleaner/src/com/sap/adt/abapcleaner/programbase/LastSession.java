package com.sap.adt.abapcleaner.programbase;

import java.io.IOException;

import com.sap.adt.abapcleaner.base.*;

public class LastSession {
	private static final int REQUIRED_VERSION = 1;

	private static final String KEY_SOURCE_NAME = "sourceName";
	private static final String KEY_SOURCE_PATH = "sourcePath";
	private static final String KEY_CODE_TEXT = "codeText";
	private static final String KEY_TOP_LINE_INDEX = "topLineIndex";
	private static final String KEY_CUR_LINE_INDEX = "curLineIndex";
	private static final String KEY_SELECTION_START_LINE = "selectionStartLine";
	private static final String KEY_ABAP_RELEASE = "abapRelease";
	
	// -------------------------------------------------------------------------

	private String sourceName;
	private String sourcePath;
	private String codeText;
	private String abapRelease;
	private int topLineIndex;
	private int curLineIndex;
	private int selectionStartLine;

	public final String getSourceName() { return sourceName; }

	public final String getSourcePath() { return sourcePath; }

	public final String getCodeText() { return codeText; }

	public final String getAbapRelease() { return abapRelease; }

	public final int getTopLineIndex() { return topLineIndex; }

	public final int getCurLineIndex() { return curLineIndex; }

	public final int getSelectionStartLine() { return selectionStartLine; }

	// -------------------------------------------------------------------------
	
	public static LastSession createEmpty() {
		return new LastSession();
	}
	
	public static LastSession create(String sourceName, String sourcePath, String codeText, String abapRelease, int topLineIndex, int curLineIndex, int selectionStartLine) {
		return new LastSession(sourceName, sourcePath, codeText, abapRelease, topLineIndex, curLineIndex, selectionStartLine);
	}

	private LastSession() {
	}

	private LastSession(String sourceName, String sourcePath, String codeText, String abapRelease, int topLineIndex, int curLineIndex, int selectionStartLine) {
		boolean isCodeEmpty = StringUtil.isNullOrEmpty(codeText);
		this.sourceName = sourceName;
		this.sourcePath = sourcePath;
		this.codeText = codeText;
		this.abapRelease = (abapRelease == null) ? "" : abapRelease;
		this.topLineIndex = isCodeEmpty ? 0 : topLineIndex;
		this.curLineIndex = isCodeEmpty ? 0 : curLineIndex;
		this.selectionStartLine = isCodeEmpty ? 0 : selectionStartLine;
	}

	public final void save() {
		Persistency persistency = Persistency.get(); 
		String path = persistency.getSavePath(FileType.LAST_SESSION_TEXT);
		try (ISettingsWriter writer = TextSettingsWriter.createForFile(persistency, path, Program.TECHNICAL_VERSION, REQUIRED_VERSION)) {
			save(writer);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private final void save(ISettingsWriter writer) throws IOException {
		writer.write(KEY_SOURCE_NAME, sourceName != null ? sourceName : "");
		writer.write(KEY_SOURCE_PATH, sourcePath != null ? sourcePath : "");
		writer.write(KEY_CODE_TEXT, codeText != null ? codeText : "");
		writer.write(KEY_TOP_LINE_INDEX, topLineIndex);
		writer.write(KEY_CUR_LINE_INDEX, curLineIndex);
		writer.write(KEY_SELECTION_START_LINE, selectionStartLine);
		writer.write(KEY_ABAP_RELEASE, abapRelease);
	}

	public void load() {
		Persistency persistency = Persistency.get();
		String path = persistency.getLoadPath(FileType.LAST_SESSION_TEXT);
		if (path != null && persistency.fileExists(path)) {
			try {
				try (ISettingsReader reader = TextSettingsReader.createFromFile(persistency, path, Program.TECHNICAL_VERSION)) {
					load(reader);
				}
			} catch (IOException e) {
			}
		}
	}

	private final void load(ISettingsReader reader) throws IOException {
		sourceName = reader.readString(KEY_SOURCE_NAME);
		sourcePath = reader.readString(KEY_SOURCE_PATH);
		codeText = reader.readString(KEY_CODE_TEXT);
		topLineIndex = reader.readInt32(KEY_TOP_LINE_INDEX);
		curLineIndex = reader.readInt32(KEY_CUR_LINE_INDEX);
		selectionStartLine = reader.readInt32(KEY_SELECTION_START_LINE);
		if (reader.getFileVersion() >= 12) {
			abapRelease = reader.readString(KEY_ABAP_RELEASE);
		} else {
			abapRelease = ABAP.NEWEST_RELEASE;
		}
	}
	
	public final void reloadCodeTextFromCodePath() {
		if (StringUtil.isNullOrEmpty(sourcePath))
			return;

		// if the code file still exists, read its current content, otherwise use the codeText that was saved from the last session
		Persistency persistency = Persistency.get();
		if (persistency.fileExists(sourcePath))
			codeText = persistency.readAllTextFromFile(sourcePath);
	}
}
