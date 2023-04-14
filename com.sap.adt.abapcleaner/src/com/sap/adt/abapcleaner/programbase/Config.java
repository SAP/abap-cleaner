package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.*;

import java.io.IOException;
import java.util.*;

/**
 * <p>Encapsulates configuration settings that are loaded at program start.</p>
 * 
 * <p>Specifically, 'folder priorities' may be configured, meaning that when a file is to be loaded, 
 * the program can search several folders for this file, following the order given by the 'folder priorities'. 
 * For instance, if folder priorities are set to the folders "user, team, area", 
 * then user-specific files in folder 'user' can 'shadow' underlying (fallback-)files in folder 'team', 
 * which in turn shadow underlying files in folder 'area'.</p>
 * 
 * <p>Folder priorities are currently not used in ABAP cleaner, but may be useful at a later point in time.</p>    
 */
class Config {
	private static final int REQUIRED_VERSION = 16;
	private static final String DEFAULT_FOLDER = ""; // currently unused; may be set to a folder name with default settings
	private static final String USER_FOLDER = "user";

	private static final String FOLDER_SEPARATOR = ",";

	private static final String KEY_FOLDER_PRIO_DEFAULT = "folderPriority";
	private static final String KEY_FOLDER_PRIO_SETTINGS = "folderPrioritySettings";

	// ----------------------------------------------------------------------

	private final String workDir;
	private ArrayList<String> folderPrioritiesDefault = new ArrayList<String>();
	private ArrayList<String> folderPrioritiesSettings = new ArrayList<String>();

	final ArrayList<String> getFolderPrioritiesSettings() { return folderPrioritiesSettings; }

	Config(String workDir) {
		this.workDir = workDir;
		setDefault();
	}

	private void setDefault() {
		folderPrioritiesDefault.clear();
		folderPrioritiesDefault.add(USER_FOLDER); // whether or not it exists

		if (!StringUtil.isNullOrEmpty(DEFAULT_FOLDER) && Persistency.get().directoryExists(workDir, DEFAULT_FOLDER)) {
			folderPrioritiesDefault.add(DEFAULT_FOLDER); 
		}

		folderPrioritiesSettings = new ArrayList<String>(folderPrioritiesDefault);
	}

	final void save(Persistency persistency, String path) {
		persistency.ensureDirectoryExistsForPath(path);

		try (ISettingsWriter writer = TextSettingsWriter.createForFile(persistency, path, Program.TECHNICAL_VERSION, REQUIRED_VERSION)){
			writer.write(KEY_FOLDER_PRIO_DEFAULT, getFolderPrioLine(folderPrioritiesDefault));
			writer.write(KEY_FOLDER_PRIO_SETTINGS, getFolderPrioLine(folderPrioritiesSettings));
		} catch (IOException ex) {
		}
	}

	final void load(Persistency persistency, String path) {
		if (!persistency.fileExists(path)) {
			setDefault(); // in case some values are not being read
			return;
		}

		folderPrioritiesDefault.clear();
		folderPrioritiesSettings.clear();

		try (ISettingsReader reader = TextSettingsReader.createFromFile(persistency, path, REQUIRED_VERSION)) {
			folderPrioritiesDefault = readFolderPrios(reader.readString(KEY_FOLDER_PRIO_DEFAULT));
			folderPrioritiesSettings = readFolderPrios(reader.readString(KEY_FOLDER_PRIO_SETTINGS));
		} catch (IOException ex) {
		}

		if (folderPrioritiesDefault.isEmpty()) {
			setDefault();
		} else if (folderPrioritiesSettings.isEmpty()) {
			folderPrioritiesSettings = folderPrioritiesDefault;
		}
	}

	private static String getFolderPrioLine(ArrayList<String> folderPrios) {
		StringBuilder line = new StringBuilder();
		boolean isFirst = true;
		for (String folderName : folderPrios) {
			if (!isFirst)
				line.append(FOLDER_SEPARATOR + " ");
			line.append(folderName);
			isFirst = false;
		}
		return line.toString();
	}

	private static ArrayList<String> readFolderPrios(String value) {
		ArrayList<String> folderPriorities = new ArrayList<String>();
		String[] folders = StringUtil.split(value, FOLDER_SEPARATOR, true);
		for (String folder : folders)
			folderPriorities.add(folder.trim());
		return folderPriorities;
	}
}