package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.*;
import java.util.*;

public class Persistency extends PersistencyBase {
	protected static Persistency singleInstance;

	public static Persistency get() {
		// do NOT instantiate here, thus forcing a call to Persistency.create() or, for tests, PersistencyDouble.create()
		// at the earliest possible moment wherever the Persistency is needed
		// if (singleInstance == null)
		/// 	singleInstance = new Persistency(FileSystem.create());
		return singleInstance;
	}

	static Persistency create(IFileSystem fileSystem) {
		singleInstance = new Persistency(fileSystem);
		return singleInstance;
	}

	// -------------------------------------------------------------------------
	
	private String workDir;
	private String startupPath;
	private Config config;

	protected Persistency(IFileSystem fileSystem) {
		super(fileSystem);
	}

	public final void initialize(String workDir, String startupPath) {
		this.workDir = workDir;
		this.startupPath = startupPath;
		config = new Config(workDir);
		config.load(this, combinePaths(workDir, getFileName(FileType.CONFIG_TEXT)));
	}

	public String getWorkDir() { 
		return workDir; 
	}

	public String getStartupPath() { 
		return startupPath; 
	}

	public final String getExtension(FileType fileType) {
		switch (fileType) {
			case SETTINGS_MAIN_BINARY:
			case LAST_SESSION_BINARY:
			case PROFILE_BINARY:
				return ".cfg";

			case CONFIG_TEXT:
			case SETTINGS_MAIN_TEXT:
			case LAST_SESSION_TEXT:
			case PROFILE_TEXT:
				return ".cfj";
				
			case HELP:
				return ".htm";

			case CODE:
				return ".txt";

			case ERROR_LOG:
				return ".log";

			default:
				return null;
		}
	}

	public final boolean isFileTypeForMultipleFiles(FileType fileType) {
		return getFileName(fileType).contains("*");
	}

	private String getFileName(FileType fileType) {
		String extension = getExtension(fileType);
		switch (fileType) {
			case CONFIG_TEXT:
				return "config" + extension;

			case SETTINGS_MAIN_BINARY:
			case SETTINGS_MAIN_TEXT:
				return "main" + extension;

			case LAST_SESSION_BINARY:
			case LAST_SESSION_TEXT:
				return "session" + extension;

			case PROFILE_BINARY:
			case PROFILE_TEXT:
				return "*" + extension;

			case HELP:
			case CODE:
				return "*" + extension;

			case ERROR_LOG:
				return "error" + extension;

			default:
				return null;
		}
	}

	private String getFolderName(FileType fileType) {
		switch (fileType) {
			case CONFIG_TEXT:
				return "";

			case SETTINGS_MAIN_BINARY:
			case SETTINGS_MAIN_TEXT:
			case LAST_SESSION_BINARY:
			case LAST_SESSION_TEXT:
				return "settings";

			case PROFILE_BINARY:
			case PROFILE_TEXT:
				return "profiles";

			case HELP:
				return "help";

			case CODE:
				return "code";
			
			case ERROR_LOG:
				return "";

			default:
				return null;
		}
	}

	private ArrayList<String> getBaseFoldersByPriority(FileType fileType) {
		switch (fileType) {
			case CONFIG_TEXT:
			case SETTINGS_MAIN_BINARY:
			case SETTINGS_MAIN_TEXT:
			case LAST_SESSION_BINARY:
			case LAST_SESSION_TEXT:
			case PROFILE_BINARY:
			case PROFILE_TEXT:
				return config.getFolderPrioritiesSettings();

			case HELP:
				return null;

			case CODE:
			case ERROR_LOG:
				return config.getFolderPrioritiesSettings();

			default:
				return null;
		}
	}

	/**
	 * Returns all relevant load paths for the provided file type. The paths may belong to different directories;
	 * files in a directory with higher priority shadow files of the same name in directories with lower priority.
	 */
	public final String[] getLoadPaths(FileType fileType) {
		return getLoadPaths(fileType, getExistingDirs(fileType, 0));
	}

	/**
	 * Returns all relevant load paths for the provided file type in the provided directory.
	 */
	public final String[] getLoadPaths(FileType fileType, String dir) {
		return getLoadPaths(fileType, new String[] { dir });
	}

	/**
	 * Returns all relevant load paths for the provided file type. The paths may belong to different directories;
	 * files in a directory with higher priority shadow files of the same name in directories with lower priority.
	 */
	private final String[] getLoadPaths(FileType fileType, String[] dirs) {
		String fileName = getFileName(fileType);
		if (!fileName.contains("*"))
			throw new IllegalArgumentException("Method not suitable for fileTypes with just one file name!");

		ArrayList<String> paths = new ArrayList<String>();
		HashSet<String> fileShadows = new HashSet<String>();

		for (String dir : dirs) {
			String[] pathsInDir = getFilesInDirectory(dir, fileName, false);
			if (pathsInDir == null || pathsInDir.length == 0)
				continue;

			for (String pathInFolder : pathsInDir) {
				String fileShadowKey = getFileName(pathInFolder).toUpperCase(Locale.ROOT);
				// do not add the file if it is shadowed by another file of the same name
				// (which is in a folder with a higher priority as it was processed earlier)
				if (!fileShadows.contains(fileShadowKey)) {
					paths.add(pathInFolder);
					fileShadows.add(fileShadowKey);
				}
			}
		}
		return paths.toArray(new String[0]);
	}

	/**
	  * Returns the load path for the provided file type by searching within the suitable folders in the order of their priority.
	  * (Files in a folder with higher priority shadow files of the same name in directories with lower priority).
	  * If the file is not found in any folder, null is returned.
	  */
	public final String getLoadPath(FileType fileType) {
		return getLoadPath(fileType, 0, null);
	}

	/**
	  * Returns the load path for the provided file type by searching within the suitable folders in the order of their priority.
	  * (Files in a folder with higher priority shadow files of the same name in directories with lower priority).
	  * If the file is not found in any folder, null is returned.
	  * To get a file of lower priority, set startPrio = 1
	  * (files in a folder with lower priority are considered 'fallback' files for files of the same name in directories with higher priority).
	  */
	public final String getLoadPath(FileType fileType, int startPrio, String fileBase) {
		String fileName = StringUtil.isNullOrEmpty(fileBase) ? getFileName(fileType) : fileBase + getExtension(fileType);
		if (fileName.contains("*"))
			throw new IllegalArgumentException("Method not suitable for fileTypes with multiple files!");

		String[] dirs = getExistingDirs(fileType, startPrio);

		for (String dir : dirs) {
			String path = combinePaths(dir, fileName);
			if (fileExists(path))
				return path;
		}
		return null;
	}

	public final String[] getExistingDirs(FileType fileType) {
		return getExistingDirs(fileType, 0);
	}
	public final String[] getExistingDirs(FileType fileType, int startPrio) {
		ArrayList<String> dirs = new ArrayList<String>();

		ArrayList<String> baseFolders = getBaseFoldersByPriority(fileType);
		if (baseFolders == null) 
			baseFolders = new ArrayList<String>(Arrays.asList(""));
		String subfolder = getFolderName(fileType);

		for (int prio = startPrio; prio < baseFolders.size(); ++prio) {
			String baseFolder = baseFolders.get(prio);
			String dir = getDir(baseFolder, subfolder);
			if (directoryExists(dir))
				dirs.add(dir);
		}
		return dirs.toArray(new String[0]);
	}

	/**
	 * Returns the path into which the provided file type should be saved, using the folder with highest priority (usually, the 'user' folder).
	 * If the fileType has no unique file name, the file name must be provided, too.
	 */
	public final String getSavePath(FileType fileType) {
		return getSavePath(fileType, "");
	}

	/**
	 * Returns the path into which the provided file type should be saved, using the folder with highest priority (usually, the 'user' folder).
	 */
	public final String getSavePath(FileType fileType, String fileBase) {
		// always save to the folder with the highest priority (usually the "user" folder)
		ArrayList<String> baseFolders = getBaseFoldersByPriority(fileType);
		String baseFolder = (baseFolders == null) ? "" : baseFolders.get(0);
		String subfolder = getFolderName(fileType);
		String dir = getDir(baseFolder, subfolder);

		return getSavePath(fileType, dir, fileBase);
	}

	/**
	 * Returns the path into which the provided file type should be saved, using the folder with highest priority (usually, the 'user' folder).
	 */
	public final String getSavePath(FileType fileType, String dir, String fileBase) {
		// if a filename was provided, use it
		if (!StringUtil.isNullOrEmpty(fileBase))
			return combinePaths(dir, fileBase + getExtension(fileType));

		String fileName = getFileName(fileType);
		if (fileName.contains("*"))
			throw new IllegalArgumentException("For fileTypes with multiple files, a file name must be provided!");

		return combinePaths(dir, fileName);
	}

	private String getDir(String baseFolder, String subfolder) {
		String dir = StringUtil.isNullOrEmpty(baseFolder) ? workDir : combinePaths(workDir, baseFolder);
		return StringUtil.isNullOrEmpty(subfolder) ? dir : combinePaths(dir, subfolder);
	}

	public final void ensureDirectoryExists(String dir) {
		if (!directoryExists(dir))
			createDirectory(dir);
	}

	public final void ensureDirectoryExistsForPath(String path) {
		ensureDirectoryExists(getDirectoryName(path));
	}

	public final String addDirSep(String dir) {
		String dirSep = String.valueOf(getDirectorySeparatorChar());
		if (!StringUtil.endsWith(dir, dirSep, false))
			dir += dirSep;
		return dir;
	}

	public final boolean moveFile(String sourcePath, String destPath, boolean overwriteIfExists) {
		if (!fileExists(sourcePath))
			return false;
		
		if (fileExists(destPath)) {
			if (!overwriteIfExists)
				return false;
			if (!deleteFile(destPath))
				return false;
		} else {
			ensureDirectoryExistsForPath(destPath);
		}
		
		return renameFile(sourcePath, destPath);
	}
	
	public final String getValidPath(String fileName, boolean allowDirSep) {
		char dirSep = getDirectorySeparatorChar();
		String invalidChars = new String(getInvalidFileNameChars());
		StringBuilder sb = new StringBuilder();
		for (char c : fileName.toCharArray()) {
			if (invalidChars.indexOf(c) < 0) {
				sb.append(c);
			} else if (c == '\\' || c == '/' || c == dirSep) {
				sb.append(dirSep);
			}
		}
		return sb.toString();
	}
}