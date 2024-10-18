package com.sap.adt.abapcleaner.base;

import java.io.BufferedReader;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;

public class FileSystemDouble implements IFileSystem {
	// helper classes to mock the file system

	private static class FileInfo {
		public final String path;
		public byte[] data;
		public boolean isAnsiEncoded;
		public boolean isWriteProtected;
		public long lastModified;
		
		public FileInfo(String path, byte[] data) {
			this.path = path;
			this.data = data;
		}
		public FileInfo(String path, FileInfo model) {
			this.path = path;
			this.data = model.data;
			this.isAnsiEncoded = model.isAnsiEncoded;
			this.isWriteProtected = model.isWriteProtected;
		}
	}
	
	private static class DirInfo {
		public final String path;
		public DirInfo(String path) {
			this.path = path;
		}
	}
	
	// -------------------------------------------------------------------------

	private final String dirSep;
	private final boolean isCaseSensitive;

	private HashMap<String, FileInfo> fileInfos = new HashMap<>();
	private HashMap<String, DirInfo> directories = new HashMap<>();
	private int writeAllBytesCallCount;

	public static FileSystemDouble create() {
		return new FileSystemDouble();
	}
	
	private FileSystemDouble() {
		dirSep = String.valueOf(PersistencyBase.getDirectorySeparatorChar());
		isCaseSensitive = (System.getProperty("os.name").toUpperCase().indexOf("WIN") < 0);		
	}
	
	private String getKey(String file) {
		return isCaseSensitive ? file : file.toUpperCase();
	}

	public String addDirSep(String dir) {
		return StringUtil.endsWith(dir, dirSep, false) ? dir : dir + dirSep;
	}

	public void setWriteProtect(String path, boolean isWriteProtected) {
		FileInfo fileInfo = fileInfos.get(getKey(path));
		if (fileInfo != null) {
			fileInfo.isWriteProtected = isWriteProtected;
		}
	}
	
	public void setLastModified(String path, long lastModified) {
		FileInfo fileInfo = fileInfos.get(getKey(path));
		if (fileInfo != null) {
			fileInfo.lastModified = lastModified;
		}
	}

	public void clear() {
		fileInfos.clear();
		directories.clear();
		writeAllBytesCallCount = 0;
	}
	
	public int getWriteAllBytesCallCount() {
		return writeAllBytesCallCount;
	}
	
	// -------------------------------------------------------------------------
	// interface methods to mock the file system
	
	@Override
	public boolean fileExists(String path) {
		if (path == null)
			return false;
		return fileInfos.containsKey(getKey(path));
	}

	@Override
	public long getLastModified(String path) {
		if (path == null)
			return 0;
		FileInfo fileInfo = fileInfos.get(getKey(path));
		return (fileInfo == null) ? 0 : fileInfo.lastModified;
	}

	@Override
	public boolean deleteFile(String path) {
		FileInfo fileInfo = fileInfos.get(getKey(path));
		if (fileInfo != null && fileInfo.isWriteProtected)
			return false;
		if (fileExists(path)) {
			fileInfos.remove(getKey(path));
			return true;
		} else {
			return false;
		}
	}

	@Override
	public boolean renameFile(String sourcePath, String destPath) {
		if (fileExists(sourcePath) && !fileExists(destPath)) {
			FileInfo fileInfo = fileInfos.get(getKey(sourcePath));
			fileInfos.put(getKey(destPath), new FileInfo(destPath, fileInfo) );
			deleteFile(sourcePath);
			return true;
		} else {
			return false;
		}
	}

	@Override
	public byte[] readAllBytesFromFile(String path) {
		FileInfo fileInfo = fileInfos.get(getKey(path));
		return fileInfo.data; 
	}

	@Override
	public boolean writeAllBytesToFile(String path, byte[] data) {
		++writeAllBytesCallCount;
		fileInfos.put(getKey(path), new FileInfo(path, data));
		return true;
	}

	@Override
	public boolean appendToFile(String path, byte[] appendData) {
		if (!fileExists(path)) {
			return writeAllBytesToFile(path, appendData);
		}
		byte[] existingData = readAllBytesFromFile(path);
		byte[] completeData = new byte[existingData.length + appendData.length];

		System.arraycopy(existingData, 0, completeData, 0, existingData.length);
		System.arraycopy(appendData, 0, completeData, existingData.length, appendData.length);
		
		return writeAllBytesToFile(path, completeData);
	}

	@Override
	public BufferedReader getBufferedReader(String path, Charset charSet) {
		// must be implemented in PersistencyDouble
		return null;
	}

	@Override
	public boolean directoryExists(String dir) {
		if (dir == null)
			return false;
		return directories.containsKey(getKey(addDirSep(dir)));
	}

	@Override
	public String[] getFilesInDirectory(String path, String searchPattern, boolean recursive) {
		if (!directoryExists(path))
			return null;
		ArrayList<String> paths = new ArrayList<>();
		path = getKey(addDirSep(path));
		for (String file : fileInfos.keySet()) {
			if (!StringUtil.startsWith(file, path, !isCaseSensitive)) 
				continue;
			
			int dirSepPos = file.indexOf(dirSep, path.length());
			if (dirSepPos >= 0 && !recursive) 
				continue;
			
			if (matchesPattern(file, searchPattern))
				paths.add(fileInfos.get(file).path);
		}
		return StringUtil.toStringArray(paths);
	}
	
	private boolean matchesPattern(String fileName, String searchPattern) {
		if (searchPattern == null || searchPattern.length() == 0)
			return true;
		if (searchPattern.startsWith("*"))
			return StringUtil.endsWith(fileName, searchPattern.substring(1), true);
		throw new IllegalArgumentException("Unexpected search pattern!");
	}

	@Override
	public String[] getDirectories(String path, boolean recursive) {
		ArrayList<String> paths = new ArrayList<>();
		path = addDirSep(path);
		for (String dir : directories.keySet()) {
			if (!StringUtil.startsWith(dir, path, !isCaseSensitive)) 
				continue;
			
			int dirSepPos = dir.indexOf(dirSep, path.length());
			if (dirSepPos >= 0 && !recursive) 
				continue;
			
			paths.add(directories.get(dir).path);
		}
		return StringUtil.toStringArray(paths);
	}

	@Override
	public void createDirectory(String dir) {
		dir = addDirSep(dir);
		directories.put(getKey(dir), new DirInfo(dir));
	}

	@Override
	public boolean deleteDir(String dir, boolean recursive) {
		String[] files = getFilesInDirectory(dir, "", recursive);
		if (files.length > 0)
			return false;
		String[] dirs = getDirectories(dir, recursive);
		for (String directory : dirs) {
			directories.remove(getKey(directory));
		}
		return true;
	}

	@Override
	public String getAbsolutePath(String path) {
		// TODO: better mocking required??
		return path;
	}
}
