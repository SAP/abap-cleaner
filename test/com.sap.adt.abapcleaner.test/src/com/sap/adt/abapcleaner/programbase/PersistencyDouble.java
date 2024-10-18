package com.sap.adt.abapcleaner.programbase;

import java.io.BufferedReader;
import java.io.StringReader;

import com.sap.adt.abapcleaner.base.FileSystemDouble;

public class PersistencyDouble extends Persistency {
	private final FileSystemDouble fileSystemDouble;
	
	private int newFileNum = 0;
	
	public static PersistencyDouble create() {
		FileSystemDouble fileSystemDouble = FileSystemDouble.create();

		// create a PersistencyDouble and inject it as the single instance
		PersistencyDouble persistencyDouble = new PersistencyDouble(fileSystemDouble);
		singleInstance = persistencyDouble;
		
		// as the workDir, use any directory path that is valid on the local system 
		String workDir = fileSystemDouble.addDirSep(System.getProperty("java.io.tmpdir"));
		persistencyDouble.initialize(workDir, null);
		return persistencyDouble;
	}

	private PersistencyDouble(FileSystemDouble fileSystemDouble) {
		super(fileSystemDouble);
		
		this.fileSystemDouble = fileSystemDouble;
	}

	@Override
	public BufferedReader getBufferedReader(String path, boolean ansiEncoding) {
		String text = readAllTextFromFile(path, ansiEncoding);
		return (text == null) ? null : new BufferedReader(new StringReader(text));
	}

	// -------------------------------------------------------------------------
	// helper methods for tests

	public String getAnyNewPath() {
		++newFileNum;
		return combinePaths(getWorkDir(), "file" + String.valueOf(newFileNum) + ".tmp");
	}
	
	public void prepareFile(String path, String data) {
		writeAllTextToFile(path, data);
	}

	public String prepareFile(String dir, String file, String data) {
		String path = combinePaths(dir, file);
		prepareDirectory(dir);
		prepareFile(path, data);
		return path;
	}

	public void prepareDirectory(String dir) {
		createDirectory(dir);
	}
	
	public String prepareDirectory(String baseDir, String folder) {
		String dir = addDirSep(combinePaths(baseDir, folder));
		prepareDirectory(dir);
		return dir;
	}
	
	public void setWriteProtect(String path, boolean isWriteProtected) {
		fileSystemDouble.setWriteProtect(path, isWriteProtected);
	}

	public void setLastModified(String path, long lastModified) {
		fileSystemDouble.setLastModified(path, lastModified);
	}

	public String getTempPath(String file) {
		return combinePaths(getTempDir(), file);
	}
	
	public int getDirectoryCount(String baseDir) {
		String[] dirs = getDirectories(baseDir, true);
		return (dirs == null) ? 0 : dirs.length;
	}
	
	public void clear() {
		fileSystemDouble.clear();
	}
}
