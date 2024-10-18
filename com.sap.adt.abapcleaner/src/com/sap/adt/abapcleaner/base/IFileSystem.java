package com.sap.adt.abapcleaner.base;

import java.io.BufferedReader;
import java.nio.charset.Charset;

public interface IFileSystem {
   // File
	public boolean fileExists(String path);
	public long getLastModified(String path);
	public boolean deleteFile(String path);
	public boolean renameFile(String sourcePath, String destPath);

	public byte[] readAllBytesFromFile(String path);
	public boolean writeAllBytesToFile(String path, byte[] data);
	public boolean appendToFile(String path, byte[] data);
	
	public BufferedReader getBufferedReader(String path, Charset charSet);

	// Path
	public String getAbsolutePath(String path);

	// Directory
	public boolean directoryExists(String dir);
	public String[] getFilesInDirectory(String path, String searchPattern, boolean recursive);
	public String[] getDirectories(String path, boolean recursive);
	public void createDirectory(String dir);
	public boolean deleteDir(String dir, boolean recursive);
}
