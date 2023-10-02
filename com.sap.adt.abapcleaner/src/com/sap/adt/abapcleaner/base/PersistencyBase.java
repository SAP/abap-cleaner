package com.sap.adt.abapcleaner.base;

import java.io.File;
import java.nio.charset.Charset;

/**
 * Provides methods to persist data on the local file system, encapsulating java.io.File.
 */
public class PersistencyBase {
	private IFileSystem fileSystem;

	private static Charset ansiCharset;
	private static Charset utf8Charset;

	protected PersistencyBase(IFileSystem fileSystem) {
		this.fileSystem = fileSystem;
   	ansiCharset = Charset.forName("ISO-8859-1");
   	utf8Charset = Charset.forName("UTF-8");
	}

	private static Charset getCharset(boolean ansiEncoding) {
		return ansiEncoding ? ansiCharset : utf8Charset;
	}
	
	public IFileSystem getFileSystem() {
		return fileSystem;
	}
	
	// -------------------------------------------------------------------------
	// Path

	public String getAbsolutePath(String path) {
		return fileSystem.getAbsolutePath(path);
	}
	
	// -------------------------------------------------------------------------
	// File
	
	public boolean fileExists(String dir, String file) {
		return fileExists(combinePaths(dir, file));
	}
	public boolean fileExists(String path) {
		return fileSystem.fileExists(path);
	}

	public boolean deleteFile(String path) {
		return fileSystem.deleteFile(path);
	}

	/** expects sourcePath to exist, the directory of destPath to exist, and destPath to not exist */
	public boolean renameFile(String sourcePath, String destPath) {
		return fileSystem.renameFile(sourcePath, destPath);
	}

	public final String readAllTextFromFile(String path) {
		return readAllTextFromFile(path, false);
	}
	public String readAllTextFromFile(String path, boolean ansiEncoding) {
		byte[] encoded = fileSystem.readAllBytesFromFile(path);
		return (encoded == null) ? null : new String(encoded, getCharset(ansiEncoding));
	}

	public final boolean writeAllTextToFile(String path, String contents) {
		return writeAllTextToFile(path, contents, false);
	}
	public boolean writeAllTextToFile(String path, String contents, boolean ansiEncoding) {
		return fileSystem.writeAllBytesToFile(path, contents.getBytes(getCharset(ansiEncoding)));
	}

	public boolean appendToFile(String path, String contents) {
		return appendToFile(path, contents, false);
	}
	public boolean appendToFile(String path, String contents, boolean ansiEncoding) {
		return fileSystem.appendToFile(path, contents.getBytes(getCharset(ansiEncoding)));
	}

	// -------------------------------------------------------------------------
	// Directory

	public boolean directoryExists(String dir) {
		return fileSystem.directoryExists(dir);
	}
	public boolean directoryExists(String dir, String folder) {
		return directoryExists(combinePaths(dir, folder));
	}

	public final String[] getFilesInDirectory(String path, String searchPattern) {
		return getFilesInDirectory(path, searchPattern, false);
	}
	public String[] getFilesInDirectory(String path, String searchPattern, boolean recursive) {
		return fileSystem.getFilesInDirectory(path, searchPattern, recursive);
	}

	public String[] getDirectories(String path, boolean recursive) {
		return fileSystem.getDirectories(path, recursive);
	}

	public void createDirectory(String dir) {
		fileSystem.createDirectory(dir);
	}

	/**
	 * Deletes the supplied directory (and, in the recursive case, its sub-directory) 
	 * if the directory is empty (and its sub-directories are empty)
	 * @param dir - the directory to delete
	 * @param recursive - true to delete sub-directories, too
	 * @return true if deletion was successful
	 */
	public boolean deleteDir(String dir, boolean recursive) {
		return fileSystem.deleteDir(dir, recursive);
	}

	// -------------------------------------------------------------------------
	// methods that use java.io.File, but do not need FileSystem access

	public final String getFileName(String path) {
		return (new File(path)).getName();
	}

	public final String getFileNameWithoutExtension(String path) {
		String fileName = (new File(path)).toPath().getFileName().toString();
		int dotPos = fileName.lastIndexOf('.');
		return (dotPos < 0) ? fileName : fileName.substring(0, dotPos);
	}

	public final String getExtension(String path) {
		String fileName = (new File(path)).toPath().getFileName().toString();
		int dotPos = fileName.lastIndexOf('.');
		return (dotPos < 0) ? "" : fileName.substring(dotPos);
	}

	public final String combinePaths(String... paths) {
		String result = null;
		char dirSepChar = getDirectorySeparatorChar();
		for (String path : paths) {
			if (result == null || result.length() == 0)
				result = path;
			else {
				if (result.charAt(result.length() - 1) != dirSepChar) {
					result += dirSepChar;
				}
				if (!StringUtil.isNullOrEmpty(path) && path.charAt(0) == dirSepChar) {
					path = path.substring(1);
				}
				result += path;
			}
		}
		return result;
	}

	public final char[] getInvalidFileNameChars() {
		return new char[] { '\"', '<', '>', '|', ':', '*', '?', '\\', '/', '\u0000', '\u0001', '\u0002', '\u0003', '\u0004', '\u0005', '\u0006', '\u0007', '\u0008', '\u0009', '\n',
				'\u000b', '\u000c', '\r', '\u000e', '\u000f', '\u0010', '\u0011', '\u0012', '\u0013', '\u0014', '\u0015', '\u0016', '\u0017', '\u0018', '\u0019', '\u001a', '\u001b',
				'\u001c', '\u001d', '\u001e', '\u001f' };
	}

	public final String getDirectoryName(String path) {
		return (new File(path)).getParent();
	}

	public static final char getDirectorySeparatorChar() { 
		return File.separatorChar; 
	}

	public final String getTempDir() { 
		return System.getProperty("java.io.tmpdir");
	}

	public String getAppDataDir(String appDataCompanyFolderWin, String appDataFolderWin, String appDataFolderMac, String appDataFolderLinux, String fallbackDir) {
		String sep = String.valueOf(File.separatorChar);

		String appDataDir;
	   String subfolder = null;
	   
	   // determine a platform-specific folder to store application data
		try {
			String osNameUpper = System.getProperty("os.name").toUpperCase();
			if (osNameUpper.indexOf("WIN") >= 0) { // "Windows 10", "Windows 8.1" etc.
				// for Windows, use the Windows-specific APPDATA, usually "C:\Users\<user>\AppData\Roaming"
				// (user.home would just return "C:\Users\<user>" here)
				appDataDir = System.getenv("APPDATA");
				subfolder = appDataCompanyFolderWin + sep + appDataFolderWin;
			
			} else {
				// for all non-Windows systems, use a sub-folder of user.home 
				appDataDir = System.getProperty("user.home");
				
				if (osNameUpper.indexOf("MAC") >= 0) { // "Mac OS X" etc.
					// for macOS, create and manage all app-specific data and support files in 
					// "~/Library/Application Support/<bundle identifier>" (or alternatively, "~/Library/Preferences/<bundle identifier>")
					subfolder =  "Library" + sep + "Application Support" + sep + appDataFolderMac;   
					
				} else if (osNameUpper.indexOf("NIX") >= 0 || osNameUpper.indexOf("NUX") >= 0 || osNameUpper.indexOf("AIX") > 0) { // "Linux" etc.
					// Linux prefers a hidden directory (therefore, leading ".")
					subfolder =  "." + appDataFolderLinux; 
				
				} else {
					// for all other ("SunOS", "FreeBSD" etc.), we use Linux behavior 
					subfolder =  "." + appDataFolderLinux; 
				}
			}
		} catch (SecurityException ex) {
			appDataDir = null;
			subfolder =  null; 
		}

		// if the above failed, fall back to a sub-folder in the startupPath
		if (StringUtil.isNullOrEmpty(appDataDir)) 
			appDataDir = fallbackDir;
		if (StringUtil.isNullOrEmpty(subfolder))
			subfolder =  "." + appDataFolderLinux; 

		if (!appDataDir.endsWith(sep))
			appDataDir += sep;
		
		return appDataDir + subfolder;
	}
}
