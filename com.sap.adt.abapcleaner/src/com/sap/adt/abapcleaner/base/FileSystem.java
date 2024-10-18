package com.sap.adt.abapcleaner.base;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;

public class FileSystem implements IFileSystem {
	public static FileSystem create() {
		return new FileSystem();
	}

	@Override
	public String getAbsolutePath(String path) {
		return new File(path).getAbsolutePath();
	}

	// -------------------------------------------------------------------------
	// File


	@Override
	public boolean fileExists(String path) {
		return (new File(path)).isFile();
	}

	@Override
	public long getLastModified(String path) {
		return (new File(path)).lastModified();
	}

	@Override
	public boolean deleteFile(String path) {
		return (new File(path)).delete();
	}

	/** expects sourcePath to exist, the directory of destPath to exist, and destPath to not exist */
	@Override
	public boolean renameFile(String sourcePath, String destPath) {
		try {
			return (new File(sourcePath)).renameTo(new File(destPath));
		} catch(SecurityException ex) {
			return false;
		}
	}

	@Override
	public byte[] readAllBytesFromFile(String path) {
		try {
			return Files.readAllBytes(Paths.get(path));
		} catch (IOException e) {
			return null;
		}
	}
	
	@Override
	public boolean writeAllBytesToFile(String path, byte[] data) {
      try {
      	Files.write((new File(path)).toPath(), data);
      	return true;
      } catch (IOException e) {
      	return false;
      }
	}
	
	@Override
	public boolean appendToFile(String path, byte[] data) {
      try {
      	Path filePath = (new File(path)).toPath();
      	Files.write(filePath, data, StandardOpenOption.CREATE, StandardOpenOption.APPEND);
      	return true;
      } catch (IOException e) { 
      	return false;
      }
	}

	@Override
	public BufferedReader getBufferedReader(String path, Charset charSet) {
		try {
			return new BufferedReader(new InputStreamReader(new FileInputStream(new File(path)), charSet));
		} catch (FileNotFoundException e) {
			return null;
		}
	}

	// -------------------------------------------------------------------------
	// Directory
	
	@Override
	public boolean directoryExists(String dir) {
		File dirFile = new File(dir);
		return dirFile.exists() && dirFile.isDirectory();
	}

	@Override
	public String[] getFilesInDirectory(String path, String searchPattern, boolean recursive) {
		File dir = new File(path);
		ArrayList<String> paths = new ArrayList<>();
		addFiles(dir, searchPattern, recursive, paths);
		return StringUtil.toStringArray(paths);
	}

	private void addFiles(File dir, String searchPattern, boolean recursive, ArrayList<String> paths) {
		File[] files = dir.listFiles(); 
		if (files == null)
			return;
		for (File file : files) {
			if (recursive && file.isDirectory())
				addFiles(file, searchPattern, recursive, paths);
			else if (file.isFile() && matchesPattern(file.getName(), searchPattern))
				paths.add(file.getAbsolutePath());
		}
	}

	private boolean matchesPattern(String fileName, String searchPattern) {
		if (searchPattern == null || searchPattern.length() == 0)
			return true;
		
		String[] patterns = StringUtil.split(searchPattern, ';', true);
		for (String pattern : patterns) {
			int asteriskPos = pattern.indexOf('*');
			if (asteriskPos < 0) 
				throw new IllegalArgumentException();
	
			if (asteriskPos > 0) {
				// compare expected prefix
				if (!StringUtil.startsWith(fileName, pattern.substring(0, asteriskPos), true)) {
					continue;
				}
			} // do NOT attach with else if
			if (asteriskPos + 1 < pattern.length()) {
				// compare expected suffix
				if (!StringUtil.endsWith(fileName, pattern.substring(asteriskPos + 1), true)) {
					continue;
				}
			}
			return true;
		}
		return false;
	}
	
	@Override
	public String[] getDirectories(String path, boolean recursive) {
		File dir = new File(path);
		ArrayList<String> paths = new ArrayList<>();
		addDirs(dir, recursive, paths);
		return StringUtil.toStringArray(paths);
	}

	private void addDirs(File dir, boolean recursive, ArrayList<String> paths) {
		File[] files = dir.listFiles(); // TODO: provide FilenameFilter interface created with searchPattern!
		if (files == null)
			return;
		for (File file : files) {
			if (file.isDirectory()) {
				paths.add(file.getAbsolutePath());
				if (recursive) {
					addDirs(file, recursive, paths);
				}
			}
		}
	}
	
	@Override
	public void createDirectory(String dir) {
		(new File(dir)).mkdirs();
	}

	@Override
	public boolean deleteDir(String dir, boolean recursive) {
		return deleteDir(new File(dir), recursive);
	}

	private boolean deleteDir(File dir, boolean recursive) {
		// delete (empty) sub-directories first
		if (recursive) {
			File[] files = dir.listFiles(); 
			if (files != null) {
				for (File file : files) {
					// if any file was found, stop the operation
					if (file.isFile()) {
						return false;
					}
				}
	
				for (File file : files) {
					// if any deletion fails, stop the operation
					if (file.isDirectory() && !deleteDir(file, recursive)) {
						return false;
					}
				}
			}
		}
		return dir.delete();
	}
}
