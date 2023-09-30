package com.sap.adt.abapcleaner.programbase;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.PersistencyBase;

public class PersistencyTest {
	private PersistencyDouble persistency;
	private char DIR_SEP = File.separatorChar;
	
	private String workDir;
	private String helpDir;
	private String userDir;
	private String settingsDir;
	private String profilesDir;
	private String codeDir;
	
	// private String configFile;
	
	private String mainSettingsFileBinary;
	private String mainSettingsFileText;
	private String lastSessionFileBinary;
	private String lastSessionFileText;

	private String profileFile1Binary;
	private String profileFile1Text;
	private String profileFile2Binary;
	private String profileFile2Text;
	
	private String helpFile1;
	private String helpFile2;
	private String codeFile1;
	private String codeFile2;

	private String errorLogFile;

	private void assertStringArrayEquals(String[] exp, String[] act) {
		assertEquals(exp.length, act.length);
		for (int i = 0; i < exp.length; ++i)
			assertEquals(exp[i], act[i]);
	}

	private void assertUnorderedStringArrayEquals(String[] exp, String[] act) {
		Arrays.sort(exp);
		Arrays.sort(act);
		assertStringArrayEquals(exp, act);
	}

	@BeforeEach
	void setup() {
		persistency = PersistencyDouble.create();
		DIR_SEP = PersistencyBase.getDirectorySeparatorChar();
		
		workDir = persistency.getWorkDir();

		helpDir = persistency.prepareDirectory(workDir, "help");
		userDir = persistency.prepareDirectory(workDir, "user");
		settingsDir = persistency.prepareDirectory(userDir, "settings");
		profilesDir = persistency.prepareDirectory(userDir, "profiles");
		codeDir = persistency.prepareDirectory(userDir, "code");
	}

	private void prepareSampleFiles() {
		// configFile = prepareFile(workDir, "config.cfj");
		
		mainSettingsFileBinary = prepareFile(settingsDir, "main.cfg");
		mainSettingsFileText = prepareFile(settingsDir, "main.cfj");
		lastSessionFileBinary = prepareFile(settingsDir, "session.cfg");
		lastSessionFileText = prepareFile(settingsDir, "session.cfj");

		profileFile1Binary = prepareFile(profilesDir, "profile1.cfg");
		profileFile1Text = prepareFile(profilesDir, "profile1.cfj");
		profileFile2Binary = prepareFile(profilesDir, "profile2.cfg");
		profileFile2Text = prepareFile(profilesDir, "profile2.cfj");
		
		helpFile1 = prepareFile(helpDir, "help1.htm");
		helpFile2 = prepareFile(helpDir, "help2.htm");
		codeFile1 = prepareFile(codeDir, "code1.txt");
		codeFile2 = prepareFile(codeDir, "code2.txt");

		errorLogFile = prepareFile(userDir, "error.log");
	}
	
	/** prepares a file on the PersistencyDouble which simply has its file name as its content */
	private String prepareFile(String dir, String file) {
		return persistency.prepareFile(dir, file, file);
	}

	@Test
	void testGetLoadPaths() {
		prepareSampleFiles();
		
		assertUnorderedStringArrayEquals(new String[] { profileFile1Binary, profileFile2Binary }, persistency.getLoadPaths(FileType.PROFILE_BINARY) );
		assertUnorderedStringArrayEquals(new String[] { profileFile1Text, profileFile2Text }, persistency.getLoadPaths(FileType.PROFILE_TEXT) );

		assertUnorderedStringArrayEquals(new String[] { helpFile1, helpFile2 }, persistency.getLoadPaths(FileType.HELP) );
		assertUnorderedStringArrayEquals(new String[] { codeFile1, codeFile2 }, persistency.getLoadPaths(FileType.CODE) );
	}
	
	@Test
	void testGetLoadPathsExc() {
		boolean exceptionThrown = false;
		try {
			persistency.getLoadPaths(FileType.SETTINGS_MAIN_TEXT);
		} catch (IllegalArgumentException e) {
			exceptionThrown = true;
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testGetLoadPathsWithDir() {
		prepareSampleFiles();
		
		assertUnorderedStringArrayEquals(new String[] { profileFile1Binary, profileFile2Binary }, persistency.getLoadPaths(FileType.PROFILE_BINARY, profilesDir) );
		assertUnorderedStringArrayEquals(new String[] { profileFile1Text, profileFile2Text }, persistency.getLoadPaths(FileType.PROFILE_TEXT, profilesDir) );

		assertUnorderedStringArrayEquals(new String[] { helpFile1, helpFile2 }, persistency.getLoadPaths(FileType.HELP, helpDir) );
		assertUnorderedStringArrayEquals(new String[] { codeFile1, codeFile2 }, persistency.getLoadPaths(FileType.CODE, codeDir) );

		// try finding code files in help directory
		assertUnorderedStringArrayEquals(new String[] { }, persistency.getLoadPaths(FileType.CODE, helpDir) );
	}

	@Test
	void testGetLoadPath() {
		prepareSampleFiles();
		
		assertEquals(mainSettingsFileBinary, persistency.getLoadPath(FileType.SETTINGS_MAIN_BINARY));
		assertEquals(mainSettingsFileText, persistency.getLoadPath(FileType.SETTINGS_MAIN_TEXT));
		assertEquals(lastSessionFileBinary, persistency.getLoadPath(FileType.LAST_SESSION_BINARY));
		assertEquals(lastSessionFileText, persistency.getLoadPath(FileType.LAST_SESSION_TEXT));

		assertEquals(errorLogFile, persistency.getLoadPath(FileType.ERROR_LOG));
	}

	@Test
	void testGetLoadEmpty() {
		// no prepareSampleFiles() here
		
		assertNull(persistency.getLoadPath(FileType.SETTINGS_MAIN_BINARY));
		assertNull(persistency.getLoadPath(FileType.SETTINGS_MAIN_TEXT));
		assertNull(persistency.getLoadPath(FileType.LAST_SESSION_BINARY));
		assertNull(persistency.getLoadPath(FileType.LAST_SESSION_TEXT));

		assertNull(persistency.getLoadPath(FileType.ERROR_LOG));
	}

	@Test
	void testGetLoadPathExc() {
		boolean exceptionThrown = false;
		try {
			persistency.getLoadPath(FileType.PROFILE_TEXT);
		} catch (IllegalArgumentException e) {
			exceptionThrown = true;
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testGetExistingDirs() {
		prepareSampleFiles();

		assertUnorderedStringArrayEquals(persistency.getExistingDirs(FileType.SETTINGS_MAIN_BINARY, 0), persistency.getExistingDirs(FileType.SETTINGS_MAIN_BINARY) );
		assertUnorderedStringArrayEquals(persistency.getExistingDirs(FileType.SETTINGS_MAIN_TEXT, 0), persistency.getExistingDirs(FileType.SETTINGS_MAIN_TEXT) );
	}

	@Test
	void testGetSavePath() {
		prepareSampleFiles();

		assertEquals(mainSettingsFileBinary, persistency.getSavePath(FileType.SETTINGS_MAIN_BINARY));
		assertEquals(mainSettingsFileText, persistency.getSavePath(FileType.SETTINGS_MAIN_TEXT));
		assertEquals(lastSessionFileBinary, persistency.getSavePath(FileType.LAST_SESSION_BINARY));
		assertEquals(lastSessionFileText, persistency.getSavePath(FileType.LAST_SESSION_TEXT));

		assertEquals(profileFile1Binary, persistency.getSavePath(FileType.PROFILE_BINARY, "profile1"));
		assertEquals(profileFile1Text, persistency.getSavePath(FileType.PROFILE_TEXT, "profile1"));

		assertEquals(helpFile1, persistency.getSavePath(FileType.HELP, "help1"));
		assertEquals(codeFile1, persistency.getSavePath(FileType.CODE, "code1"));

		assertEquals(errorLogFile, persistency.getSavePath(FileType.ERROR_LOG));
	}

	@Test
	void testGetSavePathExc() {
		boolean exceptionThrown = false;
		try {
			persistency.getSavePath(FileType.CODE);
		} catch (IllegalArgumentException e) {
			exceptionThrown = true;
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testEnsureExistingDirectoryExists() {
		// given 
		String existingDir = codeDir;
		assertTrue(persistency.directoryExists(existingDir));
		int oldDirectoryCount = persistency.getDirectoryCount(existingDir);

		// when
		persistency.ensureDirectoryExists(existingDir);
		
		// then
		assertTrue(persistency.directoryExists(existingDir));
		assertEquals(oldDirectoryCount, persistency.getDirectoryCount(existingDir));
	}
	
	@Test
	void testEnsureNewDirectoryExists() {
		// given
		String newDir = persistency.combinePaths(workDir, "new");
		assertFalse(persistency.directoryExists(newDir));
		int oldDirectoryCount = persistency.getDirectoryCount(workDir);

		// when
		persistency.ensureDirectoryExists(newDir);

		// then
		assertTrue(persistency.directoryExists(newDir));
		assertTrue(persistency.getDirectoryCount(workDir) == oldDirectoryCount + 1);
	}
	
	@Test
	void testEnsureExistingDirectoryExistsForPath() {
		prepareSampleFiles();
		
		// given 
		String existingDir = profilesDir;
		assertTrue(persistency.directoryExists(existingDir));
		int oldDirectoryCount = persistency.getDirectoryCount(existingDir);

		// when
		persistency.ensureDirectoryExistsForPath(profileFile1Text);
		
		// then
		assertTrue(persistency.directoryExists(existingDir));
		assertEquals(oldDirectoryCount, persistency.getDirectoryCount(existingDir));
	}
	
	@Test
	void testEnsureNewDirectoryExistsForPath() {
		// given
		String newDir = persistency.combinePaths(workDir, "new");
		String newFile = persistency.combinePaths(newDir, "file.txt");
		assertFalse(persistency.directoryExists(newDir));
		int oldDirectoryCount = persistency.getDirectoryCount(workDir);

		// when
		persistency.ensureDirectoryExistsForPath(newFile);

		// then
		assertTrue(persistency.directoryExists(newDir));
		assertTrue(persistency.getDirectoryCount(workDir) == oldDirectoryCount + 1);
	}

	@Test 
	void testMoveFile() {
		prepareSampleFiles();

		String newDir = persistency.combinePaths(workDir, "new"); 
		String newPath = persistency.combinePaths(newDir, "new.txt");
		
		// rename a file into a new directory
		assertTrue(persistency.moveFile(profileFile2Text, newPath, false));

		// ensure that the file and its content has arrived under the new path and directory
		assertFalse(persistency.fileExists(profileFile2Text));
		assertTrue(persistency.fileExists(newPath));
		assertTrue(persistency.directoryExists(newDir));
		assertEquals("profile2.cfj", persistency.readAllTextFromFile(newPath));
	}

	@Test 
	void testMoveFileToExisting() {
		prepareSampleFiles();

		String profileFile3Text = persistency.combinePaths(profilesDir, "profile3.cfj");
		
		// try renaming a non-existing file
		assertFalse(persistency.moveFile(profileFile3Text, profileFile2Text, false));
		// ensure that the source file still exists and the target file still has the same content
		assertTrue(persistency.fileExists(profileFile1Text));
		assertEquals("profile2.cfj", persistency.readAllTextFromFile(profileFile2Text));

		// try renaming without allowing to overwrite
		assertFalse(persistency.moveFile(profileFile1Text, profileFile2Text, false));
		// ensure that the source file still exists and the target file still has the same content
		assertTrue(persistency.fileExists(profileFile1Text));
		assertEquals("profile2.cfj", persistency.readAllTextFromFile(profileFile2Text));

		// try overwriting, but write-protect files
		persistency.setWriteProtect(profileFile2Text, true);
		assertFalse(persistency.moveFile(profileFile1Text, profileFile2Text, true));
		// ensure that the source file still exists and the target file still has the same content
		assertTrue(persistency.fileExists(profileFile1Text));
		assertEquals("profile2.cfj", persistency.readAllTextFromFile(profileFile2Text)); 

		// overwrite
		persistency.setWriteProtect(profileFile2Text, false);
		assertTrue(persistency.moveFile(profileFile1Text, profileFile2Text, true));
		// ensure that the source file doesn't exist anymore and the target file has changed content
		assertFalse(persistency.fileExists(profileFile1Text));
		assertEquals("profile1.cfj", persistency.readAllTextFromFile(profileFile2Text));
	}

	@Test
	void testGetExtensionForFileType() {
		assertEquals(".cfj", persistency.getExtension(FileType.CONFIG_TEXT));
		assertEquals(".cfg", persistency.getExtension(FileType.LAST_SESSION_BINARY));
		assertEquals(".cfj", persistency.getExtension(FileType.LAST_SESSION_TEXT));
		assertEquals(".cfg", persistency.getExtension(FileType.SETTINGS_MAIN_BINARY));
		assertEquals(".cfj", persistency.getExtension(FileType.SETTINGS_MAIN_TEXT));
		assertEquals(".cfg", persistency.getExtension(FileType.PROFILE_BINARY));
		assertEquals(".cfj", persistency.getExtension(FileType.PROFILE_TEXT));

		assertEquals(".htm", persistency.getExtension(FileType.HELP));
		assertEquals(".txt", persistency.getExtension(FileType.CODE));
		assertEquals(".log", persistency.getExtension(FileType.ERROR_LOG));
	}

	@Test
	void testFileTypeForMultipleFiles() {
		assertFalse(persistency.isFileTypeForMultipleFiles(FileType.CONFIG_TEXT));
		assertFalse(persistency.isFileTypeForMultipleFiles(FileType.LAST_SESSION_BINARY));
		assertFalse(persistency.isFileTypeForMultipleFiles(FileType.LAST_SESSION_TEXT));
		assertFalse(persistency.isFileTypeForMultipleFiles(FileType.SETTINGS_MAIN_BINARY));
		assertFalse(persistency.isFileTypeForMultipleFiles(FileType.SETTINGS_MAIN_TEXT));
		assertTrue(persistency.isFileTypeForMultipleFiles(FileType.PROFILE_BINARY));
		assertTrue(persistency.isFileTypeForMultipleFiles(FileType.PROFILE_TEXT));

		assertTrue(persistency.isFileTypeForMultipleFiles(FileType.HELP));
		assertTrue(persistency.isFileTypeForMultipleFiles(FileType.CODE));
		assertFalse(persistency.isFileTypeForMultipleFiles(FileType.ERROR_LOG));
	}

	@Test
	void testAddDirSep() {
		String dirWithoutSep = "dir";
		String dirWithSep = dirWithoutSep + DIR_SEP;

		assertEquals(dirWithSep, persistency.addDirSep(dirWithoutSep));
		assertEquals(dirWithSep, persistency.addDirSep(dirWithSep));
		
		dirWithoutSep = "dir" + DIR_SEP + "subdir";
		dirWithSep = dirWithoutSep + DIR_SEP;

		assertEquals(dirWithSep, persistency.addDirSep(dirWithoutSep));
		assertEquals(dirWithSep, persistency.addDirSep(dirWithSep));
	}
	
	@Test
	void testGetFileName() {
		String path = "dir" + DIR_SEP + "subdir" + DIR_SEP + "file.txt";
		assertEquals("file.txt", persistency.getFileName(path));
	}

	@Test
	void testGetFileNameWithoutExtension() {
		String pathWithoutExtension = "dir" + DIR_SEP + "subdir" + DIR_SEP + "file";
		String pathWithExtension = pathWithoutExtension + ".txt";
		assertEquals("file", persistency.getFileNameWithoutExtension(pathWithoutExtension));
		assertEquals("file", persistency.getFileNameWithoutExtension(pathWithExtension));
	}

	@Test
	void testGetExtensionOfPath() {
		String pathWithoutExtension = "dir" + DIR_SEP + "subdir" + DIR_SEP + "file";
		String pathWithExtension = pathWithoutExtension + ".txt";
		assertEquals("", persistency.getExtension(pathWithoutExtension));
		assertEquals(".txt", persistency.getExtension(pathWithExtension));
	}
	
	@Test
	void testGetDirectoryName() {
		String path = "dir" + DIR_SEP + "subdir" + DIR_SEP + "file.txt";
		assertEquals("dir" + DIR_SEP + "subdir", persistency.getDirectoryName(path));
	}

	@Test
	void testCombinePaths() {
		assertEquals("dir" + DIR_SEP + "subdir", persistency.combinePaths("dir", "subdir"));
		assertEquals("dir" + DIR_SEP + "subdir", persistency.combinePaths("dir" + DIR_SEP, "subdir"));
		assertEquals("dir" + DIR_SEP + "subdir", persistency.combinePaths("", "dir", "", "subdir"));
		assertEquals("dir" + DIR_SEP + "subdir", persistency.combinePaths("dir", DIR_SEP + "subdir"));
		assertEquals("dir" + DIR_SEP + "subdir" + DIR_SEP + "file.txt", persistency.combinePaths("dir", "subdir", "file.txt"));
	}
	
	@Test
	void testGetInvalidFileNameChars() {
		assertTrue(persistency.getInvalidFileNameChars().length > 0);
	}
	
	@Test
	void testGetDirectorySeparatorChar() {
		assertEquals(DIR_SEP, Persistency.getDirectorySeparatorChar());
	}
}
