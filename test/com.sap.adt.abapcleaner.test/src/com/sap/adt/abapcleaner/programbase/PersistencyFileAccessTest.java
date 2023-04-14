package com.sap.adt.abapcleaner.programbase;

import static org.junit.jupiter.api.Assertions.*;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Random;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.Cult;
import com.sap.adt.abapcleaner.base.FileSystem;

public class PersistencyFileAccessTest {
	private static final String LINE_SEP = System.lineSeparator();
	
	private Persistency persistency;
	
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
		// use the 'real' FileSystem for these tests
		persistency = Persistency.create(FileSystem.create());
	}

	@Test 
	void testFileAccess() {
		String subFolder = "subdir";
		
		String text1 = "abc" + LINE_SEP + " DEF " + LINE_SEP + "\t  Ghi  \t" + LINE_SEP;
		String text2 = "012" + LINE_SEP + " 345 " + LINE_SEP;
		
		String mainTempDir = persistency.getTempDir();
		assertNotNull(mainTempDir);
		
		String rndSuffix = Cult.getPaddedString((new Random()).nextInt(999999), 6, '0');
		String rndFolder = "test_" + Cult.getReverseDateTime(LocalDateTime.now(), true) + "_" + rndSuffix;
		String tempDir1 = persistency.combinePaths(mainTempDir, rndFolder);
		String tempDir2 = persistency.combinePaths(tempDir1, subFolder);
		assertNotNull(tempDir1);
		assertNotNull(tempDir2);
		
		// test creating directories
		assertFalse(persistency.directoryExists(tempDir1));
		persistency.createDirectory(tempDir1);
		assertTrue(persistency.directoryExists(tempDir1));
		assertTrue(persistency.directoryExists(mainTempDir, rndFolder));

		assertFalse(persistency.directoryExists(tempDir2));
		persistency.createDirectory(tempDir2);
		assertTrue(persistency.directoryExists(tempDir2));
		assertTrue(persistency.directoryExists(tempDir1, subFolder));

		// test writing, appending, and reading a file with UTF-8 encoding
		String file1 = "file1.txt";
		String path1 = persistency.combinePaths(tempDir1, file1);
		persistency.writeAllTextToFile(path1, text1);
		assertTrue(persistency.fileExists(path1));
		assertFalse(persistency.directoryExists(path1)); // not a directory
		assertEquals(text1, persistency.readAllTextFromFile(path1));

		persistency.appendToFile(path1, text2);
		assertEquals(text1 + text2, persistency.readAllTextFromFile(path1));

		// test writing, appending, and reading a file with ANSI encoding (subfolder)
		String file2 = "file2.txt";
		String path2 = persistency.combinePaths(tempDir2, file2);
		persistency.writeAllTextToFile(path2, text1, true);
		assertTrue(persistency.fileExists(path2));
		assertFalse(persistency.directoryExists(path2)); // not a directory
		assertEquals(text1, persistency.readAllTextFromFile(path2, true));

		persistency.appendToFile(path2, text2, true);
		assertEquals(text1 + text2, persistency.readAllTextFromFile(path2, true));

		// test getFilesInDirectory()
		String[] expFilesNonRecursive = new String[] { path1 };
		String[] expFilesRecursive = new String[] { path1, path2 };
		// the order returned by persistency.getFilesInDirectory() is system-dependent and thus not predictable
		assertUnorderedStringArrayEquals(expFilesNonRecursive, persistency.getFilesInDirectory(tempDir1, "*.txt"));
		assertUnorderedStringArrayEquals(expFilesRecursive, persistency.getFilesInDirectory(tempDir1, "*.txt", true));
		assertUnorderedStringArrayEquals(expFilesNonRecursive, persistency.getFilesInDirectory(tempDir1, "*.TXT"));
		assertUnorderedStringArrayEquals(expFilesRecursive, persistency.getFilesInDirectory(tempDir1, "*.TXT", true));
		assertUnorderedStringArrayEquals(expFilesNonRecursive, persistency.getFilesInDirectory(tempDir1, null));
		assertUnorderedStringArrayEquals(expFilesRecursive, persistency.getFilesInDirectory(tempDir1, null, true));
		assertEquals(null, persistency.getFilesInDirectory(tempDir1, "*.abc"));
		assertEquals(null, persistency.getFilesInDirectory(tempDir1, "*.abc", true));
		// test getDirectories()
		assertUnorderedStringArrayEquals(new String[] { tempDir2 }, persistency.getDirectories(tempDir1, false));
		
		// test invalid pattern
		try {
			persistency.getFilesInDirectory(tempDir1, "??");
			fail();
		} catch (IllegalArgumentException ex) {
			// expected case
		}

		String pathNotFound = persistency.combinePaths(tempDir1, "file_not_found.txt");
		assertEquals(null, persistency.readAllTextFromFile(pathNotFound));
		
		// rename file1 to file3 in the subfolder
		String file3 = "file3.txt";
		String path3 = persistency.combinePaths(tempDir2, file3);
		assertTrue(persistency.renameFile(path1, path3));
		assertFalse(persistency.fileExists(path1));
		assertTrue(persistency.fileExists(path3));
		assertEquals(null, persistency.getFilesInDirectory(tempDir1, null));
		assertEquals(2, persistency.getFilesInDirectory(tempDir1, null, true).length);

		// delete files
		persistency.deleteFile(path2);
		assertFalse(persistency.fileExists(path2));
		persistency.deleteFile(path3);
		assertFalse(persistency.fileExists(path3));
		assertEquals(null, persistency.getFilesInDirectory(tempDir1, null, true));
		
		// delete directories
		persistency.deleteDir(tempDir2, false);
		assertFalse(persistency.directoryExists(tempDir2));
		persistency.deleteDir(tempDir1, false);
		assertFalse(persistency.directoryExists(tempDir1));
	}
}
