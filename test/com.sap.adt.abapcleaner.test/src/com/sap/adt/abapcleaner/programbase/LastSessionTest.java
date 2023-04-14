package com.sap.adt.abapcleaner.programbase;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class LastSessionTest {
	private PersistencyDouble persistency;
	private String codeDir; 
	private String code1Path;
	private final String code1File = "code1.txt";
	private final String code1Text = "CLASS any_class IMPLEMENTATION. ENDCLASS.";
	private final String code1TextChanged = "CLASS other_class IMPLEMENTATION. ENDCLASS.";

	@BeforeEach
	void setup() {
		persistency = PersistencyDouble.create();
		Program.initialize(persistency, "");

		String workDir = persistency.getWorkDir();
		String userDir = persistency.prepareDirectory(workDir, "user");
		codeDir = persistency.prepareDirectory(userDir, "code");
		code1Path = persistency.prepareFile(codeDir, code1File, code1Text);
	}

	@Test 
	void testSaveAndLoad() {
		// 1. save session to file
		LastSession lastSession = LastSession.create("sourceName", code1Path, code1Text, "758", 10, 20, 25);
		lastSession.save();
		
		// 2. load another session instance from file
		LastSession lastSession2 = LastSession.createEmpty();
		lastSession2.load();
		
		assertEquals("sourceName", lastSession2.getSourceName());
		assertEquals(code1Path, lastSession2.getSourcePath());
		assertEquals(code1Text, lastSession2.getCodeText());
		assertEquals("758", lastSession2.getAbapRelease());
		assertEquals(10, lastSession2.getTopLineIndex());
		assertEquals(20, lastSession2.getCurLineIndex());
		assertEquals(25, lastSession2.getSelectionStartLine());
	}

	@Test 
	void testSaveAndReload() {
		// 1. save session to file
		LastSession lastSession = LastSession.create("sourceName", code1Path, code1Text, "758", 10, 20, 25);
		lastSession.save();

		// 2. load another session instance from file
		LastSession lastSession2 = LastSession.createEmpty();
		lastSession2.load();
		
		// expect code path and text to be as it was saved 
		assertEquals(code1Path, lastSession2.getSourcePath());
		assertEquals(code1Text, lastSession2.getCodeText());

		// 3. reload from changed source file
		persistency.prepareFile(code1Path, code1TextChanged);
		lastSession2.reloadCodeTextFromCodePath();

		// expect code text to be changed
		assertEquals(code1TextChanged, lastSession2.getCodeText());
		
		// 4. reload from deleted source file
		persistency.deleteFile(code1Path);
		lastSession2.reloadCodeTextFromCodePath();

		// expect buffered code text from step 2 
		assertEquals(code1TextChanged, lastSession2.getCodeText());
	}

	@Test 
	void testSaveAndLoadEmpty() {
		// 1. save session to file
		LastSession lastSession = LastSession.create(null, null, null, null, 10, 20, 30);
		lastSession.save();
		
		// 2. load another session instance from file
		LastSession lastSession2 = LastSession.createEmpty();
		lastSession2.load();

		// expect values to be empty, but non-null
		assertEquals("", lastSession2.getSourceName());
		assertEquals("", lastSession2.getSourcePath());
		assertEquals("", lastSession2.getCodeText());
		assertEquals("", lastSession2.getAbapRelease());
		// expect line indices to be 0, because the code was empty
		assertEquals(0, lastSession2.getTopLineIndex());
		assertEquals(0, lastSession2.getCurLineIndex());
		assertEquals(0, lastSession2.getSelectionStartLine());

		// 3. reload, although there is no source path
		lastSession2.reloadCodeTextFromCodePath();

		// expect code text to be unchanged 
		assertEquals("", lastSession2.getCodeText());
	}

}
