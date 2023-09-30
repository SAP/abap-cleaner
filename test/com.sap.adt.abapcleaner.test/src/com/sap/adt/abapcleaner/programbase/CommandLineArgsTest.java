package com.sap.adt.abapcleaner.programbase;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class CommandLineArgsTest {
	private PersistencyDouble persistency;
	
	private final String anySourceCode = "CLASS any_class IMPLEMENTATION." + System.lineSeparator() + "ENDCLASS.";
	private final String anyProfileData = "{}";
	
	private void assertErrorsContain(CommandLineArgs args, String text) {
		assertTrue(args.hasErrors());
		assertTrue(args.errors.indexOf(text) >= 0);
	}
	
	@BeforeEach
	void setup() {
		persistency = PersistencyDouble.create();
	}

	@Test 
	void testGetAllOptions() {
		String[] allOptions = CommandLineArgs.getAllOptions();
		assertNotNull(allOptions);
		assertTrue(allOptions.length > 0);
	}

	@Test 
	void testGetHelp() {
		String help = CommandLineArgs.getHelp(persistency);
		
		// ensure that all options are mentioned in the help text
		String[] allOptions = CommandLineArgs.getAllOptions();
		for (String option : allOptions) {
			assertTrue(help.indexOf(option) >= 0, "option " + option + " not mentioned in CommandLineArgs help");
		}
	}
	
	@Test
	void testCreateEmpty() {
		assertNull(CommandLineArgs.create(persistency, null));
		assertNull(CommandLineArgs.create(persistency, new String[] {} ));
	}

	@Test
	void testCreateForHelp() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] { "/?" } );
		assertTrue(args.showHelp);

		args = CommandLineArgs.create(persistency, new String[] { "/man" } );
		assertTrue(args.showHelp);
	}
	
	@Test
	void testCreateFromSourceCodeWithNoOptions() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode } );
	
		assertEquals(anySourceCode, args.sourceCode);
		assertNull(args.cleanupRange);
		assertNull(args.profileData);
		assertNull(args.abapRelease);

		assertNull(args.targetPath); 
		assertFalse(args.overwrite);
		assertFalse(args.partialResult);
		assertFalse(args.showStats);
		assertFalse(args.showUsedRules);
		assertEquals("", args.errors);
		assertFalse(args.showHelp);

		assertTrue(args.writesResultCodeToOutput());
		assertFalse(args.hasErrors());
	}

	@Test
	void testCreateFromSourceFileWithAllOptions() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		String profilePath = persistency.getTempPath("any_profile.cfj");
		String targetPath = persistency.getTempPath("any_target.txt");
		
		persistency.prepareFile(sourcePath, anySourceCode);
		persistency.prepareFile(profilePath, anyProfileData);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--sourcefile", sourcePath, 
				"--linerange", "20-35", 
				"--profile", profilePath, 
				"--release", "757", 
				"--targetfile", targetPath, 
				"--overwrite", "--partialresult", "--stats", "--usedrules"} );
	
		assertEquals(anySourceCode, args.sourceCode);
		assertEquals(20, args.cleanupRange.startLine);
		assertEquals(35, args.cleanupRange.endLine);
		assertTrue(args.cleanupRange.expandRange);
		assertEquals(anyProfileData, args.profileData);
		assertEquals("757", args.abapRelease);
		
		assertEquals(targetPath, args.targetPath); 
		assertTrue(args.overwrite);
		assertTrue(args.partialResult);
		assertTrue(args.showStats);
		assertTrue(args.showUsedRules);
		assertEquals("", args.errors);
		assertFalse(args.showHelp);

		assertFalse(args.writesResultCodeToOutput());
		assertFalse(args.hasErrors());
	}
	
	@Test
	void testCreateFromSourceDir() {
		persistency.prepareFile("src", "any_source.abap", anySourceCode);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcedir", "src",
				"--recursive",
				"--overwrite"} );

		assertEquals(1, args.sourcePaths.length);
		assertTrue(args.overwrite);
	}

	@Test
	void testCreateFromSourceDirWithTargetdir() {
		persistency.prepareFile("src", "any_source.txt", anySourceCode);
		persistency.prepareFile("src", "any_source2.abap", anySourceCode);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcedir", "src",
				"--targetdir", "src",
				"--filepattern", "*.txt",
				"--recursive",
				"--overwrite" } );

		assertEquals(1, args.sourcePaths.length);
		assertTrue(args.overwrite);
		assertFalse(args.hasErrors());
	}
	
	@Test
	void testCreateWithProfileDataAndLineRangeStartOnly() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--profiledata", anyProfileData } ); 
	
		assertEquals(anySourceCode, args.sourceCode);
		assertEquals(anyProfileData, args.profileData);
		assertFalse(args.hasErrors());
	}
	
	@Test
	void testCreateErrorWithInvalidLineRange1() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--linerange", "20-" } );
	
		assertNull(args.cleanupRange);
		assertErrorsContain(args, "Expected format");
	}
	
	@Test
	void testCreateErrorWithInvalidLineRange2() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--linerange", "-35" } );
	
		assertNull(args.cleanupRange);
		assertErrorsContain(args, "Expected format");
	}
	
	@Test
	void testCreateErrorWithInvalidLineRange3() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--linerange", "35-20" } );
	
		assertNull(args.cleanupRange);
		assertErrorsContain(args, "Expected format");
	}
	
	@Test
	void testCreateErrorArgumentMissing() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		persistency.prepareFile(sourcePath, anySourceCode);

		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--sourcefile" } );

		assertErrorsContain(args, "Argument missing");
		assertErrorsContain(args, "--sourcefile");
	}
	
	@Test
	void testCreateErrorWithTwoSources() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		persistency.prepareFile(sourcePath, anySourceCode);

		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--sourcefile", sourcePath, 
				"--source", anySourceCode } );

		assertErrorsContain(args, "Source code supplied twice");
	}
	
	@Test
	void testCreateErrorOverwriteMissing() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		String targetPath = persistency.getTempPath("any_target.txt");
		
		persistency.prepareFile(sourcePath, anySourceCode);
		persistency.prepareFile(targetPath, anySourceCode);

		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--sourcefile", sourcePath, 
				"--targetfile", targetPath } );

		assertErrorsContain(args, "Target file already exists");
		assertErrorsContain(args, "--overwrite");
	}
	
	@Test
	void testCreateErrorSourceDirOverwriteMissing() {
		persistency.prepareFile("src", "any_source.abap", anySourceCode);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcedir", "src" } );

		assertErrorsContain(args, "Source file src" + PersistencyDouble.getDirectorySeparatorChar() + "any_source.abap already exists");
		assertErrorsContain(args, "--overwrite");
	}

	@Test
	void testCreateErrorSourceDirNotExisting() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcedir", "src" } );
		
		assertErrorsContain(args, "Source directory src does not exist!");
	}
	
	@Test
	void testCreateErrorNoFilesInSourceDir() {
		persistency.prepareDirectory("src");
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcedir", "src" } );
		
		assertErrorsContain(args, "No matching files found");
	}
	
	@Test
	void testCreateErrorSourceDirWithInvalidOptions() {
		persistency.prepareDirectory("src");
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcedir", "src", 
				"--linerange", "10-20", 
				"--targetfile", "any_target_file.txt", 
				"--partialresult" } );

		assertErrorsContain(args, "Invalid combination: --linerange");
		assertErrorsContain(args, "Invalid combination: --targetfile");
		assertErrorsContain(args, "Invalid combination: --partialresult");
	}
	
	@Test
	void testCreateErrorWithTwoProfiles() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		String profilePath = persistency.getTempPath("any_profile.cfj");
		persistency.prepareFile(sourcePath, anySourceCode);
		persistency.prepareFile(profilePath, anyProfileData);

		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode,
				"--profile", profilePath, 
				"--profiledata", anyProfileData } );

		assertErrorsContain(args, "Profile supplied twice");
	}
	
	@Test
	void testCreateErrorSourceFileMissing() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		String wrongSourcePath = persistency.getTempPath("other_source.txt");
		persistency.prepareFile(sourcePath, anySourceCode);

		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--sourcefile", wrongSourcePath } );

		assertErrorsContain(args, "File not found");
		assertErrorsContain(args, wrongSourcePath);
	}
	
	@Test
	void testCreateErrorProfileFileMissing() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		String profilePath = persistency.getTempPath("any_profile.cfj");
		String wrongProfilePath = persistency.getTempPath("other_profile.cfj");
		
		persistency.prepareFile(sourcePath, anySourceCode);
		persistency.prepareFile(profilePath, anyProfileData);
		persistency.prepareFile(sourcePath, anySourceCode);

		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--sourcefile", sourcePath,
				"--profile", wrongProfilePath } );

		assertErrorsContain(args, "File not found");
		assertErrorsContain(args, wrongProfilePath);
	}
	
	@Test
	void testCreateErrorWithUnknownOption() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--unknownoption" } );

		assertErrorsContain(args, "Unknown option");
	}
}
