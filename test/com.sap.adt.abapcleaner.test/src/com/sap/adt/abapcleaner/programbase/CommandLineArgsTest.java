package com.sap.adt.abapcleaner.programbase;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.CleanupRangeExpandMode;

public class CommandLineArgsTest {
	private PersistencyDouble persistency;
	
	private final String anySourceCode = "CLASS any_class IMPLEMENTATION." + System.lineSeparator() + "ENDCLASS.";
	private final String anyProfileData = "{}";
	private final String anyProfileName = "dummy_profile";
	
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
		assertEquals(CommandLineAction.SHOW_HELP, args.action);

		args = CommandLineArgs.create(persistency, new String[] { "/man" } );
		assertEquals(CommandLineAction.SHOW_HELP, args.action);
	}
	
	@Test
	void testCreateForVersion() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] { "--version" } );
		assertEquals(CommandLineAction.SHOW_VERSION, args.action);
	}
	
	@Test
	void testCreateFromSourceCodeWithNoOptions() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode } );
	
		assertEquals(CommandLineAction.CLEANUP, args.action);
		assertNull(args.sourceName);
		assertEquals(anySourceCode, args.sourceCode);
		assertNull(args.cleanupRange);
		
		assertFalse(args.hasAnyProfileOption());
		assertFalse(args.hasProfileData());
		assertFalse(args.hasProfileName());
		assertFalse(args.useLastProfile());
		assertNull(args.profileData);
		assertNull(args.profileName);
		assertNull(args.abapRelease);

		assertEquals(ABAP.LINE_SEP_FOR_COMMAND_LINE, args.lineSeparator);
		assertNull(args.targetPath); 
		assertFalse(args.overwrite);
		assertFalse(args.partialResult);
		assertFalse(args.showStatsOrUsedRules());
		assertFalse(args.showStats);
		assertFalse(args.showUsedRules);
		assertEquals("", args.errors);

		assertFalse(args.hasErrors());
		assertTrue(args.isInSingleSourceMode());
		assertTrue(args.writesResultCodeToOutput());
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
				"--scope", "method",
				"--profile", profilePath, 
				"--release", "757", 
				"--crlf",
				"--targetfile", targetPath, 
				"--overwrite", "--partialresult", "--stats", "--usedrules"} );
	
		assertEquals(CommandLineAction.CLEANUP, args.action);
		assertEquals("", args.errors);
		
		assertEquals("any_source", args.sourceName);
		assertEquals(anySourceCode, args.sourceCode);
		assertEquals(20, args.cleanupRange.startLine);
		assertEquals(35, args.cleanupRange.lastLine);
		assertTrue(args.cleanupRange.expandRange);
		assertEquals(CleanupRangeExpandMode.FULL_METHOD, args.cleanupRangeExpandMode);
		
		assertTrue(args.hasAnyProfileOption());
		assertTrue(args.hasProfileData());
		assertFalse(args.hasProfileName());
		assertFalse(args.useLastProfile());
		assertEquals(anyProfileData, args.profileData);
		assertNull(args.profileName);
		assertEquals("757", args.abapRelease);
		
		assertFalse(args.simulate);
		assertEquals(targetPath, args.targetPath); 
		assertTrue(args.partialResult);
		assertTrue(args.overwrite);
		assertEquals("\r\n", args.lineSeparator);

		assertTrue(args.showStatsOrUsedRules());
		assertTrue(args.showStats);
		assertTrue(args.showUsedRules);

		assertFalse(args.hasErrors());
		assertTrue(args.isInSingleSourceMode());
		assertFalse(args.writesResultCodeToOutput());
	}
	
	@Test
	void testCreateFromSourceFileInteractive() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		String targetPath = persistency.getTempPath("any_target.txt");
		
		persistency.prepareFile(sourcePath, anySourceCode);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--sourcefile", sourcePath, 
				"--linerange", "20-35", 
				"--scope", "user",
				"--release", "757", 
				"--workspace", "any_workspace_dir",
				"--ui",
				"--title", "any_title",
				"--darktheme",
				"--targetfile", targetPath, 
				"--overwrite"} );
	
		assertEquals(CommandLineAction.CLEANUP, args.action);
		assertEquals("", args.errors);
		
		assertEquals("any_source", args.sourceName);
		assertEquals(anySourceCode, args.sourceCode);
		assertEquals(20, args.cleanupRange.startLine);
		assertEquals(35, args.cleanupRange.lastLine);
		assertTrue(args.cleanupRange.expandRange);
		assertNull(args.cleanupRangeExpandMode); // null means that the user settings from the UI will be used
		assertEquals("757", args.abapRelease);
		
		assertFalse(args.hasAnyProfileOption());
		assertFalse(args.hasProfileData());
		assertFalse(args.hasProfileName());
		assertFalse(args.useLastProfile());
		assertNull(args.profileData);
		assertNull(args.profileName);
		
		assertTrue(args.interactive);
		assertEquals("any_title", args.title);
		assertEquals("any_workspace_dir", args.workspaceDir);
		assertFalse(args.readOnly);
		assertTrue(args.darkTheme);
		
		assertFalse(args.simulate);
		assertEquals(targetPath, args.targetPath); 
		assertFalse(args.partialResult);
		assertTrue(args.overwrite);

		assertFalse(args.showStatsOrUsedRules());
		assertFalse(args.showStats);
		assertFalse(args.showUsedRules);

		assertFalse(args.hasErrors());
		assertTrue(args.isInSingleSourceMode());
		assertFalse(args.writesResultCodeToOutput());
	}
	
	@Test
	void testCreateFromSourceDir() {
		persistency.prepareFile("src", "any_source.abap", anySourceCode);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcedir", "src",
				"--recursive",
				"--last-profile",
				"--overwrite"} );

		assertEquals(CommandLineAction.CLEANUP, args.action);
		assertEquals(1, args.sourcePaths.length);
		
		assertTrue(args.hasAnyProfileOption());
		assertFalse(args.hasProfileData());
		assertFalse(args.hasProfileName());
		assertTrue(args.useLastProfile());

		assertFalse(args.simulate);
		assertTrue(args.overwrite);
	}

	@Test
	void testCreateFromSourceDirSimulate() {
		persistency.prepareFile("src", "any_source.abap", anySourceCode);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcedir", "src",
				"--profilename", anyProfileName,
				"--simulate", "--usedrules"} );

		assertEquals(CommandLineAction.CLEANUP, args.action);
		assertEquals(1, args.sourcePaths.length);
		
		assertTrue(args.hasAnyProfileOption());
		assertFalse(args.hasProfileData());
		assertTrue(args.hasProfileName());
		assertFalse(args.useLastProfile());
		assertEquals(anyProfileName, args.profileName);
		
		assertTrue(args.simulate);
		assertFalse(args.writesResultCodeToOutput());
	
		assertTrue(args.showStatsOrUsedRules());
		assertFalse(args.showStats);
		assertTrue(args.showUsedRules);
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

		assertEquals(CommandLineAction.CLEANUP, args.action);
		assertFalse(args.hasErrors());

		assertEquals(1, args.sourcePaths.length);
		assertFalse(args.isInSingleSourceMode());

		assertFalse(args.simulate);
		assertFalse(args.writesResultCodeToOutput());
		assertTrue(args.overwrite);
	}
	
	@Test
	void testCreateWithProfileDataOnly() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--profiledata", anyProfileData } ); 
	
		assertEquals(CommandLineAction.CLEANUP, args.action);
		assertFalse(args.hasErrors());
		assertEquals(anySourceCode, args.sourceCode);
		assertEquals(anyProfileData, args.profileData);
	}
	
	@Test
	void testCreateWithLineRangeScopeStatement() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--linerange", "20-35",
				"--scope", "statement" } ); 
	
		assertEquals(CommandLineAction.CLEANUP, args.action);
		assertFalse(args.hasErrors());
		assertEquals(anySourceCode, args.sourceCode);
		assertEquals(20, args.cleanupRange.startLine);
		assertEquals(35, args.cleanupRange.lastLine);
		assertFalse(args.cleanupRange.expandRange);
		assertEquals(CleanupRangeExpandMode.FULL_STATEMENT, args.cleanupRangeExpandMode);
	}
	
	@Test
	void testCreateWithLineRangeScopeClass() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--linerange", "20-35",
				"--scope", "class" } ); 
	
		assertEquals(CommandLineAction.CLEANUP, args.action);
		assertFalse(args.hasErrors());
		assertEquals(anySourceCode, args.sourceCode);
		assertEquals(20, args.cleanupRange.startLine);
		assertEquals(35, args.cleanupRange.lastLine);
		assertTrue(args.cleanupRange.expandRange);
		assertEquals(CleanupRangeExpandMode.FULL_CLASS, args.cleanupRangeExpandMode);
	}
	
	@Test
	void testCreateWithLineRangeScopeDocument() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--linerange", "20-35",
				"--scope", "document" } ); 
	
		assertEquals(CommandLineAction.CLEANUP, args.action);
		assertFalse(args.hasErrors());
		assertEquals(anySourceCode, args.sourceCode);
		assertEquals(20, args.cleanupRange.startLine);
		assertEquals(35, args.cleanupRange.lastLine);
		assertTrue(args.cleanupRange.expandRange);
		assertEquals(CleanupRangeExpandMode.FULL_DOCUMENT, args.cleanupRangeExpandMode);
	}
	
	@Test
	void testCreateWithLineRangeScopeUser() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--linerange", "20-35",
				"--scope", "user" } ); 
	
		assertEquals(CommandLineAction.CLEANUP, args.action);
		assertFalse(args.hasErrors());
		assertEquals(anySourceCode, args.sourceCode);
		assertEquals(20, args.cleanupRange.startLine);
		assertEquals(35, args.cleanupRange.lastLine);
		assertTrue(args.cleanupRange.expandRange);
		// CleanupRangeExpandMode does not contain a "user" value; if it is null, the user settings from the UI will be used
		assertNull(args.cleanupRangeExpandMode);
	}
	
	@Test
	void testCreateErrorWithInvalidScope() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--linerange", "20-35",
				"--scope", "unknown" } ); 
	
		assertErrorsContain(args, "Invalid --scope");
		assertErrorsContain(args, "statement");
		assertErrorsContain(args, "method");
		assertErrorsContain(args, "class");
		assertErrorsContain(args, "document");
		assertErrorsContain(args, "user");
	}
	
	@Test
	void testCreateErrorWithMissingScope() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--linerange", "20-35",
				"--scope" } ); 
	
		assertErrorsContain(args, "Invalid --scope");
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
	void testCreateErrorWithMissingLineRange() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode, 
				"--scope", "class" } );
	
		assertNull(args.cleanupRange);
		assertErrorsContain(args, "Missing option");
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
	void testCreateErrorSourceFileAndSourceDir() {
		String sourcePath = persistency.getTempPath("any_source.txt");

		persistency.prepareFile(sourcePath, anySourceCode);
		persistency.prepareDirectory("src");

		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcefile", sourcePath, 
				"--sourcedir", "src" } );
		
		assertErrorsContain(args, "Source was supplied multiple times");
		assertErrorsContain(args, "--sourcefile");
		assertErrorsContain(args, "--source");
		assertErrorsContain(args, "--sourcedir");
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
				"--ui",
				"--targetfile", "any_target_file.txt", 
				"--partialresult" } );

		assertErrorsContain(args, "Invalid combination: --linerange");
		assertErrorsContain(args, "Invalid combination: --ui");
		assertErrorsContain(args, "Invalid combination: --targetfile");
		assertErrorsContain(args, "Invalid combination: --partialresult");
	}
	
	@Test
	void testCreateErrorWithProfilePathAndData() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		String profilePath = persistency.getTempPath("any_profile.cfj");
		persistency.prepareFile(sourcePath, anySourceCode);
		persistency.prepareFile(profilePath, anyProfileData);

		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode,
				"--profile", profilePath, 
				"--profiledata", anyProfileData } );

		assertErrorsContain(args, "Multiple profile arguments supplied");
	}
	
	@Test
	void testCreateErrorWithProfileDataAndName() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		persistency.prepareFile(sourcePath, anySourceCode);

		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode,
				"--profiledata", anyProfileData,
				"--profilename", anyProfileName } );

		assertErrorsContain(args, "Multiple profile arguments supplied");
	}
	
	@Test
	void testCreateErrorWithProfilePathAndLastProfile() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		String profilePath = persistency.getTempPath("any_profile.cfj");
		persistency.prepareFile(sourcePath, anySourceCode);
		persistency.prepareFile(profilePath, anyProfileData);

		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {  
				"--source", anySourceCode,
				"--profile", profilePath,
				"--last-profile" } );

		assertErrorsContain(args, "Multiple profile arguments supplied");
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
	
	@Test
	void testCreateErrorSourceFileAndPattern() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		persistency.prepareFile(sourcePath, anySourceCode);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcefile", sourcePath,
				"--filepattern", "Z*", 
				"--recursive"} );

		assertErrorsContain(args, "Invalid combination: --filepattern");
		assertErrorsContain(args, "Invalid combination: --recursive");
	}
	
	@Test
	void testCreateErrorSourceAndTargetDir() {
		String sourcePath = persistency.getTempPath("any_source.txt");
		persistency.prepareFile(sourcePath, anySourceCode);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--source", anySourceCode,
				"--targetdir", "any_dir"} );

		assertErrorsContain(args, "Invalid combination: --targetdir");
	}
	
	@Test
	void testCreateErrorInteractiveWithProfile() {
		String profilePath = persistency.getTempPath("any_profile.cfj");
		persistency.prepareFile(profilePath, anyProfileData);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--source", anySourceCode,
				"--profile", profilePath,				
				"--ui"} );

		assertErrorsContain(args, "Invalid combination: --profile");
	}
	
	@Test
	void testCreateErrorInteractiveWithExpandRangeMethod() {
		String profilePath = persistency.getTempPath("any_profile.cfj");
		persistency.prepareFile(profilePath, anyProfileData);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--source", anySourceCode,
				"--linerange", "20-35",
				"--scope", "method",
				"--ui"} );

		assertErrorsContain(args, "Invalid combination: --scope");
	}
	
	@Test
	void testCreateErrorInteractiveReadOnlyWithTargetPath() {
		String targetPath = persistency.getTempPath("any_target.txt");

		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--source", anySourceCode,
				"--ui", 
				"--readonly",
				"--targetfile", targetPath} );

		assertErrorsContain(args, "Invalid combination: --targetfile");
	}
	
	@Test
	void testCreateErrorInteractiveReadOnlyWithTargetDir() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--source", anySourceCode,
				"--ui", 
				"--readonly",
				"--targetdir", "any_dir"} );

		assertErrorsContain(args, "Invalid combination: --targetdir");
	}
	
	@Test
	void testCreateErrorInteractiveWithProfileData() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--source", anySourceCode,
				"--profiledata", anyProfileData,				
				"--ui"} );

		assertErrorsContain(args, "Invalid combination: --profiledata");
	}
	
	@Test
	void testCreateErrorInteractiveWithStats() {
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--source", anySourceCode,
				"--ui",
				"--stats",
				"--usedrules"} );

		assertErrorsContain(args, "Invalid combination: --stats");
		assertErrorsContain(args, "Invalid combination: --usedrules");
	}
	
	@Test
	void testCreateErrorSimulateAndTargetFile() {
		persistency.prepareFile("src", "any_source.abap", anySourceCode);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcefile", "src",
				"--simulate",
				"--targetfile", "any_target.abap", "--overwrite"} );

		assertErrorsContain(args, "Invalid combination: --targetfile");
		assertErrorsContain(args, "Invalid combination: --overwrite");
	}
	
	@Test
	void testCreateErrorSimulateAndTargetDir() {
		persistency.prepareFile("src", "any_source.abap", anySourceCode);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcedir", "src",
				"--simulate",
				"--targetdir", "tgt"} );

		assertErrorsContain(args, "Invalid combination: --targetdir");
	}
	
	@Test
	void testCreateErrorFilePatternAsteriskMissing() {
		persistency.prepareFile("src", "any_source.abap", anySourceCode);
		
		CommandLineArgs args = CommandLineArgs.create(persistency, new String[] {
				"--sourcedir", "src",
				"--filepattern", "\".txt\"",
				"--simulate",
				"--targetdir", "tgt"} );

		assertErrorsContain(args, "File pattern must contain an asterisk");
		assertErrorsContain(args, "--filepattern");
	}
}
