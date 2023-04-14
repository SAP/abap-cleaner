package com.sap.adt.abapcleaner.programbase;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class ConfigTest {
	private PersistencyDouble persistency;
	private String workDir;
	private String configPath;

	@BeforeEach
	void setup() {
		persistency = PersistencyDouble.create();
		Program.initialize(persistency, "");
		workDir = persistency.getWorkDir();
		configPath = persistency.getSavePath(FileType.CONFIG_TEXT);
	}

	@Test 
	void testSaveAndLoad() {
		Config config = new Config(workDir);

		// 1. load non-existing file, expecting default settings
		Config config2 = new Config(workDir);
		config2.load(persistency, configPath);
		assertEquals(config.getFolderPrioritiesSettings(), config2.getFolderPrioritiesSettings());
		
		// 2. save Config to file
		config.save(persistency, configPath);
		
		// 3. load another Config instance from file
		Config config3 = new Config(workDir);
		config3.load(persistency, configPath);
		assertEquals(config.getFolderPrioritiesSettings(), config3.getFolderPrioritiesSettings());
	}
}
