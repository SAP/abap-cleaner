package com.sap.adt.abapcleaner.programbase;

import static org.junit.jupiter.api.Assertions.*;

import java.time.LocalDate;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.StringUtil;

public class ProgramTest {
	private PersistencyDouble persistency;
	private String workDir;
	
	@BeforeEach
	void setup() {
		persistency = PersistencyDouble.create();
		Program.initialize(persistency, "");

		workDir = persistency.getWorkDir();
	}

	@Test
	void testGetReleases() {
		Release[] releases = Program.getReleases();
		assertNotNull(releases);
		assertTrue(releases.length > 0);
		
		LocalDate tomorrow = LocalDate.now().plusDays(1);
		
		Release lastRelease = null;
		for (Release release : releases) {
			assertFalse(StringUtil.isNullOrEmpty(release.toString()));
			
			assertTrue(release.wasEarlierThan(tomorrow.getYear(), tomorrow.getMonthValue(), tomorrow.getDayOfMonth()));
			if (lastRelease != null) {
				assertTrue(release.wasEarlierThan(lastRelease.releaseDate));
			}
			
			lastRelease = release;
		}
	}
	
	@Test
	void testGetVersion() {
		// outside ADT, this returns "" (but not null)
		assertNotNull(StringUtil.isNullOrEmpty(Program.getVersion()));
	}

	@Test
	void testShowDevFeatures() {
		Program.initialize(persistency, workDir);
		assertFalse(Program.showDevFeatures());

		persistency.prepareFile(workDir, "devfeatures", "");
		Program.initialize(persistency, workDir);
		assertTrue(Program.showDevFeatures());
	}

	@Test
	void testGetAboutText() {
		String aboutText = Program.getAboutText();
		assertFalse(StringUtil.isNullOrEmpty(aboutText));
		assertTrue(aboutText.indexOf(Program.PRODUCT_NAME) >= 0);
		assertTrue(aboutText.indexOf(Program.CONTACT_URL) >= 0);
	}
}
