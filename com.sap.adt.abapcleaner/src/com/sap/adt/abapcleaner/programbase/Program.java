package com.sap.adt.abapcleaner.programbase;

import java.io.File;
import java.security.CodeSource;

import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

import com.sap.adt.abapcleaner.base.FileSystem;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.ITokenTypeRefiner;
import com.sap.adt.abapcleaner.parser.TokenTypeRefinerRnd;

public final class Program {
	/** Technical version used in settings files. */
	public static final int TECHNICAL_VERSION = 26;

	public static final String PRODUCT_NAME = "ABAP cleaner";
	
	private static final String APP_DATA_COMPANY_FOLDER_WIN = "SAP";
	private static final String APP_DATA_FOLDER_WIN = "ABAP cleaner";
	private static final String APP_DATA_FOLDER_MAC = "com.sap.adt.abapcleaner";
	private static final String APP_DATA_FOLDER_LINUX = "ABAP cleaner";

	public static final String CONTACT_URL = "https://github.com/SAP/abap-cleaner";  
	public static final String DOCUMENTATION_BASE_URL = "https://github.com/SAP/abap-cleaner/blob/main/";

	private static boolean showDevFeatures;
	private static Log log;
	private static ITokenTypeRefiner tokenTypeRefiner;
	private static boolean wasInitialized;
	
	public static boolean showDevFeatures() { return showDevFeatures; }

	public static boolean testDdl() { return showDevFeatures; } // experimental

	public static boolean wasInitialized() { return wasInitialized; }
	
	public static void initialize(Persistency persistency, String overrideWorkDir) {
		if (persistency == null)
			persistency = Persistency.create(FileSystem.create());

		CodeSource codeSource = Program.class.getProtectionDomain().getCodeSource();
		String startupPath = new File(codeSource.getLocation().getPath()).getParent(); 

		String workDir = overrideWorkDir;
		if (StringUtil.isNullOrEmpty(workDir)) {
			workDir = persistency.getAppDataDir(APP_DATA_COMPANY_FOLDER_WIN, APP_DATA_FOLDER_WIN, APP_DATA_FOLDER_MAC, APP_DATA_FOLDER_LINUX, startupPath);
		}
		persistency.initialize(workDir, startupPath);
		persistency.ensureDirectoryExists(workDir);

		showDevFeatures = persistency.fileExists(workDir, "devfeatures"); 
		log = null;
		wasInitialized = false;
	}
	
	public static String getVersion() {
		Bundle bundle = FrameworkUtil.getBundle(Program.class); 
		return (bundle == null) ? "" : bundle.getVersion().toString();
	}
	
	public static Release[] getReleases() {
		return new Release[] { 
				Release.create("1.20.0", 2024, 10, 19), 
				Release.create("1.19.0", 2024,  9, 18), 
				Release.create("1.18.0", 2024,  7,  8), 
				Release.create("1.17.0", 2024,  6, 24), 
				Release.create("1.16.0", 2024,  4, 12), 
				Release.create("1.15.0", 2024,  3, 25), 
				Release.create("1.14.0", 2024,  3, 11), 
				Release.create("1.13.0", 2024,  1,  5), 
				Release.create("1.12.0", 2023, 11, 27), 
				Release.create("1.11.0", 2023, 11,  6), 
				Release.create("1.10.0", 2023, 11,  1), 
				Release.create("1.9.0",  2023, 10, 30), 
				Release.create("1.8.0",  2023, 10, 23), 
				Release.create("1.7.0",  2023, 10, 10), 
				Release.create("1.6.0",  2023, 10,  2), 
				Release.create("1.5.0",  2023,  7,  5), 
				Release.create("1.4.0",  2023,  6, 12), 
				Release.create("1.3.0",  2023,  6,  6), 
				Release.create("1.2.0",  2023,  5, 22), 
				Release.create("1.1.0",  2023,  5,  7), 
				Release.create("1.0.0",  2023,  4, 21) 
		};
	}

	public static Log getLog() {
		if (log == null) 
			log = Log.create(Persistency.get().getSavePath(FileType.ERROR_LOG));
		return log; 
	}

	public static void setLogForTesting() {
		if (log != null)
			log.flush();
		log = Log.createNonPersistentForTesting();
	}

	public static ITokenTypeRefiner getTokenTypeRefiner() {
		if (tokenTypeRefiner == null)
			tokenTypeRefiner = TokenTypeRefinerRnd.create();
		return tokenTypeRefiner; 
	}
	
	public static TokenTypeRefinerRnd getRndParser() {
		ITokenTypeRefiner refiner = getTokenTypeRefiner();
		if (refiner != null && refiner instanceof TokenTypeRefinerRnd) { 
			return (TokenTypeRefinerRnd)refiner;
		} else {
			return null;
		}
	}
	
	public static String getAboutText() {
		final String lineSep = System.lineSeparator();

		StringBuilder text = new StringBuilder();
		text.append(Program.PRODUCT_NAME);
	
		String version = Program.getVersion();
		if (!StringUtil.isNullOrEmpty(version)) {
			text.append(lineSep).append("Version ").append(version);
		}
		text.append(lineSep);
		text.append(lineSep).append("Contact: ").append(Program.CONTACT_URL);

		return text.toString();
	}
}
