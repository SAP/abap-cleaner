package com.sap.adt.abapcleaner.gui;

import com.sap.adt.abapcleaner.base.MarkdownBuilder;
import com.sap.adt.abapcleaner.programbase.FileType;
import com.sap.adt.abapcleaner.programbase.HelpTopic;
import com.sap.adt.abapcleaner.programbase.Persistency;
import com.sap.adt.abapcleaner.programbase.Program;

public class ProgramLauncher {
	public static final void showHelp(HelpTopic helpTopic) {
		startProcess(Program.DOCUMENTATION_BASE_URL + helpTopic.getPageName() + MarkdownBuilder.MARKDOWN_EXTENSION);
		// for local help files:
		// Persistency persistency = Persistency.get(); 
		// String path = persistency.getLoadPath(FileType.HELP, 0, helpTopic.toString());
		// if (persistency.fileExists(path))
		// 	startProcess(path);
	}

	public final void openDirectories(FileType fileType) {
		Persistency persistency = Persistency.get(); 
		String[] dirs = persistency.getExistingDirs(fileType);
		if (dirs != null) {
			for (String dir : dirs) {
				persistency.ensureDirectoryExists(dir);
				startProcess(dir);
			}
		}
	}

	public static void startProcess(String path) {
		org.eclipse.swt.program.Program.launch(path);
	}
}
