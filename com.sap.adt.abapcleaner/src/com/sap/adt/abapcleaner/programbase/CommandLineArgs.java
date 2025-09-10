package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.CleanupRange;

public class CommandLineArgs {
	private static final String LINE_SEP = System.lineSeparator();

	// options for help or version info
	private static final String OPT_HELP_WINDOWS = "/?";
	private static final String OPT_HELP_LINUX = "/man";
	private static final String OPT_VERSION = "--version";

	// options for cleanup
	// - input (single file)
	private static final String OPT_SOURCE_FILE = "--sourcefile";
	private static final String OPT_SOURCE_CODE = "--source";
	private static final String OPT_LINE_RANGE = "--linerange";
	// - input (multiple files)
	private static final String OPT_SOURCE_DIR = "--sourcedir";
	private static final String OPT_FILE_FILTER = "--filepattern";
	private static final String OPT_RECURSIVE = "--recursive";
	
	// - cleanup
	private static final String OPT_PROFILE = "--profile";
	private static final String OPT_PROFILE_DATA = "--profiledata";
	private static final String OPT_RELEASE = "--release";

	// - output
	private static final String OPT_SIMULATE = "--simulate";
	private static final String OPT_TARGET_FILE = "--targetfile";
	private static final String OPT_PARTIAL_RESULT = "--partialresult";
	private static final String OPT_TARGET_DIR = "--targetdir";
	private static final String OPT_OVERWRITE = "--overwrite";
	private static final String OPT_CRLF = "--crlf";

	// - statistics
	private static final String OPT_STATS = "--stats";
	private static final String OPT_USED_RULES = "--usedrules";

	private static final String[] allOptions = new String[] { 
			OPT_SOURCE_FILE, OPT_SOURCE_CODE, OPT_LINE_RANGE, OPT_SOURCE_DIR, OPT_FILE_FILTER, OPT_RECURSIVE, 
			OPT_PROFILE, OPT_PROFILE_DATA, OPT_RELEASE, 
			OPT_TARGET_FILE, OPT_PARTIAL_RESULT, OPT_TARGET_DIR, OPT_OVERWRITE, OPT_CRLF, 
			OPT_STATS, OPT_USED_RULES };

	private static final String EXECUTABLE_NAME = ".\\abap-cleanerc.exe"; 
	private static final char LINE_RANGE_SEP = '-';
	private static final String LINE_RANGE_EXAMPLE = "\"20-35\"";

	private static final String DEFAULT_ABAP_FILE_PATTERN = "*.abap"; 
	private static final String INVALID_OPTION_COMBO_FORMAT = "Invalid combination: %s cannot be used together with %s";

	private static final int OPTIONS_INDENT = 4;
	private static final int OPTIONS_LINE_PREFIX_LENGTH = 20; // must be at least the length of the longest OPT_ + 1

	private static final String[] optionsRequiringNextArg = new String[] { 
			OPT_SOURCE_FILE, OPT_SOURCE_CODE, OPT_LINE_RANGE, OPT_SOURCE_DIR, OPT_FILE_FILTER,
			OPT_PROFILE, OPT_PROFILE_DATA, OPT_RELEASE,
			OPT_TARGET_FILE, OPT_TARGET_DIR };

	public static String[] getAllOptions() { return allOptions; }
	
	@SuppressWarnings("unused")
	public static CommandLineArgs create(Persistency persistency, String[] args) {
		final String LINE_SEP = System.lineSeparator();

		if (args == null || args.length == 0)
			return null;

		StringBuilder errors = new StringBuilder();

		// check whether help or version info is requested
		if (args.length == 1 && (args[0].equals(OPT_HELP_WINDOWS) || args[0].equals(OPT_HELP_LINUX))) {
			return new CommandLineArgs(CommandLineAction.SHOW_HELP);

		} else if (args.length == 1 && args[0].equals(OPT_VERSION)) {
			return new CommandLineArgs(CommandLineAction.SHOW_VERSION);
		}

		// in all other cases, cleanup is requested:
		// - input options (single file)
		String sourceName = null; 
		String sourceCode = null;
		CleanupRange cleanupRange = null;
		// - input options (multiple files)
		String sourceDir = null;
		String[] sourcePaths = null;
		String fileFilter = null;
		boolean recursive = false;
		
		// - cleanup options
		String profileData = null;
		String abapRelease = null;

		// - output options
		boolean simulate = false;
		String targetPath = null; 
		boolean partialResult = false;
		String targetDir = null;
		boolean overwrite = false;
		String lineSeparator = ABAP.LINE_SEP_FOR_COMMAND_LINE;
		
		// - statistics options 
		boolean showStats = false;
		boolean showUsedRules = false;

		for (int i = 0; i < args.length; ++i) {
			String arg = args[i];
			
			// check whether the next argument is required and supplied
			String nextArg = null;
			for (String optRequiringNextArg : optionsRequiringNextArg) {
				if (arg.equals(optRequiringNextArg)) {
					nextArg = (i + 1 >= args.length) ? null : args[i + 1];
					if (StringUtil.isNullOrEmpty(nextArg)) 
						errors.append("Argument missing after " + optRequiringNextArg).append(LINE_SEP);
					break;
				}
			}

			// - input options (single file)
			if (arg.equals(OPT_SOURCE_FILE) || arg.equals(OPT_SOURCE_CODE)) {
				if (sourceCode != null) {
					errors.append("Source code supplied twice; please use " + OPT_SOURCE_FILE + " or " + OPT_SOURCE_CODE + " only once.").append(LINE_SEP);
				} else if (arg.equals(OPT_SOURCE_CODE)) {
					sourceCode = nextArg;
				} else if (persistency.fileExists(nextArg)) {
					sourceName = persistency.getFileNameWithoutExtension(nextArg);
					sourceCode = persistency.readAllTextFromFile(nextArg);
				} else {
					errors.append("File not found: " + nextArg).append(LINE_SEP);
				}

			} else if (arg.equals(OPT_LINE_RANGE)) {
				String lineRange = (nextArg == null) ? "" : nextArg;
				int sepPos = lineRange.indexOf(LINE_RANGE_SEP);
				int startLine = (sepPos <= 0) ? -1 : Integer.valueOf(lineRange.substring(0, sepPos));
				int lastLine = (sepPos < 0 || sepPos + 1 >= lineRange.length()) ? -1 : Integer.valueOf(lineRange.substring(sepPos + 1));
				if (startLine <= 0 || lastLine <= 0 || startLine > lastLine) {
					errors.append("Invalid " + OPT_LINE_RANGE + ": Expected format \"m-n\" (1-based), e.g. " + LINE_RANGE_EXAMPLE).append(LINE_SEP);
				} else {
					cleanupRange = CleanupRange.create(startLine, lastLine, true);
				}

				// - input options (multiple files)
			} else if (arg.equals(OPT_SOURCE_DIR)) {
				if (!persistency.directoryExists(nextArg)) {
					errors.append("Source directory " + nextArg + " does not exist!").append(LINE_SEP);
				} else {
					sourceDir = persistency.getAbsolutePath(nextArg);
				}

			} else if (arg.equals(OPT_FILE_FILTER)) {
				if (nextArg == null || nextArg.indexOf("*") < 0) {
					errors.append("File pattern must contain an asterisk, e.g. " + OPT_FILE_FILTER + " \"*.abap\"").append(LINE_SEP);
				} else {
					fileFilter = nextArg;
				}

			} else if (arg.equals(OPT_RECURSIVE)) {
				recursive = true;

				// - cleanup options
			} else if (arg.equals(OPT_PROFILE) || arg.equals(OPT_PROFILE_DATA)) {
				if (profileData != null) {
					errors.append("Profile supplied twice; please use " + OPT_PROFILE + " or " + OPT_PROFILE_DATA + " only once.").append(LINE_SEP);
				} else if (arg.equals(OPT_PROFILE_DATA)) {
					profileData = nextArg;
				} else if (persistency.fileExists(nextArg)) { 
					profileData = persistency.readAllTextFromFile(nextArg);
				} else {
					errors.append("File not found: " + nextArg).append(LINE_SEP);
				}

			} else if (arg.equals(OPT_RELEASE)) {
				abapRelease = nextArg;

				// - output options
			} else if (arg.equals(OPT_SIMULATE)) {
				simulate = true;

			} else if (arg.equals(OPT_TARGET_FILE)) {
				targetPath = nextArg;

			} else if (arg.equals(OPT_PARTIAL_RESULT)) {
				partialResult = true;

			} else if (arg.equals(OPT_TARGET_DIR)) {
				targetDir = persistency.getAbsolutePath(nextArg);

			} else if (arg.equals(OPT_OVERWRITE)) {
				overwrite = true;

			} else if (arg.equals(OPT_CRLF)) {
				lineSeparator = "\r\n";

				// - statistics options
			} else if (arg.equals(OPT_STATS)) {
				showStats = true;

			} else if (arg.equals(OPT_USED_RULES)) {
				showUsedRules = true;

			} else {
				errors.append("Unknown option: " + arg).append(LINE_SEP);
			}
			
			// skip next argument, since it was already consumed above
			if (nextArg != null) {
				++i;
			}
		}
		
		// check input options
		if (sourceDir != null && sourceCode != null) {
			errors.append("Source was supplied multiple times; please use " + OPT_SOURCE_FILE + " or " + OPT_SOURCE_CODE + " or " + OPT_SOURCE_DIR + " only once.").append(LINE_SEP);
		}
		
		// check whether input options (multiple files) match cleanup and output options
		if (!StringUtil.isNullOrEmpty(sourceDir)) {
			if (!simulate && StringUtil.isNullOrEmpty(targetDir)) {
				targetDir = sourceDir;
			}
			if (cleanupRange != null)
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_LINE_RANGE, OPT_SOURCE_DIR)).append(LINE_SEP);
			if (targetPath != null)
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_TARGET_FILE, OPT_SOURCE_DIR)).append(LINE_SEP);
			if (partialResult) 
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_PARTIAL_RESULT, OPT_SOURCE_DIR)).append(LINE_SEP);
				
			sourcePaths = persistency.getFilesInDirectory(sourceDir, StringUtil.isNullOrEmpty(fileFilter) ? DEFAULT_ABAP_FILE_PATTERN : fileFilter, recursive);
			if (sourcePaths == null || sourcePaths.length == 0) {
				errors.append("No matching files found in given source directory: " + sourceDir).append(LINE_SEP);
			}
		}

		// check output options
		if (simulate) {
			if (targetPath != null) 
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_TARGET_FILE, OPT_SIMULATE)).append(LINE_SEP);
			if (targetDir != null) 
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_TARGET_DIR, OPT_SIMULATE)).append(LINE_SEP);
			if (overwrite) 
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_OVERWRITE, OPT_SIMULATE)).append(LINE_SEP);
			// partialResult can be tolerated
		
		} else  if (!overwrite) {
			if (!StringUtil.isNullOrEmpty(targetPath) && persistency.fileExists(targetPath)) {
				errors.append("Target file already exists; please use " + OPT_OVERWRITE + " to allow overwriting: " + targetPath).append(LINE_SEP);
			} else if (!StringUtil.isNullOrEmpty(targetDir) && persistency.directoryExists(targetDir) && sourcePaths != null) {
				// check if there is a source file that already exists in the target directory
				for (String sourcePath : sourcePaths) {
					if (persistency.fileExists(persistency.combinePaths(targetDir, sourcePath.substring(sourceDir.length())))) {
						errors.append("Source file " + sourcePath + " already exists in the target directory; please use " + OPT_OVERWRITE + " to allow overwriting: " + targetDir).append(LINE_SEP);
						break;
					}
				}
			}
		}
		
		if (sourceCode != null) {
			// single file
			return new CommandLineArgs(errors.toString(), sourceName, sourceCode, cleanupRange, profileData, abapRelease, 
												simulate, targetPath, partialResult, overwrite, lineSeparator, showStats, showUsedRules);
		} else {
			// multiple files
			return new CommandLineArgs(errors.toString(), sourceDir, sourcePaths, profileData, abapRelease, 
												simulate, targetDir, overwrite, lineSeparator, showStats, showUsedRules);
		}
	}

	public static String getHelp(Persistency persistency) {
		String profileExtension = persistency.getExtension(FileType.PROFILE_TEXT);

		StringBuilder sb = new StringBuilder();

		String usagePrefix = "    " + EXECUTABLE_NAME;
		String spacePrefix = StringUtil.repeatChar(' ', usagePrefix.length());

		sb.append("Shop help or version information:");
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_HELP_WINDOWS);
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_HELP_LINUX);
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_VERSION);
		sb.append(LINE_SEP + LINE_SEP);

		sb.append("Cleanup of single file:");
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" {" + OPT_SOURCE_FILE + " sourcefile");
		sb.append(" / " + OPT_SOURCE_CODE + " sourcecode }");
		sb.append(" [" + OPT_LINE_RANGE + " linerange]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [{ " + OPT_PROFILE + " profile");
		sb.append(" / " + OPT_PROFILE_DATA + " profiledata }]");
		sb.append(" [" + OPT_RELEASE + " release]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [" + OPT_CRLF + "]");
		sb.append(" [" + OPT_TARGET_FILE + " targetfile");
		sb.append(" [" + OPT_OVERWRITE + "]]");
		sb.append(" [" + OPT_PARTIAL_RESULT + "]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [" + OPT_STATS + "]");
		sb.append(" [" + OPT_USED_RULES + "]");
		sb.append(LINE_SEP + LINE_SEP);

		sb.append("Example for cleanup of single file:");
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_SOURCE_FILE + " \"CL_ANY_CLASS.txt\"");
		sb.append(" " + OPT_LINE_RANGE + " " + LINE_RANGE_EXAMPLE);
		sb.append(" " + OPT_PROFILE + " \"" + "team profile" + profileExtension + "\"");
		sb.append(" " + OPT_RELEASE + " \"757\"");
		sb.append(" " + OPT_TARGET_FILE + " \"result\\CL_ANY_CLASS.txt\"");
		sb.append(" " + OPT_OVERWRITE);
		sb.append(" " + OPT_STATS);
		sb.append(" " + OPT_USED_RULES);
		sb.append(LINE_SEP + LINE_SEP);

		sb.append("Cleanup of multiple files:");
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_SOURCE_DIR + " sourcedir");
		sb.append(" [" + OPT_FILE_FILTER + " filepattern]");
		sb.append(" [" + OPT_RECURSIVE + "]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [{ " + OPT_PROFILE + " profile");
		sb.append(" / " + OPT_PROFILE_DATA + " profiledata }]");
		sb.append(" [" + OPT_RELEASE + " release]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [" + OPT_TARGET_DIR + " targetdir");
		sb.append(" [" + OPT_OVERWRITE + "]]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [" + OPT_STATS + "]");
		sb.append(" [" + OPT_USED_RULES + "]");
		sb.append(LINE_SEP + LINE_SEP);

		sb.append("Example for cleanup of multiple files:");
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_SOURCE_DIR + " \"C:\\temp\\source\"");
		sb.append(" " + OPT_FILE_FILTER + " \"*.txt\"");
		sb.append(" " + OPT_RECURSIVE);
		sb.append(" " + OPT_PROFILE + " \"" + "team profile" + profileExtension + "\"");
		sb.append(" " + OPT_RELEASE + " \"757\"");
		sb.append(" " + OPT_TARGET_DIR + " \"C:\\temp\\target\"");
		sb.append(" " + OPT_OVERWRITE);
		sb.append(LINE_SEP + LINE_SEP);

		sb.append("Options for cleanup: ");
		sb.append(LINE_SEP);
		sb.append(getOptionHelp(OPT_SOURCE_FILE, "File name of an ABAP source file which is input to the cleanup."));
		sb.append(getOptionHelp(OPT_SOURCE_CODE, "ABAP source code which is input to the cleanup."));
		sb.append(getOptionHelp(null, "Please use either " + OPT_SOURCE_FILE + " or " + OPT_SOURCE_CODE + " or " + OPT_SOURCE_DIR + "."));
		sb.append(getOptionHelp(OPT_LINE_RANGE, "Single line range for partial cleanup, e.g. " + LINE_RANGE_EXAMPLE));
		sb.append(getOptionHelp(null, "Without this option, the cleanup will be applied to the whole code document."));
		sb.append(LINE_SEP);
		sb.append(getOptionHelp(OPT_SOURCE_DIR, "Folder that contains ABAP source files (default file pattern is \"*.abap\")"));
		sb.append(getOptionHelp(OPT_FILE_FILTER, "File pattern to look for (only relevant when " + OPT_SOURCE_DIR + " has been supplied)"));
		sb.append(getOptionHelp(OPT_RECURSIVE, "Searches provided source directory recursively for ABAP files"));
		sb.append(LINE_SEP);
		sb.append(getOptionHelp(OPT_PROFILE, "File name of the cleanup profile to be used (extension " + profileExtension + ")."));
		sb.append(getOptionHelp(null, "From the UI, you may use button 'Export...' from the profiles editor to create the file."));
		sb.append(getOptionHelp(OPT_PROFILE_DATA, "Content of the cleanup profile to be used."));
		sb.append(getOptionHelp(null, "Please use either " + OPT_PROFILE + " or " + OPT_PROFILE_DATA + " (or none for program defaults)."));
		sb.append(getOptionHelp(OPT_RELEASE, "ABAP release to restrict syntax of cleanup changes, e.g. \"758\""));
		sb.append(getOptionHelp(null, "Without this option, the latest ABAP syntax will be allowed."));
		sb.append(LINE_SEP);
		sb.append(getOptionHelp(OPT_TARGET_FILE, "Target file name to which the cleanup result will be saved."));
		sb.append(getOptionHelp(null, "Without this option, the cleanup result will be written to the standard output."));
		sb.append(getOptionHelp(OPT_TARGET_DIR, "Target directory name to which the cleanup files will be saved"));
		sb.append(getOptionHelp(null, "If not supplied, " + OPT_SOURCE_DIR + " will be the target diretory" ));
		sb.append(getOptionHelp(OPT_OVERWRITE, "Overwrite target file if it already exists."));
		sb.append(getOptionHelp(null, "Without this option, an error will be raised if the target file already exists."));
		sb.append(getOptionHelp(OPT_PARTIAL_RESULT, "Restrict output to the cleanup result of the " + OPT_LINE_RANGE + " (if supplied)."));
		sb.append(getOptionHelp(null, "Without this option, the cleanup result of whole code document will be returned."));
		sb.append(getOptionHelp(OPT_CRLF, "Use CRLF = \"\\r\\n\" as line separator (default: LF = \"\\n\")."));
		sb.append(getOptionHelp(OPT_SIMULATE, "Run cleanup without writing the result code to a file or to standard output."));
		sb.append(getOptionHelp(null, "Use this option to check the potential effect of cleanup with " + OPT_STATS + " or " + OPT_USED_RULES + "."));
		sb.append(LINE_SEP);
		sb.append(getOptionHelp(OPT_STATS, "Write statistical summary to standard output."));
		sb.append(getOptionHelp(OPT_USED_RULES, "Write list of used rules to standard output."));

		return sb.toString();
	}

	private static String getOptionHelp(String option, String text) {
		return getOptionsLinePrefix(option) + text + LINE_SEP;
	}
	
	private static String getOptionsLinePrefix(String option) {
		if (option == null) {
			return StringUtil.repeatChar(' ', OPTIONS_INDENT + OPTIONS_LINE_PREFIX_LENGTH);
		} else {
			return StringUtil.repeatChar(' ', OPTIONS_INDENT) + option + StringUtil.repeatChar(' ', OPTIONS_LINE_PREFIX_LENGTH - option.length());
		}
	}
	
	// -------------------------------------------------------------------------

	public final CommandLineAction action;
	public final String errors;

	// - input (single file)
	/** may be null if the code is supplied directly as a command line argument; otherwise, the file name without extension */
	public final String sourceName;
	public final String sourceCode;
	public final CleanupRange cleanupRange;
	// - input (multiple files)
	public final String sourceDir;
	public final String[] sourcePaths;

	// - cleanup
	public final String profileData;
	public final String abapRelease;
	
	// - output
	public final boolean simulate;
	public final String targetPath; 
	public final boolean partialResult;
	public final String targetDir;
	public final boolean overwrite;
	public final String lineSeparator;
	
	// - statistics
	public final boolean showStats;
	public final boolean showUsedRules;

	public boolean hasErrors() { return !StringUtil.isNullOrEmpty(errors); }
	
	public boolean isInSingleSourceMode() { return sourceDir == null; }

	public boolean writesResultCodeToOutput() { return !simulate && isInSingleSourceMode() && StringUtil.isNullOrEmpty(targetPath); }

	public boolean showStatsOrUsedRules() { return showStats || showUsedRules; }
	
	private CommandLineArgs(CommandLineAction action) {
		this.action = action;
		this.errors = null;
		
		this.sourceName = null;
		this.sourceCode = null;
		this.cleanupRange = null;
		this.sourceDir = null;
		this.sourcePaths = null;

		this.profileData = null;
		this.abapRelease = null;

		this.simulate = false;
		this.targetPath = null;
		this.partialResult = false;
		this.targetDir = null;
		this.overwrite = false;
		this.lineSeparator = null;

		this.showStats = false;
		this.showUsedRules = false;
	}
	
	private CommandLineArgs(
			String errors, 
			String sourceName, String sourceCode, CleanupRange cleanupRange, 
			String profileData, String abapRelease, 
			boolean simulate, String targetPath, boolean partialResult, boolean overwrite, String lineSeparator, 
			boolean showStats, boolean showUsedRules) {

		this.action = CommandLineAction.CLEANUP;
		this.errors = errors;
		
		this.sourceName = sourceName; 
		this.sourceCode = sourceCode;
		this.cleanupRange = cleanupRange;
		this.sourceDir = null;
		this.sourcePaths = null;

		this.profileData = profileData;
		this.abapRelease = abapRelease;

		this.simulate = simulate;
		this.targetPath = targetPath;
		this.partialResult = partialResult;
		this.targetDir = null;
		this.overwrite = overwrite;
		this.lineSeparator = lineSeparator;

		this.showStats = showStats;
		this.showUsedRules = showUsedRules;
	}

	private CommandLineArgs(
			String errors,
			String sourceDir, String[] sourcePaths,
			String profileData, String abapRelease, 
			boolean simulate, String targetDir, boolean overwrite, String lineSeparator, 
			boolean showStats, boolean showUsedRules) {

		this.action = CommandLineAction.CLEANUP;
		this.errors = errors;

		this.sourceName = null;
		this.sourceCode = null;
		this.cleanupRange = null;
		this.sourceDir = sourceDir;
		this.sourcePaths = sourcePaths;

		this.profileData = profileData;
		this.abapRelease = abapRelease;

		this.simulate = simulate;
		this.targetPath = null;
		this.partialResult = false;
		this.targetDir = targetDir;
		this.overwrite = overwrite;
		this.lineSeparator = lineSeparator;

		this.showStats = showStats;
		this.showUsedRules = showUsedRules;
	}
}
