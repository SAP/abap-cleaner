package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.CleanupRange;

public class CommandLineArgs {
	private static final String LINE_SEP = System.lineSeparator();

	private static final String OPT_SOURCE_FILE = "--sourcefile";
	private static final String OPT_SOURCE_CODE = "--source";
	private static final String OPT_LINE_RANGE = "--linerange";
	private static final String OPT_TARGET_FILE = "--targetfile";

	private static final String OPT_SOURCE_DIR = "--sourcedir";
	private static final String OPT_RECURSIVE = "--recursive";
	private static final String OPT_TARGET_DIR = "--targetdir";
	private static final String OPT_FILE_FILTER = "--filepattern";

	private static final String OPT_PROFILE = "--profile";
	private static final String OPT_PROFILE_DATA = "--profiledata";
	private static final String OPT_RELEASE = "--release";
	private static final String OPT_OVERWRITE = "--overwrite";
	private static final String OPT_PARTIAL_RESULT = "--partialresult";
	private static final String OPT_STATS = "--stats";
	private static final String OPT_USED_RULES = "--usedrules";

	private static final String[] allOptions = new String[] { OPT_SOURCE_FILE, OPT_SOURCE_CODE, OPT_LINE_RANGE, OPT_TARGET_FILE, OPT_SOURCE_DIR, OPT_RECURSIVE, OPT_TARGET_DIR, OPT_FILE_FILTER, OPT_PROFILE, OPT_PROFILE_DATA, OPT_RELEASE, OPT_OVERWRITE, OPT_PARTIAL_RESULT, OPT_STATS, OPT_USED_RULES };

	private static final String EXECUTABLE_NAME = ".\\abap-cleanerc.exe"; 
	private static final String OPT_HELP_WINDOWS = "/?";
	private static final String OPT_HELP_LINUX = "/man";
	private static final char LINE_RANGE_SEP = '-';
	private static final String LINE_RANGE_EXAMPLE = "\"20-35\"";

	private static final String DEFAULT_ABAP_FILE_PATTERN = "*.abap"; 
	private static final String INVALID_OPTION_COMBO_FORMAT = "Invalid combination: %s cannot be used together with %s";

	private static final int OPTIONS_INDENT = 4;
	private static final int OPTIONS_LINE_PREFIX_LENGTH = 20; // must be at least the length of the longest OPT_ + 1

	private static final String[] optionsRequiringNextArg = new String[] { OPT_SOURCE_FILE, OPT_SOURCE_CODE, OPT_LINE_RANGE, OPT_TARGET_FILE, OPT_SOURCE_DIR, OPT_TARGET_DIR, OPT_FILE_FILTER, OPT_RELEASE, OPT_PROFILE, OPT_PROFILE_DATA };

	public static String[] getAllOptions() { return allOptions; }
	
	public static CommandLineArgs create(Persistency persistency, String[] args) {
		if (args == null || args.length == 0)
			return null;

		final String LINE_SEP = System.lineSeparator();
		
		String sourceCode = null;
		String targetPath = null; 
		CleanupRange cleanupRange = null;

		String[] sourcePaths = null;
		boolean recursive = false;
		String sourceDir = null;
		String targetDir = null;
		String fileFilter = null;

		String profileData = null;
		String abapRelease = null;
		boolean overwrite = false;
		boolean partialResult = false;
		boolean showStats = false;
		boolean showUsedRules = false;
		StringBuilder errors = new StringBuilder();
		boolean showHelp = false;

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

			if (arg.equals(OPT_SOURCE_FILE) || arg.equals(OPT_SOURCE_CODE)) {
				if (sourceCode != null) {
					errors.append("Source code supplied twice; please use " + OPT_SOURCE_FILE + " or " + OPT_SOURCE_CODE + " only once.").append(LINE_SEP);
				} else if (arg.equals(OPT_SOURCE_CODE)) {
					sourceCode = nextArg;
				} else if (persistency.fileExists(nextArg)) {
					sourceCode = persistency.readAllTextFromFile(nextArg);
				} else {
					errors.append("File not found: " + nextArg).append(LINE_SEP);
				}

			} else if (arg.equals(OPT_SOURCE_DIR)) {
				if (!persistency.directoryExists(nextArg)) {
					errors.append("Source directory " + nextArg + " does not exist!");
				} else {
					sourceDir = persistency.getAbsolutePath(nextArg);
				}

			} else if (arg.equals(OPT_RECURSIVE)) {
				recursive = true;

			} else if (arg.equals(OPT_TARGET_DIR)) {
				targetDir = persistency.getAbsolutePath(nextArg);

			} else if (arg.equals(OPT_LINE_RANGE)) {
				String lineRange = nextArg;
				int sepPos = lineRange.indexOf(LINE_RANGE_SEP);
				int startLine = (sepPos <= 0) ? -1 : Integer.valueOf(lineRange.substring(0, sepPos));
				int endLine = (sepPos < 0 || sepPos + 1 >= lineRange.length()) ? -1 : Integer.valueOf(lineRange.substring(sepPos + 1));
				if (startLine < 0 || endLine < 0 || startLine > endLine) {
					errors.append("Invalid " + OPT_LINE_RANGE + ": Expected format \"m-n\", e.g. " + LINE_RANGE_EXAMPLE).append(LINE_SEP);
				} else {
					cleanupRange = CleanupRange.create(startLine, endLine, true);
				}

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

			} else if (arg.equals(OPT_TARGET_FILE)) {
				targetPath = nextArg;

			} else if (arg.equals(OPT_OVERWRITE)) {
				overwrite = true;

			} else if (arg.equals(OPT_PARTIAL_RESULT)) {
				partialResult = true;

			} else if (arg.equals(OPT_STATS)) {
				showStats = true;

			} else if (arg.equals(OPT_USED_RULES)) {
				showUsedRules = true;

			} else if (arg.equals(OPT_HELP_WINDOWS) || arg.equals(OPT_HELP_LINUX)) {
				showHelp = true;

			} else if (arg.equals(OPT_FILE_FILTER)) {
				fileFilter = nextArg;

			} else {
				errors.append("Unknown option: " + arg).append(LINE_SEP);
			}
			
			// skip next argument, since it was already consumed above
			if (nextArg != null)
				++i;
		}
		
		if (sourceDir != null && sourceCode != null) {
			errors.append("Source was supplied multiple times; please use " + OPT_SOURCE_FILE + " or " + OPT_SOURCE_CODE + " or " + OPT_SOURCE_DIR + " only once.").append(LINE_SEP);
		}
		
		if (!StringUtil.isNullOrEmpty(sourceDir)) {
			if (StringUtil.isNullOrEmpty(targetDir)) {
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
		
		if (!overwrite) {
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
			return new CommandLineArgs(sourceCode, targetPath, cleanupRange, profileData, abapRelease, overwrite, partialResult, showStats, showUsedRules, errors.toString(), showHelp);
		} else {
			return new CommandLineArgs(sourceDir, sourcePaths, targetDir, profileData, abapRelease, overwrite, showStats, showUsedRules, errors.toString(), showHelp);
		}
	}

	public static String getHelp(Persistency persistency) {
		String profileExtension = persistency.getExtension(FileType.PROFILE_TEXT);

		StringBuilder sb = new StringBuilder();

		String usagePrefix = "    " + EXECUTABLE_NAME; 
		String spacePrefix = StringUtil.repeatChar(' ', usagePrefix.length());

		sb.append("Usage for single file:");
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
		sb.append(" [" + OPT_TARGET_FILE + " targetfile");
		sb.append(" [" + OPT_OVERWRITE + "]]");
		sb.append(" [" + OPT_PARTIAL_RESULT + "]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [" + OPT_STATS + "]");
		sb.append(" [" + OPT_USED_RULES + "]");
		sb.append(LINE_SEP + LINE_SEP);

		sb.append("Example for single file:");
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

		sb.append("Usage for multiple files:");
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

		sb.append("Example for multiple files:");
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

		sb.append("Options: ");
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

	public final String sourceCode;
	public final CleanupRange cleanupRange;
	public final String targetPath; 

	public final String[] sourcePaths;
	public final String sourceDir;
	public final String targetDir;

	public final String profileData;
	public final String abapRelease;
	public final boolean overwrite;
	public final boolean partialResult;
	public final boolean showStats;
	public final boolean showUsedRules;
	public final String errors;
	public final boolean showHelp;
	
	public boolean writesResultCodeToOutput() { return isInSingleSourceMode() && StringUtil.isNullOrEmpty(targetPath); }
	
	public boolean isInSingleSourceMode() { return sourceDir == null; }

	public boolean hasErrors() { return !StringUtil.isNullOrEmpty(errors); }
	
	private CommandLineArgs(String sourceCode, String targetPath, CleanupRange cleanupRange, String profileData, String abapRelease, boolean overwrite, boolean partialResult, boolean showStats, boolean showUsedRules, String errors, boolean showHelp) {
		this.sourceCode = sourceCode;
		this.targetPath = targetPath;
		this.cleanupRange = cleanupRange;

		this.sourcePaths = null;
		this.sourceDir = null;
		this.targetDir = null;

		this.profileData = profileData;
		this.abapRelease = abapRelease;
		this.overwrite = overwrite;
		this.partialResult = partialResult;
		this.showStats = showStats;
		this.showUsedRules = showUsedRules;
		this.errors = errors;
		this.showHelp = showHelp;

	}

	private CommandLineArgs(String sourceDir, String[] sourcePaths, String targetDir, String profileData, String abapRelease, boolean overwrite, boolean showStats, boolean showUsedRules, String errors, boolean showHelp) {
		this.sourceCode = null;
		this.cleanupRange = null;
		this.targetPath = null;

		this.sourceDir = sourceDir;
		this.sourcePaths = sourcePaths;
		this.targetDir = targetDir;

		this.profileData = profileData;
		this.abapRelease = abapRelease;
		this.overwrite = overwrite;
		this.partialResult = false;
		this.showStats = showStats;
		this.showUsedRules = showUsedRules;
		this.errors = errors;
		this.showHelp = showHelp;
	}
}
