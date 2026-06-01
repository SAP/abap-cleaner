package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.CleanupRange;
import com.sap.adt.abapcleaner.parser.CleanupRangeExpandMode;

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
	private static final String OPT_EXPAND_MODE = "--scope";
	private static final String EXPAND_MODE_STATEMENT = "statement"; // default for CLI
	private static final String EXPAND_MODE_METHOD = "method"; // default for UI
	private static final String EXPAND_MODE_CLASS = "class";
	private static final String EXPAND_MODE_DOCUMENT = "document";
	private static final String EXPAND_MODE_USER = "user"; // use user setting from the UI

	// - input (multiple files)
	private static final String OPT_SOURCE_DIR = "--sourcedir";
	private static final String OPT_FILE_FILTER = "--filepattern";
	private static final String OPT_RECURSIVE = "--recursive";
	
	// - cleanup
	private static final String OPT_PROFILE = "--profile";
	private static final String OPT_PROFILE_DATA = "--profiledata";
	private static final String OPT_PROFILE_NAME = "--profilename";
	private static final String OPT_LAST_PROFILE = "--last-profile";
	private static final String OPT_RELEASE = "--release";
	private static final String OPT_WORKSPACE = "--workspace";

	// - interactive cleanup (single source only, no profile or cleanup range allowed)
	private static final String OPT_INTERACTIVE = "--ui";
	private static final String OPT_TITLE = "--title";
	private static final String OPT_READ_ONLY = "--readonly";
	private static final String OPT_DARK_THEME = "--darktheme";
	
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
			OPT_SOURCE_FILE, OPT_SOURCE_CODE, OPT_LINE_RANGE, OPT_EXPAND_MODE, OPT_SOURCE_DIR, OPT_FILE_FILTER, OPT_RECURSIVE, 
			OPT_PROFILE, OPT_PROFILE_DATA, OPT_PROFILE_NAME, OPT_LAST_PROFILE, OPT_RELEASE, OPT_WORKSPACE, 
			OPT_INTERACTIVE, OPT_TITLE, OPT_READ_ONLY, OPT_DARK_THEME,
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
			OPT_SOURCE_FILE, OPT_SOURCE_CODE, OPT_LINE_RANGE, OPT_EXPAND_MODE, OPT_SOURCE_DIR, OPT_FILE_FILTER,
			OPT_PROFILE, OPT_PROFILE_DATA, OPT_PROFILE_NAME, OPT_RELEASE, OPT_WORKSPACE,
			OPT_TITLE,
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
		CleanupRangeExpandMode expandMode = CleanupRangeExpandMode.FULL_STATEMENT;
		boolean foundExpandModeOption = false;
		boolean foundSourceCodeOption = false; // true if OPT_SOURCE_CODE was found (otherwise, OPT_SOURCE_FILE)
		String expandModeArg = null; 
		// - input options (multiple files)
		String sourceDir = null;
		String[] sourcePaths = null;
		String fileFilter = null;
		boolean recursive = false;
		
		// - cleanup options
		String profileData = null;
		String profileName = null;
		boolean lastProfile = false;
		String abapRelease = null;
		boolean foundProfileOption = false; // true if OPT_PROFILE, OPT_PROFILE_DATA, OPT_PROFILE_NAME or OPT_LAST_PROFILE was found
		String usedProfileArg = null;
		String workspaceDir = null;

		// - interactive cleanup options
		boolean interactive = false;
		String title = ""; // will be set to sourceName for interactive cleanup if not supplied via --title
		boolean readOnly = false;
		boolean darkTheme = false;

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
			String nextArgNonNull = (nextArg == null) ? "" : nextArg;

			// -------------------------------------
			// - input options (single file)

			if (arg.equals(OPT_SOURCE_FILE) || arg.equals(OPT_SOURCE_CODE)) {
				if (sourceCode != null) {
					errors.append("Source code supplied twice; please use " + OPT_SOURCE_FILE + " or " + OPT_SOURCE_CODE + " only once.").append(LINE_SEP);
				} else if (arg.equals(OPT_SOURCE_CODE)) {
					foundSourceCodeOption = true;
					sourceCode = nextArg;
				} else if (persistency.fileExists(nextArg)) {
					sourceName = persistency.getFileNameWithoutExtension(nextArg);
					sourceCode = persistency.readAllTextFromFile(nextArg);
				} else {
					errors.append("File not found: " + nextArg).append(LINE_SEP);
				}

			} else if (arg.equals(OPT_LINE_RANGE)) {
				String lineRange = nextArgNonNull;
				int sepPos = lineRange.indexOf(LINE_RANGE_SEP);
				int startLine = (sepPos <= 0) ? -1 : Integer.valueOf(lineRange.substring(0, sepPos));
				int lastLine = (sepPos < 0 || sepPos + 1 >= lineRange.length()) ? -1 : Integer.valueOf(lineRange.substring(sepPos + 1));
				if (startLine <= 0 || lastLine <= 0 || startLine > lastLine) {
					errors.append("Invalid " + OPT_LINE_RANGE + ": Expected format \"m-n\" (1-based), e.g. " + LINE_RANGE_EXAMPLE).append(LINE_SEP);
				} else {
					// the --scope option is usually not yet known at this point, but we can simply use expandRange = true:
					// if no --scope is supplied, the range will only be expanded to statement scope, which matches the behavior 
					// of expandRange = false
					cleanupRange = CleanupRange.create(startLine, lastLine, true);
				}

			} else if (arg.equals(OPT_EXPAND_MODE)) {
				expandModeArg = nextArgNonNull;
				foundExpandModeOption = true;

				if (EXPAND_MODE_STATEMENT.equals(expandModeArg)) {
					expandMode = CleanupRangeExpandMode.FULL_STATEMENT;
					
				} else if (EXPAND_MODE_METHOD.equals(expandModeArg)) {
					expandMode = CleanupRangeExpandMode.FULL_METHOD;
					
				} else if (EXPAND_MODE_CLASS.equals(expandModeArg)) {
					expandMode = CleanupRangeExpandMode.FULL_CLASS;
					
				} else if (EXPAND_MODE_DOCUMENT.equals(expandModeArg)) {
					expandMode = CleanupRangeExpandMode.FULL_DOCUMENT;
					
				} else if (EXPAND_MODE_USER.equals(expandModeArg)) {
					expandMode = null; // use user setting from the UI
					
				} else {
					errors.append("Invalid " + OPT_EXPAND_MODE + ": Expected one of the following scopes: " 
							+ EXPAND_MODE_STATEMENT + ", " + EXPAND_MODE_METHOD + ", " + EXPAND_MODE_CLASS + ", " 
							+ EXPAND_MODE_DOCUMENT + " or " + EXPAND_MODE_USER + "").append(LINE_SEP);
				}

				// -------------------------------------
				// - input options (multiple files)

			} else if (arg.equals(OPT_SOURCE_DIR)) {
				if (!persistency.directoryExists(nextArg)) {
					errors.append("Source directory " + nextArg + " does not exist!").append(LINE_SEP);
				} else {
					sourceDir = persistency.getAbsolutePath(nextArg);
				}

			} else if (arg.equals(OPT_FILE_FILTER)) {
				if (nextArgNonNull.indexOf("*") < 0) {
					errors.append("File pattern must contain an asterisk, e.g. " + OPT_FILE_FILTER + " \"*.abap\"").append(LINE_SEP);
				} else {
					fileFilter = nextArg;
				}

			} else if (arg.equals(OPT_RECURSIVE)) {
				recursive = true;

				// -------------------------------------
				// - cleanup options

			} else if (arg.equals(OPT_PROFILE) || arg.equals(OPT_PROFILE_DATA) || arg.equals(OPT_PROFILE_NAME) || arg.equals(OPT_LAST_PROFILE)) {
				if (foundProfileOption) {
					errors.append("Multiple profile arguments supplied; please use " + OPT_PROFILE + ", " + OPT_PROFILE_DATA + ", " 
							+ OPT_PROFILE_NAME + " or " + OPT_LAST_PROFILE + " only once.").append(LINE_SEP);
				} else if (arg.equals(OPT_PROFILE)) {
					if (persistency.fileExists(nextArg)) {
						profileData = persistency.readAllTextFromFile(nextArg);
					} else {
						errors.append("File not found: " + nextArg).append(LINE_SEP);
					}
				} else if (arg.equals(OPT_PROFILE_DATA)) {
					profileData = nextArg;
				} else if (arg.equals(OPT_PROFILE_NAME)) {
					// at this point, it is not possible to validate whether the supplied profile name actually exists; 
					// if it doesn't, we will use program defaults as the fallback
					profileName = nextArg;
				} else if (arg.equals(OPT_LAST_PROFILE)) {
					lastProfile = true;
				}
				foundProfileOption = true;
				usedProfileArg = arg;

			} else if (arg.equals(OPT_RELEASE)) {
				abapRelease = nextArg;

			} else if (arg.equals(OPT_WORKSPACE)) {
				workspaceDir = nextArgNonNull;

				// -------------------------------------
				// - interactive cleanup options
				
			} else if (arg.equals(OPT_INTERACTIVE)) {
				interactive = true;

			} else if (arg.equals(OPT_TITLE)) {
				title = nextArgNonNull;

			} else if (arg.equals(OPT_READ_ONLY)) {
				readOnly = true;

			} else if (arg.equals(OPT_DARK_THEME)) {
				darkTheme = true;

				// -------------------------------------
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

				// -------------------------------------
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

		// set CleanupRange.expandRange = false if --scope was explicitly set to "statement": this can esp. be used
		// for interactive cleanup if the user explicitly selected line range to be cleaned
		// (cp. AbapCleanerHandlerBase.execute(), where expandRange = true is only used for selection length 0)
		if (cleanupRange != null && foundExpandModeOption && expandMode == CleanupRangeExpandMode.FULL_STATEMENT) {
			cleanupRange = CleanupRange.create(cleanupRange.startLine, cleanupRange.lastLine, false);
		}
		
		// check input options
		if (sourceDir != null && sourceCode != null) {
			errors.append("Source was supplied multiple times; please use " + OPT_SOURCE_FILE + " or " + OPT_SOURCE_CODE + " or " + OPT_SOURCE_DIR + " only once.").append(LINE_SEP);
		}
		if (foundExpandModeOption && cleanupRange == null) {
			errors.append("Missing option: " + OPT_EXPAND_MODE + " requires " + OPT_LINE_RANGE).append(LINE_SEP);
		}

		// check whether input options for single file match other options
		if (sourceCode != null) {
			String sourceOption = foundSourceCodeOption ? OPT_SOURCE_CODE : OPT_SOURCE_FILE;
			if (fileFilter != null) {
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_FILE_FILTER, sourceOption)).append(LINE_SEP);
			}
			if (recursive) {
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_RECURSIVE, sourceOption)).append(LINE_SEP);
			}
			if (targetDir != null) {
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_TARGET_DIR, sourceOption)).append(LINE_SEP);
			}
		}

		// check whether input options for multiple files match cleanup and output options
		if (!StringUtil.isNullOrEmpty(sourceDir)) {
			if (!simulate && StringUtil.isNullOrEmpty(targetDir)) {
				targetDir = sourceDir;
			}
			if (cleanupRange != null)
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_LINE_RANGE, OPT_SOURCE_DIR)).append(LINE_SEP);
			if (interactive)
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_INTERACTIVE, OPT_SOURCE_DIR)).append(LINE_SEP);
			if (targetPath != null)
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_TARGET_FILE, OPT_SOURCE_DIR)).append(LINE_SEP);
			if (partialResult) 
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_PARTIAL_RESULT, OPT_SOURCE_DIR)).append(LINE_SEP);
				
			sourcePaths = persistency.getFilesInDirectory(sourceDir, StringUtil.isNullOrEmpty(fileFilter) ? DEFAULT_ABAP_FILE_PATTERN : fileFilter, recursive);
			if (sourcePaths == null || sourcePaths.length == 0) {
				errors.append("No matching files found in given source directory: " + sourceDir).append(LINE_SEP);
			}
		}

		// check interactive cleanup options
		if (interactive) {
			// errors for the combination of interactive UI with sourceDir (cleanup of multiple files) were already created above
			
			// for interactive cleanup, the expand mode setting on the UI will be used, therefore any 
			// --scope other than "--scope user" (which results in expandMode == null) and "--scope statement" is an error
			if (cleanupRange != null && foundExpandModeOption && expandMode != null && expandMode != CleanupRangeExpandMode.FULL_STATEMENT) {
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_EXPAND_MODE + " " + expandModeArg, OPT_INTERACTIVE)).append(LINE_SEP);
			}
			// for interactive cleanup, the cleanup profile must not be supplied, as the UI has its own (workspace-specific) settings for it
			if (foundProfileOption) {
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, usedProfileArg, OPT_INTERACTIVE)).append(LINE_SEP);
			}
			if (readOnly) {
				// read-only preview on the UI cannot be selected together with options that specify a target for the cleanup result (even with simulation)
				if (targetPath != null) { 
					errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_TARGET_FILE, OPT_READ_ONLY)).append(LINE_SEP);
				}
				if (targetDir != null) { 
					errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_TARGET_DIR, OPT_READ_ONLY)).append(LINE_SEP);
				}
			}
			// statistics are not available in interactive cleanup, since FrmMain.refreshCode() does not store them (esp. after partial reprocessing)
			if (showStats) {
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_STATS, OPT_INTERACTIVE)).append(LINE_SEP);
			}
			if (showUsedRules) {
				errors.append(String.format(INVALID_OPTION_COMBO_FORMAT, OPT_USED_RULES, OPT_INTERACTIVE)).append(LINE_SEP);
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
		
		} else if (!overwrite) {
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
			// single file (including for interactive cleanup; restrictions for combinations of parameters were checked above)
			return new CommandLineArgs(errors.toString(), sourceName, sourceCode, cleanupRange, expandMode, 
												profileData, profileName, lastProfile, abapRelease, workspaceDir, 
												interactive, title, readOnly, darkTheme,
												simulate, targetPath, partialResult, overwrite, lineSeparator, showStats, showUsedRules);
		} else {
			// multiple files
			return new CommandLineArgs(errors.toString(), sourceDir, sourcePaths, 
												profileData, profileName, lastProfile, abapRelease, workspaceDir, 
												simulate, targetDir, overwrite, lineSeparator, showStats, showUsedRules);
		}
	}

	public static String getHelp(Persistency persistency) {
		String profileExtension = persistency.getExtension(FileType.PROFILE_TEXT);

		StringBuilder sb = new StringBuilder();

		String usagePrefix = "    " + EXECUTABLE_NAME;
		String spacePrefix = StringUtil.repeatChar(' ', usagePrefix.length());

		sb.append("Show help or version information:");
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_HELP_WINDOWS);
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_HELP_LINUX);
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_VERSION);
		sb.append(LINE_SEP + LINE_SEP + LINE_SEP);

		sb.append("Cleanup of single source:");
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" {" + OPT_SOURCE_FILE + " <path>");
		sb.append(" | " + OPT_SOURCE_CODE + " <code>}");
		sb.append(" [" + OPT_LINE_RANGE + " <numrange> [" + OPT_EXPAND_MODE + " <scopename>] ]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [{ " + OPT_PROFILE + " <path>");
		sb.append(" | " + OPT_PROFILE_DATA + " <json>");
		sb.append(" | " + OPT_PROFILE_NAME + " <name>");
		sb.append(" | " + OPT_LAST_PROFILE + " }]");
		sb.append(" [" + OPT_RELEASE + " <num>]");
		sb.append(" [" + OPT_WORKSPACE + " <dir>]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [" + OPT_TARGET_FILE + " <path>");
		sb.append(" [" + OPT_OVERWRITE + "]]");
		sb.append(" [" + OPT_PARTIAL_RESULT + "]");
		sb.append(" [" + OPT_CRLF + "]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [" + OPT_STATS + "]");
		sb.append(" [" + OPT_USED_RULES + "]");
		sb.append(LINE_SEP + LINE_SEP);

		sb.append("- Example for cleanup of single source:");
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_SOURCE_FILE + " \"CL_ANY_CLASS.txt\"");
		sb.append(" " + OPT_LINE_RANGE + " " + LINE_RANGE_EXAMPLE + " " + OPT_EXPAND_MODE + " " + EXPAND_MODE_METHOD);
		sb.append(" " + OPT_PROFILE_NAME + " \"team A: profile\"");
		sb.append(" " + OPT_RELEASE + " \"757\"");
		sb.append(" " + OPT_TARGET_FILE + " \"result\\CL_ANY_CLASS.txt\"");
		sb.append(" " + OPT_OVERWRITE);
		sb.append(" " + OPT_STATS);
		sb.append(" " + OPT_USED_RULES);
		sb.append(LINE_SEP + LINE_SEP + LINE_SEP);

		sb.append("Interactive cleanup (single source only; profile and user scope only selected on the UI):");
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" {" + OPT_SOURCE_FILE + " <path>");
		sb.append(" | " + OPT_SOURCE_CODE + " <code>}");
		sb.append(" [" + OPT_LINE_RANGE + " <numrange>");
		sb.append(" [" + OPT_EXPAND_MODE + " {" + EXPAND_MODE_STATEMENT + " | " + EXPAND_MODE_USER + "}] ]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [" + OPT_RELEASE + " <num>]");
		sb.append(" [" + OPT_WORKSPACE + " <dir>]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" " + OPT_INTERACTIVE);
		sb.append(" [" + OPT_TITLE + " <text>]");
		sb.append(" [" + OPT_READ_ONLY + "]");
		sb.append(" [" + OPT_DARK_THEME + "]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [" + OPT_TARGET_FILE + " <path>");
		sb.append(" [" + OPT_OVERWRITE + "]]");
		sb.append(" [" + OPT_PARTIAL_RESULT + "]");
		sb.append(" [" + OPT_CRLF + "]");
		sb.append(LINE_SEP + LINE_SEP);

		sb.append("- Example for interactive cleanup of single file:");
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_SOURCE_FILE + " \"CL_ANY_CLASS.txt\"");
		sb.append(" " + OPT_RELEASE + " \"757\"");
		sb.append(" " + OPT_INTERACTIVE);
		sb.append(" " + OPT_TITLE + " \"CL_ANY_CLASS\"");
		sb.append(" " + OPT_WORKSPACE + " \"test_ws\"");
		sb.append(" " + OPT_READ_ONLY);
		sb.append(LINE_SEP + LINE_SEP + LINE_SEP);

		sb.append("Cleanup of multiple files:");
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_SOURCE_DIR + " <path>");
		sb.append(" [" + OPT_FILE_FILTER + " <pattern>]");
		sb.append(" [" + OPT_RECURSIVE + "]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [{ " + OPT_PROFILE + " <path>");
		sb.append(" | " + OPT_PROFILE_DATA + " <json>");
		sb.append(" | " + OPT_PROFILE_NAME + " <name>");
		sb.append(" | " + OPT_LAST_PROFILE + " }]");
		sb.append(" [" + OPT_RELEASE + " <num>]");
		sb.append(" [" + OPT_WORKSPACE + " <dir>]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [" + OPT_TARGET_DIR + " <path>");
		sb.append(" [" + OPT_OVERWRITE + "]]");
		sb.append(" [" + OPT_CRLF + "]");
		sb.append(LINE_SEP);
		sb.append(spacePrefix);
		sb.append(" [" + OPT_STATS + "]");
		sb.append(" [" + OPT_USED_RULES + "]");
		sb.append(LINE_SEP + LINE_SEP);

		sb.append("- Example for cleanup of multiple files:");
		sb.append(LINE_SEP);
		sb.append(usagePrefix);
		sb.append(" " + OPT_SOURCE_DIR + " \"C:\\temp\\source\"");
		sb.append(" " + OPT_FILE_FILTER + " \"*.txt\"");
		sb.append(" " + OPT_RECURSIVE);
		sb.append(" " + OPT_PROFILE + " \"" + "C:\\temp\\profile" + profileExtension + "\"");
		sb.append(" " + OPT_RELEASE + " \"757\"");
		sb.append(" " + OPT_TARGET_DIR + " \"C:\\temp\\target\"");
		sb.append(" " + OPT_OVERWRITE);
		sb.append(LINE_SEP + LINE_SEP + LINE_SEP);

		sb.append("Options for cleanup:");
		sb.append(LINE_SEP);
		sb.append(getOptionHelp(OPT_SOURCE_FILE, "File name of an ABAP source file which is input to the cleanup."));
		sb.append(getOptionHelp(OPT_SOURCE_CODE, "ABAP source code which is input to the cleanup."));
		sb.append(getOptionHelp(null, "Please use either " + OPT_SOURCE_FILE + " or " + OPT_SOURCE_CODE + " or " + OPT_SOURCE_DIR + "."));
		sb.append(getOptionHelp(OPT_LINE_RANGE, "Single line range for partial cleanup, e.g. " + LINE_RANGE_EXAMPLE));
		sb.append(getOptionHelp(null, "Without this option, the cleanup will be applied to the whole code document."));
		sb.append(getOptionHelp(OPT_EXPAND_MODE, "Expands the line range to the supplied scope: " + EXPAND_MODE_STATEMENT + " (default), " + EXPAND_MODE_METHOD + ", "));
		sb.append(getOptionHelp(null, EXPAND_MODE_CLASS + ", " + EXPAND_MODE_DOCUMENT + " or " + EXPAND_MODE_USER + " (setting from the UI)."));
		sb.append(getOptionHelp(null, "Without this option, the line range will only be expanded to statement scope."));
		sb.append(LINE_SEP);
		sb.append(getOptionHelp(OPT_SOURCE_DIR, "Folder that contains ABAP source files (default file pattern is \"*.abap\")"));
		sb.append(getOptionHelp(OPT_FILE_FILTER, "File pattern to look for (only relevant when " + OPT_SOURCE_DIR + " has been supplied)"));
		sb.append(getOptionHelp(OPT_RECURSIVE, "Searches provided source directory recursively for ABAP files"));
		sb.append(LINE_SEP);
		sb.append(getOptionHelp(OPT_PROFILE, "File name of the cleanup profile to be used (extension " + profileExtension + "). From the UI,"));
		sb.append(getOptionHelp(null, "you may use button 'Export...' from the profiles editor to create the file."));
		sb.append(getOptionHelp(OPT_PROFILE_DATA, "JSON-like content of the cleanup profile to be used."));
		sb.append(getOptionHelp(OPT_PROFILE_NAME, "Name of the cleanup profile to be used, as displayed on the UI."));
		sb.append(getOptionHelp(null, "This includes synchronized team profiles such as \"team A: profile\"."));
		sb.append(getOptionHelp(OPT_LAST_PROFILE, "Use the cleanup profile that was last selected on the UI on this machine."));
		sb.append(getOptionHelp(null, "Please provide only one profile option (or none for program defaults)."));
		sb.append(LINE_SEP);
		sb.append(getOptionHelp(OPT_RELEASE, "ABAP release to restrict syntax of cleanup changes, e.g. \"758\"."));
		sb.append(getOptionHelp(null, "Without this option, the latest ABAP syntax will be allowed."));
		sb.append(getOptionHelp(OPT_WORKSPACE, "The workspace directory or ID, used to retrieve workspace-specific settings"));
		sb.append(getOptionHelp(null, "for the last cleanup profile to be used (" + OPT_LAST_PROFILE + "), for expanding the"));
		sb.append(getOptionHelp(null, "line range (" + OPT_EXPAND_MODE + " " + EXPAND_MODE_USER + ") and additional release restrictions (from the UI)."));
		sb.append(LINE_SEP);
		sb.append(getOptionHelp(OPT_INTERACTIVE, "Open the UI for interactive cleanup and profile configuration"));
		sb.append(getOptionHelp(null, "(only possible for single source input)."));
		sb.append(getOptionHelp(OPT_TITLE, "The source code title to be displayed on the UI."));
		sb.append(getOptionHelp(OPT_READ_ONLY, "Show a read-only preview of the changes without allowing to apply them."));
		sb.append(getOptionHelp(OPT_DARK_THEME, "Show the code with dark theme colors."));
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
	public final CleanupRangeExpandMode cleanupRangeExpandMode;
	// - input (multiple files)
	public final String sourceDir;
	public final String[] sourcePaths;

	// - cleanup
	public final String profileData;
	public final String profileName;
	public final boolean lastProfile;
	public final String abapRelease;
	/** workspace directory, used as a key for making main window settings (profile, cleanup range, release restriction) 
	 * workspace-specific, cp. MainSettings.CleanupSettings */
	public final String workspaceDir;

	// - interactive cleanup (single source only)
	/** whether to open the interactive UI with the supplied source code */
	public final boolean interactive;
	/** source code title to be displayed on the UI */
	public final String title;
	/** true for read-only preview of changes */
	public final boolean readOnly;
	/** true to show the UI with dark theme colors */
	public final boolean darkTheme;
	
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

	// profile options were checked to be mutually exclusive
	public boolean hasAnyProfileOption() { return hasProfileData() || hasProfileName() || useLastProfile(); }
	public boolean hasProfileData() { return profileData != null; }
	public boolean hasProfileName() { return profileName != null; }
	public boolean useLastProfile() { return lastProfile; }
	
	/** constructor for non-cleanup actions (SHOW_HELP, SHOW_VERSION) */
	private CommandLineArgs(CommandLineAction action) {
		this.action = action;
		this.errors = null;
		
		this.sourceName = null;
		this.sourceCode = null;
		this.cleanupRange = null;
		this.cleanupRangeExpandMode = null;
		this.sourceDir = null;
		this.sourcePaths = null;

		this.profileData = null;
		this.profileName = null;
		this.lastProfile = false;
		this.abapRelease = null;
		this.workspaceDir = null;

		this.interactive = false;
		this.title = null;
		this.readOnly = false;
		this.darkTheme = false;
		
		this.simulate = false;
		this.targetPath = null;
		this.partialResult = false;
		this.targetDir = null;
		this.overwrite = false;
		this.lineSeparator = null;

		this.showStats = false;
		this.showUsedRules = false;
	}
	
	/** constructor for cleanup of a single file (or a line range within it), possibly opening the UI for interactive cleanup */
	private CommandLineArgs(
			String errors, 
			String sourceName, String sourceCode, CleanupRange cleanupRange, CleanupRangeExpandMode cleanupRangeExpandMode, 
			String profileData, String profileName, boolean lastProfile, String abapRelease, String workspaceDir, 
			boolean interactive, String title, boolean readOnly, boolean darkTheme,
			boolean simulate, String targetPath, boolean partialResult, boolean overwrite, String lineSeparator, 
			boolean showStats, boolean showUsedRules) {

		this.action = CommandLineAction.CLEANUP;
		this.errors = errors;
		
		this.sourceName = sourceName; 
		this.sourceCode = sourceCode;
		this.cleanupRange = cleanupRange;
		this.cleanupRangeExpandMode = cleanupRangeExpandMode;
		
		this.sourceDir = null;
		this.sourcePaths = null;

		this.profileData = profileData;
		this.profileName = profileName;
		this.lastProfile = lastProfile;
		this.abapRelease = abapRelease;
		this.workspaceDir = workspaceDir;

		this.interactive = interactive;
		this.title = title;
		this.readOnly = readOnly;
		this.darkTheme = darkTheme;
		
		this.simulate = simulate;
		this.targetPath = targetPath;
		this.partialResult = partialResult;
		this.targetDir = null;
		this.overwrite = overwrite;
		this.lineSeparator = lineSeparator;

		this.showStats = showStats;
		this.showUsedRules = showUsedRules;
	}

	/** constructor for cleanup of a multiple files (always entirely and without UI) */
	private CommandLineArgs(
			String errors,
			String sourceDir, String[] sourcePaths,
			String profileData, String profileName, boolean lastProfile, String abapRelease, String workspaceDir,
			boolean simulate, String targetDir, boolean overwrite, String lineSeparator, 
			boolean showStats, boolean showUsedRules) {

		this.action = CommandLineAction.CLEANUP;
		this.errors = errors;

		this.sourceName = null;
		this.sourceCode = null;
		this.cleanupRange = null;
		this.cleanupRangeExpandMode = null;
		
		this.sourceDir = sourceDir;
		this.sourcePaths = sourcePaths;

		this.profileData = profileData;
		this.profileName = profileName;
		this.lastProfile = lastProfile;
		this.abapRelease = abapRelease;
		this.workspaceDir = null;

		this.interactive = false;
		this.title = null;
		this.readOnly = false;
		this.darkTheme = false;
		
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
