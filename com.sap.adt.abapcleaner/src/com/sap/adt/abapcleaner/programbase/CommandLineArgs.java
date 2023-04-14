package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.CleanupRange;

public class CommandLineArgs {
	private static final String LINE_SEP = System.lineSeparator();

	private static final String OPT_SOURCE_FILE = "--sourcefile";
	private static final String OPT_SOURCE_CODE = "--source";
	private static final String OPT_LINE_RANGE = "--linerange";
	private static final String OPT_PROFILE = "--profile";
	private static final String OPT_PROFILE_DATA = "--profiledata";
	private static final String OPT_RELEASE = "--release";
	private static final String OPT_TARGET_FILE = "--targetfile";
	private static final String OPT_OVERWRITE = "--overwrite";
	private static final String OPT_PARTIAL_RESULT = "--partialresult";
	private static final String OPT_STATS = "--stats";
	private static final String OPT_USED_RULES = "--usedrules";

	private static final String[] allOptions = new String[] { OPT_SOURCE_FILE, OPT_SOURCE_CODE, OPT_LINE_RANGE, OPT_PROFILE, OPT_PROFILE_DATA, OPT_RELEASE, OPT_TARGET_FILE, OPT_OVERWRITE, OPT_PARTIAL_RESULT, OPT_STATS, OPT_USED_RULES };

	private static final String EXECUTABLE_NAME = ".\\abap-cleanerc.exe"; 
	private static final String OPT_HELP_WINDOWS = "/?";
	private static final String OPT_HELP_LINUX = "/man";
	private static final char LINE_RANGE_SEP = '-';
	private static final String LINE_RANGE_EXAMPLE = "\"20-35\"";

	private static final int OPTIONS_INDENT = 4;
	private static final int OPTIONS_LINE_PREFIX_LENGTH = 20; // must be at least the length of the longest OPT_ + 1

	private static final String[] optionsRequiringNextArg = new String[] { OPT_SOURCE_FILE, OPT_SOURCE_CODE, OPT_LINE_RANGE, OPT_RELEASE, OPT_PROFILE, OPT_PROFILE_DATA, OPT_TARGET_FILE };

	public static String[] getAllOptions() { return allOptions; }
	
	public static CommandLineArgs create(Persistency persistency, String[] args) {
		if (args == null || args.length == 0)
			return null;

		final String LINE_SEP = System.lineSeparator();
		
		String sourceCode = null;
		CleanupRange cleanupRange = null;
		String profileData = null;
		String abapRelease = null;
		String targetPath = null; 
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
				
			} else {
				errors.append("Unknown option: " + arg).append(LINE_SEP);
			}
			
			// skip next argument, since it was already consumed above
			if (nextArg != null)
				++i;
		}
		
		if (!overwrite && !StringUtil.isNullOrEmpty(targetPath) && persistency.fileExists(targetPath)) {
			errors.append("Target file already exists; please use " + OPT_OVERWRITE + " to allow overwriting: " + targetPath).append(LINE_SEP);
		}
		
		return new CommandLineArgs(sourceCode, cleanupRange, profileData, abapRelease, targetPath, overwrite, partialResult, showStats, showUsedRules, errors.toString(), showHelp);
	}

	public static String getHelp(Persistency persistency) {
		String profileExtension = persistency.getExtension(FileType.PROFILE_TEXT);

		StringBuilder sb = new StringBuilder();
		String usagePrefix = "Usage: " + EXECUTABLE_NAME; 
		String spacePrefix = StringUtil.repeatChar(' ', usagePrefix.length());
		sb.append(usagePrefix);
		sb.append(" { " + OPT_SOURCE_FILE + " sourcefile");
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

		sb.append("Example: " + EXECUTABLE_NAME);
		sb.append(" " + OPT_SOURCE_FILE + " \"CL_ANY_CLASS.txt\"");
		sb.append(" " + OPT_LINE_RANGE + " " + LINE_RANGE_EXAMPLE);
		sb.append(" " + OPT_PROFILE + " \"" + "team profile" + profileExtension + "\"");
		sb.append(" " + OPT_RELEASE + " \"757\"");
		sb.append(" " + OPT_TARGET_FILE + " \"result\\CL_ANY_CLASS.txt\"");
		sb.append(" " + OPT_OVERWRITE);
		sb.append(" " + OPT_STATS);
		sb.append(" " + OPT_USED_RULES);
		sb.append(LINE_SEP + LINE_SEP);

		sb.append("Options: ");
		sb.append(LINE_SEP);
		sb.append(getOptionHelp(OPT_SOURCE_FILE, "File name of an ABAP source file which is input to the cleanup."));
		sb.append(getOptionHelp(OPT_SOURCE_CODE, "ABAP source code which is input to the cleanup."));
		sb.append(getOptionHelp(null, "Please use either " + OPT_SOURCE_FILE + " or " + OPT_SOURCE_CODE + "."));
		sb.append(getOptionHelp(OPT_LINE_RANGE, "Single line range for partial cleanup, e.g. " + LINE_RANGE_EXAMPLE));
		sb.append(getOptionHelp(null, "Without this option, the cleanup will be applied to the whole code document."));
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
	public final String profileData;
	public final String abapRelease;
	public final String targetPath; 
	public final boolean overwrite;
	public final boolean partialResult;
	public final boolean showStats;
	public final boolean showUsedRules;
	public final String errors;
	public final boolean showHelp;
	
	public boolean writesResultCodeToOutput() { return StringUtil.isNullOrEmpty(targetPath); }

	public boolean hasErrors() { return !StringUtil.isNullOrEmpty(errors); }
	
	private CommandLineArgs(String sourceCode, CleanupRange cleanupRange, String profileData, String abapRelease, String targetPath, boolean overwrite, boolean partialResult, boolean showStats, boolean showUsedRules, String errors, boolean showHelp) {
		this.sourceCode = sourceCode;
		this.cleanupRange = cleanupRange;
		this.profileData = profileData;
		this.abapRelease = abapRelease;
		this.targetPath = targetPath;
		this.overwrite = overwrite;
		this.partialResult = partialResult;
		this.showStats = showStats;
		this.showUsedRules = showUsedRules;
		this.errors = errors;
		this.showHelp = showHelp;
	}
}
