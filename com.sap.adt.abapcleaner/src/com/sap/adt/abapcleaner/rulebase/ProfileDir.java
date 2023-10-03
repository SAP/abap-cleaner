package com.sap.adt.abapcleaner.rulebase;

public class ProfileDir {
	public final String shortName;
	public final String readOnlyDir;

	/**
	 * returns a default short name for the specified, zero-based index
	 * @param index - the zero-based index of the read-only profile directory
	 * @return - a default short name, e.g. "team A", "team B" etc.
	 */
	public static String getDefaultShortName(int index) {
		return  "team " + String.valueOf((char)(65 + index)); // "team A", "team B", "team C" etc.
	}
	
	public static String standardizeShortName(String shortName) {
		return shortName.trim().replace(Profile.READ_ONLY_INFIX.trim(), ".");
	}
	
	public ProfileDir(String shortName, String readOnlyDir) {
		this.shortName = shortName;
		this.readOnlyDir = readOnlyDir;
	}
}
