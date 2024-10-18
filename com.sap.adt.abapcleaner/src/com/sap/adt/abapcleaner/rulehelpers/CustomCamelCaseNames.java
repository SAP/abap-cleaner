package com.sap.adt.abapcleaner.rulehelpers;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.HashMap;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.Persistency;

public class CustomCamelCaseNames {
	public static String getKey(String value) {
		return value.toUpperCase();
	}
	
	public final CamelCaseNameType nameType;
	public final HashMap<String, String> camelCaseNames = new HashMap<>();
	private String loadPath = null;
	private long loadPathLastModified = 0;
	
	public CustomCamelCaseNames(CamelCaseNameType type) {
		this.nameType = type;
	}
	
	public void clear() {
		clear(null, 0);
	}

	private void clear(String loadPath, long lastModified) {
		camelCaseNames.clear();
		this.loadPath = loadPath;
		this.loadPathLastModified = lastModified;
	}

	
	public void load(String profileDir, String folderFile) throws IOException {
		final boolean checkStartsWithZ = false;
		final boolean checkUnknownPrefixesOrSuffixes = false;
		
		// expected file format: 
		// - one name per line, using either \r\n or \n as a line separator;
		// - leading and trailing spaces in a line are ignored
		// - any non-ABAP-name character at line start is considered to start a comment
		// - any non-ABAP-name character after a name is considered to start a line-end comment

		// determine the path
		if (StringUtil.isNullOrEmpty(profileDir) || StringUtil.isNullOrEmpty(folderFile)) {
			clear();
			return;
		}
		Persistency persistency = Persistency.get();
		String path = persistency.combinePaths(profileDir, folderFile);
		if (!persistency.fileExists(path)) {
			clear();
			return;
		}
		
		// only reload the file if it was modified since the last load() call
		long lastModified = persistency.getLastModified(path);
		if (loadPath != null && loadPath.equals(path) && loadPathLastModified == lastModified)
			return;
		clear(path, lastModified);
		
		// read the text file line by line from a BufferedReader
		BufferedReader bufferedReader = persistency.getBufferedReader(path, false);
		do {
			String line = bufferedReader.readLine();
			if (line == null)
				break;
			
			// determine the CamelCase name from the current line
			line = line.trim();
			String name = ABAP.readTillEndOfVariableName(line, 0, false, true);
			if (StringUtil.isNullOrEmpty(name))
				continue;
			
			// add the CamelCase name if it is valid, allowing for both Z... names and unknown prefixes and suffixes
			String discardReason = CamelCaseNames.checkDiscardReasons(name, nameType, checkStartsWithZ, checkUnknownPrefixesOrSuffixes, true);
			if (discardReason == null) {
				camelCaseNames.put(getKey(name), name);
			}
		} while(true);
		bufferedReader.close();
	}

	public String applyCamelCaseTo(String text) {
		return camelCaseNames.get(getKey(text));
	}
}
