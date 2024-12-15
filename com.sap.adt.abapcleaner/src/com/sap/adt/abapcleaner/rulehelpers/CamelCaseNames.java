package com.sap.adt.abapcleaner.rulehelpers;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.Cult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.Persistency;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.rulebase.Profile;

public class CamelCaseNames {
	public static final String FIELD_NAMES_SOURCE_PREFIX = "field_names";
	public static final String VIEW_NAMES_SOURCE_PREFIX = "view_names";

	public static final String FIELD_NAMES_RESOURCE = "camel_case_field_names.txt";
	public static final String VIEW_NAMES_RESOURCE = "camel_case_view_names.txt";

	public static final String HEADER_VIEW_NAME = "VIEW_NAME";
	public static final String HEADER_FIELD_NAME = "FIELD_NAME";
	public static final String HEADER_IS_APPROVED = "IS_APPROVED";

	private static final long LOWER_HALF_MASK = 0x00000000FFFFFFFFL;
	private static final long APPROVED_MASK   = 0x0000000080000000L;

	private static final int CASE_BITS_NOT_FOUND = 0; // reserved value which cannot occur in real 'case bits'
	private static final String LINE_SEP = System.lineSeparator();
	private static final String TAB = "\t";

	private static String[] entityPrefixLetters = null; // for view names only

	private static HashSet<String> uiPrefixes = null; // for field names only
	private static HashSet<String> entityPrefixes = null; // for view names only
	private static HashSet<String> industryPrefixes = null;
	private static HashSet<String> countryPrefixes = null;
	private static HashSet<String> productPrefixes = null;
	private static HashSet<String> extensionPrefixes = null; // for X_ view names only
	private static HashSet<String> edocProcessPrefixes = null; // for X_ view extensions
	private static HashSet<String> extensionNumberPrefixes = null; // for X_ view extensions
	private static HashSet<String> fieldTypeSuffixes = null;
	private static HashSet<String> versionSuffixes = null;

	// content from resources (lazy loading) 
	private static CamelCaseNames fieldNames;
	private static CamelCaseNames viewNames;

	private static int getHash(String text) { return text.toUpperCase().hashCode(); }
	
	private static class NameInfo {
		public final String name;
		public int count;
		public boolean isApproved;
		public String additions;
		public String sourceFile;
		public boolean hasCriticalHashCollision = false;

		private NameInfo(String name, int count, boolean isApproved, String additions, String sourceFile) {
			this.name = name;
			this.count = count;
			this.isApproved = isApproved;
			this.additions = additions;
			this.sourceFile = sourceFile;
		}

		private void add(int count, boolean isApproved, String additions, String sourceFile) {
			this.count += count;
			if (isApproved) {
				this.isApproved = true;
				this.sourceFile = sourceFile;
			}
			if (StringUtil.isNullOrEmpty(this.additions)) {
				this.additions = additions;
			}
		}
		
		private int getUpperCaseCount() {
			int result = 0;
			for (char c : name.toCharArray()) {
				if (Character.isUpperCase(c)) {
					++result;
				}
			}
			return result;
		}
	}
	
	// -------------------------------------------------------------------------
	// create methods and their helper methods 
	
	private static CamelCaseNames createEmpty(CamelCaseNameType type) { 
		return new CamelCaseNames(type, new long[0], 0); 
	}

	private static void initializeHashSets() {
		uiPrefixes = initializeHashSet( new String[] { "UICT" } ); // all added 

		entityPrefixLetters = new String[] { "A", "C", "D", "E", "F", "I", "N", "P", "R" }; // must be upper case for entityPrefixExists()
		
		entityPrefixes = initializeHashSet(entityPrefixLetters); // for "X", see below

		industryPrefixes = initializeHashSet( new String[] { 
				"CWM", "DFS", "DSD", "EWA", "ILO", "ILS", "INS", "ISU", "ODS", "OIL", "PPS", "PRA", "PSE", "PSM", "RFM", "SOM", "UTI" } );

		countryPrefixes = initializeHashSet( new String[] { // added: "EU"
				"AD", "AE", "AF", "AG", "AI", "AL", "AM", "AO", "AQ", "AR", "AS", "AT", "AU", "AW", "AZ", "BA", "BB", "BD", 
				"BE", "BF", "BG", "BH", "BI", "BJ", "BM", "BN", "BO", "BQ", "BR", "BS", "BT", "BV", "BW", "BY", "BZ", "CA", 
				"CC", "CD", "CF", "CG", "CH", "CI", "CK", "CL", "CM", "CN", "CO", "CR", "CS", "CU", "CV", "CW", "CX", "CY", 
				"CZ", "DE", "DJ", "DK", "DM", "DO", "DZ", "EC", "EE", "EG", "EH", "ER", "ES", "ET", "EU", "FI", "FJ", "FK", "FM", 
				"FO", "FR", "GA", "GB", "GD", "GE", "GF", "GG", "GH", "GI", "GL", "GM", "GN", "GP", "GQ", "GR", "GS", "GT", 
				"GU", "GW", "GY", "HK", "HM", "HN", "HR", "HT", "HU", "ID", "IE", "IL", "IM", "IN", "IO", "IQ", "IR", "IS", 
				"IT", "JE", "JM", "JO", "JP", "KE", "KG", "KH", "KI", "KM", "KN", "KP", "KR", "KW", "KY", "KZ", "LA", "LB", 
				"LC", "LI", "LK", "LR", "LS", "LT", "LU", "LV", "LY", "MA", "MC", "MD", "MG", "MH", "MK", "ML", "MM", "MN", 
				"MO", "MP", "MQ", "MR", "MS", "MT", "MU", "MV", "MW", "MX", "MY", "MZ", "NA", "NC", "NE", "NF", "NG", "NI", 
				"NL", "NO", "NP", "NR", "NU", "NZ", "OM", "PA", "PE", "PF", "PG", "PH", "PK", "PL", "PM", "PN", "PR", "PS", 
				"PT", "PW", "PY", "QA", "RE", "RO", "RS", "RU", "RW", "SA", "SB", "SC", "SD", "SE", "SG", "SH", "SI", "SJ", 
				"SK", "SL", "SM", "SN", "SO", "SR", "SS", "ST", "SV", "SX", "SY", "SZ", "TC", "TD", "TF", "TG", "TH", "TJ", 
				"TK", "TL", "TM", "TN", "TO", "TP", "TR", "TT", "TV", "TW", "TZ", "UA", "UG", "UM", "US", "UY", "UZ", "VA", 
				"VC", "VE", "VG", "VI", "VN", "VU", "WF", "WS", "YE", "YT", "ZA", "ZM", "ZW", "Z0", "Z1", "Z2" } );

		productPrefixes = initializeHashSet( new String[] { 
				"ABA", "ACM", "AIN", "ARO", "BPF", "CAI", "CBC", "CPQ", "CSL", "CTE", "EHS", "ETX", "EWM", "FCC", "FPS", 
				"FSM", "GPR", "GRC", "GTS", "HCM", "IBP", "ICM", "ILM", "JIT", "LMD", "MDG", "MIG", "MKT", "PFM", "PPM", 
				"PSM", "QM", "RFM", "S4C", "SLT", "SPC", "TAS", "VMS", "WLS" } );

		// specific for view extensions X_...
		extensionPrefixes = initializeHashSet( new String[] { "X" } );

		edocProcessPrefixes = initializeHashSet( new String[] { 
				"CONL", "DELN", "ICDL", "ICML", "INCL", "INCO", "INLE", "INSB", "INVC", "INVE", "INVI", "INVL", "INVM", 
				"INVO", "INVP", "INVR", "INVS", "INVT", "MATM", "PAYR", "RETR", "TRRE", "TXCE", "TXCV", "VIIL", "VIIS" } ); 
		
		extensionNumberPrefixes = initializeHashSet( new String[] { "1", "2", "3", "4", "5", "6", "7", "8", "9" } ); // added: "1"
		
		// suffixes
		fieldTypeSuffixes = initializeHashSet( new String[] { "E", "H" } );
		
		versionSuffixes = initializeHashSet( new String[] { "1", "2", "3", "4", "5", "6", "7", "8", "9" } ); // added: "1"
	}
	
	private static HashSet<String> initializeHashSet(String[] values) {
		HashSet<String> hashSet = new HashSet<>();
		for (String value : values) {
			hashSet.add(value);
		}
		return hashSet;
	}

	private static class GtncLine {
		private final String name;
		private final boolean isApproved;
		private final boolean isRejected;
		private final String lastChanged;
	
		private GtncLine(String name, boolean isApproved, boolean isRejected, String lastChanged) {
			this.name = name;
			this.isApproved = isApproved;
			this.isRejected = isRejected;
			this.lastChanged = lastChanged;
		}
	}

	public static String preprocessFromGTNC(CamelCaseNameType nameType, String clip, StringBuilder sb) {
		String[] lines = StringUtil.split(clip, new char[] { '\r', '\n' }, true, true);
		if (lines.length < 2)
			return "less than 2 lines found";
		
		// read header
		int nameIndex = -1;
		int statusIndex = -1;
		int lastChangedIndex = -1;
		String[] headerCells = StringUtil.split(lines[0], new char[] { '\t' }, false, true);
		for (int index = 0; index < headerCells.length; ++index) {
			String cell = headerCells[index];
			if (cell.equals("CDS View Name") || cell.equals("Field Name")) {
				nameIndex = index;
			} else if (cell.equals("Status") || cell.equals("Approval Status")) {
				statusIndex = index;
			} else if (cell.equals("Last Change Date (UTC)")) {
				lastChangedIndex = index;
			}
		}
		if (nameIndex < 0 || statusIndex < 0 || lastChangedIndex < 0)
			return "expected columns not found";
		
		// read supplied TAB-separated table
		HashMap<String, GtncLine> lineOfName = new HashMap<>();
		ArrayList<String> namesInOrder = new ArrayList<>();
		int redundantEntryCount = 0;
		String lastName = "";
		for (int lineIndex = 1; lineIndex < lines.length; ++lineIndex) {
			String[] cells = StringUtil.split(lines[lineIndex], new char[] { '\t' }, false, true);
			if (nameIndex >= cells.length || statusIndex >= cells.length || lastChangedIndex >= cells.length) 
				return "Only " + Cult.format(cells.length) + " cells found in line " + Cult.format(lineIndex) + ". Last valid name: " + lastName;

			String name = cells[nameIndex];
			String status = cells[statusIndex];
			String lastChanged = cells[lastChangedIndex];
			boolean isApproved = false;
			boolean isRejected = false;
			if (status.equalsIgnoreCase("locApproved") || status.equalsIgnoreCase("centApproved")) {
				isApproved = true;
			} else if (status.equalsIgnoreCase("locApprovalRequested") || status.equalsIgnoreCase("centApprovalRequested")) {
				isApproved = false;
			} else if (status.equalsIgnoreCase("new") || status.equalsIgnoreCase("deprecated")) {
				isApproved = false;
			} else if (status.equalsIgnoreCase("rejected")) {
				isRejected = true;
				// nevertheless enter this to lineOfName, because this might not be the latest status
			} else {
				return "unknown status: " + status;
			}
			lastName = name;
			
			GtncLine newLine = new GtncLine(name, isApproved, isRejected, lastChanged);
			GtncLine existingLine = lineOfName.get(name);
			if (existingLine == null) {
				namesInOrder.add(name);
			} else {
				++redundantEntryCount;
			}
			if (existingLine == null || lastChanged.compareTo(existingLine.lastChanged) > 0) {
				lineOfName.put(name, newLine);
			}
		}

		// create result table into the supplied StringBuilder
		final String CSV_SEP = ";";
		int approvedCount = 0;
		int notApprovedCount = 0;
		int rejectedCount = 0;
		String nameHeader = (nameType == CamelCaseNameType.VIEW) ? HEADER_VIEW_NAME : HEADER_FIELD_NAME;
		sb.append(nameHeader + CSV_SEP + HEADER_IS_APPROVED + LINE_SEP);
		for (String name : namesInOrder) {
			GtncLine gtncLine = lineOfName.get(name);
			if (gtncLine == null)  {
				continue;
 			} else if (gtncLine.isRejected) {
				++rejectedCount;
			} else if (gtncLine.isApproved) {
				sb.append(gtncLine.name + CSV_SEP + "1" + LINE_SEP);
				++approvedCount;
			} else {
				sb.append(gtncLine.name + CSV_SEP + "0" + LINE_SEP);
				++notApprovedCount;
			}
		}

		// summarize result
		StringBuilder sbSummary = new StringBuilder();
		sbSummary.append("Processed " + Cult.format(lines.length) + " lines: ");
		sbSummary.append(Cult.format(approvedCount) + " approved, ");
		sbSummary.append(Cult.format(notApprovedCount) + " not yet approved, ");
		sbSummary.append(Cult.format(rejectedCount) + " rejected." + LINE_SEP);
		if (redundantEntryCount > 0) {
			sbSummary.append("Found " + Cult.format(redundantEntryCount) + " redundant lines, using most recent status.");
		}
		
		return sbSummary.toString();
	}

	public static CamelCaseNames getFieldNames() {
		if (fieldNames == null) 
			fieldNames = createFromResources(CamelCaseNameType.FIELD, FIELD_NAMES_RESOURCE);
		return fieldNames;
	}
	
	public static CamelCaseNames getViewNames() {
		if (viewNames == null) 
			viewNames = createFromResources(CamelCaseNameType.VIEW, VIEW_NAMES_RESOURCE);
		return viewNames;
	}

	public static CamelCaseNames createFromResources(CamelCaseNameType type, String resourceName) {
		InputStream resourceStream = CamelCaseNames.class.getClassLoader().getResourceAsStream(resourceName);
		BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(resourceStream, StandardCharsets.UTF_8));
		return createFromResources(type, bufferedReader);
	}

	public static CamelCaseNames createFromResources(CamelCaseNameType type, BufferedReader bufferedReader) {
		if (bufferedReader == null)
			return createEmpty(type);

		long startTime_ms = System.currentTimeMillis();

		long[] hashAndCaseBits = null;
		try {
			String requiredVersion = bufferedReader.readLine();
			if (requiredVersion == null || Program.TECHNICAL_VERSION < Integer.parseInt(requiredVersion)) 
				return createEmpty(type);
			@SuppressWarnings("unused")
			int fileVersion = Integer.parseInt(bufferedReader.readLine());
			int entryCount = Integer.parseInt(bufferedReader.readLine());
			hashAndCaseBits = new long[entryCount];
			
			int index = 0;
			String line;
			do {
				line = bufferedReader.readLine();
				if (line == null || line.length() == 0) 
					continue;
				hashAndCaseBits[index] = Long.parseUnsignedLong(line, 16);
				++index;
			} while (line != null);

			bufferedReader.close();
		} catch (IOException e) {
		}
		long endTime_ms = System.currentTimeMillis();
		long loadDuration_ms = endTime_ms - startTime_ms;
		
		return new CamelCaseNames(type, hashAndCaseBits, loadDuration_ms);
	}

	public static CamelCaseNames createFromTextFiles(CamelCaseNameType type, String[] paths, StringBuilder sbSummary, StringBuilder sbDetails, boolean checkStartsWithZ, boolean checkUnknownPrefixesOrSuffixes) {
		long startTime_ms = System.currentTimeMillis();

		// collect valid CamelCaseNames from the supplied files
		if (sbDetails != null) {
			sbDetails.append("file" + TAB + "name" + TAB + "approved" + TAB + "discard reason" + TAB + "details").append(LINE_SEP);
		}

		HashMap<String, NameInfo> namesByCamelCase = new HashMap<>();
		HashSet<String> discardedNames = new HashSet<>();
		int readEntryCount = 0;

		Arrays.sort(paths);
	   for (String path : paths) {
			try {
				readEntryCount += createFromTextFile(type, path, sbDetails, checkStartsWithZ, checkUnknownPrefixesOrSuffixes, namesByCamelCase, discardedNames);
			} catch (IOException e) {
			}
	   }
	   
	   // fill another HashMap with hashes of (upper case) names, discarding duplicates that have a lower count
	   HashMap<Integer, NameInfo> namesByHash = new HashMap<>();
		int hashCollisionCount = 0;
		int criticalHashCollisionCount = 0;

	   for (String variantName : namesByCamelCase.keySet()) {
	   	NameInfo variant = namesByCamelCase.get(variantName);
	   	int hash = getHash(variantName);
	   	NameInfo bestVariant = namesByHash.get(hash);
	   	if (bestVariant == null) {
	   		namesByHash.put(hash, variant);
	   		continue;
	   	}
	   	if (!variantName.equalsIgnoreCase(bestVariant.name)) { 
	   		++hashCollisionCount;
	   		String discardReason;
	   		if (variantName.length() == bestVariant.name.length()) {
		   		discardReason = "critical hash collision at same length";
		   		variant.hasCriticalHashCollision = true;
		   		bestVariant.hasCriticalHashCollision = true;
	   			++criticalHashCollisionCount;
	   		} else {
		   		discardReason = "acceptable hash collision at different lengths";
	   		}
				addDetails(variant, discardReason + TAB + "with " + bestVariant.name + ": " + Integer.toHexString(hash), sbDetails);
	   	}  // do NOT attach with else if 

	   	if (variant.isApproved && !bestVariant.isApproved) {
				addDetails(bestVariant, "is not approved, unlike" + TAB + variant.name, sbDetails);
	   		namesByHash.put(hash, variant);
	   	} else if (!variant.isApproved && bestVariant.isApproved){
				addDetails(variant, "is not approved, unlike" + TAB + bestVariant.name, sbDetails);

	   	} else if (variant.count > bestVariant.count || variant.count == bestVariant.count && variant.getUpperCaseCount() > bestVariant.getUpperCaseCount()) {
				addDetails(bestVariant, "is less frequent variant" + TAB + getLessFrequentDetails(bestVariant, variant), sbDetails);
	   		namesByHash.put(hash, variant);
	   	} else {
				addDetails(variant, "is less frequent variant" + TAB + getLessFrequentDetails(variant, bestVariant), sbDetails);
   		}
	   }
	   
		// also report the names that are kept, leaving the 'discard reason' column empty
		if (sbDetails != null) {
			for (int hash : namesByHash.keySet()) {
				NameInfo bestVariant = namesByHash.get(hash);
				if (getUpperCharCount(bestVariant.name) == 1) {
					// these entries will only be used if CamelCase names exist for all fields of a VALUE expression etc. 
					addDetails(bestVariant, TAB + "upper char count = 1", sbDetails);
				} else {
					addDetails(bestVariant, "", sbDetails);
				}
			}
		}

	   // convert to a long[], which has the hash in the upper 32 bits and the caseBits in the lower 32 bits
		int validNameCount = 0;
		for (NameInfo nameInfo : namesByHash.values()) {
			if (!nameInfo.hasCriticalHashCollision) {
				++validNameCount;
			}
		}
		long[] hashAndCaseBits = new long[validNameCount];
		int index = 0;
		for (int hash : namesByHash.keySet()) {
			NameInfo nameInfo = namesByHash.get(hash);
			if (nameInfo.hasCriticalHashCollision)
				continue;
			int caseBits = extractCaseBitsFrom(nameInfo.name);
			hashAndCaseBits[index] = ((long)hash << 32) + (long)caseBits + (nameInfo.isApproved ? APPROVED_MASK : 0L);
			++index;
		}
		// sort the long[] by the hashes (which are in the upper 32 bits and unique)
		Arrays.sort(hashAndCaseBits);
		long endTime_ms = System.currentTimeMillis();
		
		// create summary
		int createDuration_ms = (int)(endTime_ms - startTime_ms);
		if (sbSummary != null) {
			sbSummary.append("Read " + Cult.format(paths.length) + " files with " + Cult.format(readEntryCount) + " entries in " + Cult.fromMillisec(createDuration_ms) + ".").append(LINE_SEP);
			sbSummary.append("Discarded " + Cult.format(discardedNames.size()) + " unique names.").append(LINE_SEP);
			sbSummary.append("Created CamelCase information for " + Cult.format(hashAndCaseBits.length) + " names.").append(LINE_SEP);
			if (hashCollisionCount == 0) {
				sbSummary.append("No hash collisions found.").append(LINE_SEP);
			} else {
				sbSummary.append("Found " + Cult.format(hashCollisionCount) + " hash collisions, of which " + Cult.format(criticalHashCollisionCount) + " are critical due to same length.").append(LINE_SEP);
			}
		}

		return new CamelCaseNames(type, hashAndCaseBits, createDuration_ms);
	}

	private static int createFromTextFile(CamelCaseNameType type, String path, StringBuilder sbDetails, boolean checkStartsWithZ, boolean checkUnknownPrefixesOrSuffixes, HashMap<String, NameInfo> namesByCamelCase, HashSet<String> discardedNames) throws IOException {
		Persistency persistency = Persistency.get();
		if (!persistency.fileExists(path)) 
			return 0;
		
		BufferedReader bufferedReader = persistency.getBufferedReader(path, false);
		if (bufferedReader == null)
			return 0;

		String sourceFile = persistency.getFileName(path);

		int readEntryCount = 0;
		boolean isFirstLineInFile = true;
		String line;
		boolean secondColContainsApprovedInfo = false;
		do {
			line = bufferedReader.readLine();

			// expected line format: 
			// # any comment
			// CamelCaseName1[;count[;further columns]]
			// CamelCaseName2[;count[;further columns]]

			// or, alternatively, with the specified header line:
			// {VIEW_NAME|FIELD_NAME};IS_APPROVED
			// ApprovedName;1
			// NotApprovedName;0

			// skip empty lines and comment lines starting with # or any other unsuited character
			if (line == null || line.length() == 0 || !ABAP.isCharAllowedForVariableNames(line, 0, true, false, false)) 
				continue;

			// read line
			String[] cells = StringUtil.split(line, ';', false);
			String camelCaseName = cells[0].trim();
			int count = secondColContainsApprovedInfo ? 0 : 1;
			boolean isApproved = false;
			if (cells.length >= 2 && ABAP.consistsOfDigitsOnly(cells[1].trim())) {
				int value = Integer.parseUnsignedInt(cells[1].trim());
				if (secondColContainsApprovedInfo) {
					isApproved = (value != 0);
				} else if (ABAP.consistsOfDigitsOnly(cells[1].trim())) {
					count = value;
				}
			}
			StringBuilder additions = new StringBuilder();
			if (cells.length >= 3) {
				for (int i = 2; i < cells.length; ++i) {
					additions.append(TAB + cells[i]);
				}
			}

			// skip header
			if (isFirstLineInFile) {
				if (camelCaseName.equals("STRUCOBJN_RAW") || camelCaseName.equals("FIELDNAME_RAW")) {
					continue;
				} else if (camelCaseName.equals(HEADER_VIEW_NAME) || camelCaseName.equals(HEADER_FIELD_NAME)) {
					secondColContainsApprovedInfo = (cells.length >= 2 && cells[1].equals(HEADER_IS_APPROVED));
					continue;
				}
			}

			isFirstLineInFile = false;
			++readEntryCount;

			// verify the name and report if it was discarded for some reason
			if (discardedNames.contains(camelCaseName)) {
				// do not analyze and report the same discarded name again if it appears multiple times (e.g. in different source files) 
				continue;
			} else {
				String discardReason = checkDiscardReasons(camelCaseName, type, checkStartsWithZ, checkUnknownPrefixesOrSuffixes, isApproved);
				if (discardReason != null) {
					addDetails(sourceFile, camelCaseName, isApproved, discardReason, sbDetails);
					discardedNames.add(camelCaseName);
					continue;
				}
			}

			// enter the name and its count to the HashMap (or "add" it if an entry with this name already exists)
			NameInfo nameInfo = namesByCamelCase.get(camelCaseName);
			if (nameInfo == null) {
				nameInfo = new NameInfo(camelCaseName, count, isApproved, additions.toString(), sourceFile);
				namesByCamelCase.put(camelCaseName, nameInfo);
			} else {
				nameInfo.add(count, isApproved, additions.toString(), sourceFile);
			}
		} while (line != null);

		bufferedReader.close();
		return readEntryCount;
	}

	public static String checkDiscardReasons(String name, CamelCaseNameType type, boolean checkStartsWithZ, boolean checkUnknownPrefixesOrSuffixes, boolean isApproved) {
		// is the name too long?
		if (name.length() > ABAP.MAX_VARIABLE_NAME_LENGTH) {
			return "too long: " + Cult.format(name.length()) + " chars";
		}
		
		// does the name contain invalid chars?
		if (!ABAP.mayBeVariableName(name, false, true)) {
			return "contains invalid chars";
		}

		int namespaceEnd = name.lastIndexOf(ABAP.NAMESPACE_SIGN) + 1;
		String namespace = (namespaceEnd == 0) ? "" : name.substring(0, namespaceEnd);
		String nameWithoutNamespace = name.substring(namespaceEnd);
		if (StringUtil.isNullOrEmpty(nameWithoutNamespace))
			return "ends with a namespace sign";
		
		// does the name start with Z (possibly after the namespace /.../)?
		if (checkStartsWithZ && Character.toUpperCase(nameWithoutNamespace.charAt(0)) == 'Z') {
			return "starts with Z";
		}

		// is the name all upper case?
		if (name.equals(name.toUpperCase())) {
			return "is all upper case";
		}

		// is the name all lower case?
		if (name.equals(name.toLowerCase())) {
			return "is all lower case";
		}

		// is the namespace all upper/lower and the remaining name all upper/lower
		if (isAllSameCase(namespace) && isAllSameCase(nameWithoutNamespace)) {
			return "is all upper/lower case";
		}

		// does the name contain any unknown prefixes or suffixes?
		String nameCore = removeAllowedPrefixesAndSuffixes(type, nameWithoutNamespace);
		if (checkUnknownPrefixesOrSuffixes && nameCore.indexOf('_') >= 0) {
			return "has unknown prefixes or suffixes" + TAB + nameCore;
		}

		// is anything left from the name after removing prefixes and suffixes?
		if (nameCore.length() == 0) {
			// this is currently not possible, because both prefixes and suffixes are all-upper case, so the name would 
			// already have been discarded above
			return "consists of prefixes or suffixes only";
		}

		// does the core name start with a lower case character?
		if (Character.isLowerCase(nameCore.charAt(0))) {
			return "starts with lower case";
		}

		if (!isApproved && nameCore.length() > 15 && nameCore.substring(1).equals(nameCore.substring(1).toLowerCase())) {
			// the longest approved single words are "Characteristic" (14 chars) and "Correspondence" (14 chars); 
			// the longest non-approved single words are "Characteristics" (15 chars), and "Classification" (14 chars); 
			// longer ones usually miss one or two upper case letters
			return "is single word with >15 chars";
		}
		
		return null;
	}

	private static boolean isAllSameCase(String name) {
		return (name.equals(name.toUpperCase()) || name.equals(name.toLowerCase()));
	}

	private static void addDetails(NameInfo nameInfo, String discardReason, StringBuilder sbDetails) {
		addDetails(nameInfo.sourceFile, nameInfo.name, nameInfo.isApproved, discardReason, sbDetails);
	}
	
	private static void addDetails(String sourceFile, String name, boolean isApproved, String discardReason, StringBuilder sbDetails) {
		if (sbDetails == null)  
			return;
		sbDetails.append(sourceFile);
		sbDetails.append(TAB + name);
		sbDetails.append(TAB + (isApproved ? "1" : ""));
		sbDetails.append(TAB + discardReason);
		sbDetails.append(LINE_SEP);
	}
	
	private static String getLessFrequentDetails(NameInfo lessFrequent, NameInfo moreFrequent) {
		return "count " + Cult.format(lessFrequent.count) + " <= " + Cult.format(moreFrequent.count) + " " + moreFrequent.name;
	}
	
	private static String removeAllowedPrefixesAndSuffixes(CamelCaseNameType nameType, String name) {
		// lazy initialization
		if (entityPrefixes == null) 
			initializeHashSets();

		// only one prefix of a kind is allowed respectively, and only in the specified order
		String nameCore = name;
		if (nameType == CamelCaseNameType.VIEW && name.startsWith("X_")) {
			nameCore = removePrefix(nameCore, extensionPrefixes); // the X_ itself
			nameCore = removePrefix(nameCore, industryPrefixes);
			nameCore = removePrefix(nameCore, countryPrefixes);
			nameCore = removePrefix(nameCore, productPrefixes);
			nameCore = removePrefix(nameCore, edocProcessPrefixes);
			nameCore = removePrefix(nameCore, extensionNumberPrefixes);
			// continue below for the prefixes of the view that is being extended
		}
		if (nameType == CamelCaseNameType.VIEW) {
			nameCore = removePrefix(nameCore, entityPrefixes);
		} else if (nameType == CamelCaseNameType.FIELD) {
			nameCore = removePrefix(nameCore, uiPrefixes);
		}
		nameCore = removePrefix(nameCore, industryPrefixes);
		nameCore = removePrefix(nameCore, countryPrefixes);
		nameCore = removePrefix(nameCore, productPrefixes);
		
		// for suffixes, start from the end
		nameCore = removeSuffix(nameCore, versionSuffixes);
		if (nameType == CamelCaseNameType.FIELD) {
			nameCore = removeSuffix(nameCore, fieldTypeSuffixes);
		}
		if (nameType == CamelCaseNameType.VIEW) {
			// suffix _F1234, _F1234A
			int len = nameCore.length();
			if (len > 6 && nameCore.substring(len - 6, len - 4).equals("_F") 
					&& ABAP.consistsOfDigitsOnly(nameCore.substring(len - 4))) {
				nameCore = nameCore.substring(0, len - 6);
	
			} else if (len > 7 && nameCore.substring(len - 7, len - 5).equals("_F") 
					&& ABAP.consistsOfDigitsOnly(nameCore.substring(len - 5, len - 1))
					&& Character.isUpperCase(nameCore.charAt(len - 1))) {
				nameCore = nameCore.substring(0, len - 7);
			}
		}
		
		return nameCore;
	}
	
	private static String removePrefix(String name, HashSet<String> prefixes) {
		// if the prefix (case-sensitive!) is found in the HashSet, remove it, together with the underscore 
		int pos = name.indexOf('_');
		if (pos < 0 || !prefixes.contains(name.substring(0, pos))) {
			return name;
		} else  { 
			return name.substring(pos + 1);
		}
	}

	private static String removeSuffix(String name, HashSet<String> suffixes) {
		// if the prefix (case-sensitive!) is found in the HashSet, remove it, together with the underscore 
		int pos = name.lastIndexOf('_');
		if (pos < 0 || !suffixes.contains(name.substring(pos + 1))) {
			return name;
		} else  { 
			return name.substring(0, pos);
		}
	}

	private static int extractCaseBitsFrom(String camelCaseText) {
		int caseBitMap = 0;
		int mask = 1;
		for (char c : camelCaseText.toCharArray()) {
			if (Character.isUpperCase(c))
				caseBitMap |= mask;
			mask *= 2;
		}
		// as an additional check, set the 'length + 1' bit
		caseBitMap |= mask;
		return caseBitMap;
	}

	private static String applyCamelCaseTo(String text, int caseBits, boolean requireUpperAfterLower, boolean requireApproval) {
		if (caseBits == CASE_BITS_NOT_FOUND)
			return null;

		boolean isApproved = ((caseBits & APPROVED_MASK) == APPROVED_MASK);
		if (requireApproval && !isApproved)
			return null;
		if (isApproved)
			caseBits ^= APPROVED_MASK;

		StringBuilder sb = new StringBuilder(text.length());
		boolean hasUpperAfterLower = false;
		boolean prevWasUpper = false;
		boolean prevWasLower = false;
		boolean prevWasDigit = false;
		boolean prevPrevWasUpperOrDigit = false;
		int mask = 1;

		for (char c : text.toCharArray()) {
			boolean changeToUpper = ((caseBits & mask) == mask);

			if (Character.isLetter(c)) {
				if (changeToUpper) {
					if (prevWasLower) {
						// this pattern 'xY' makes this a 'sure match' for a CamelCase name, because in ABAP style, there would 
						// have to be an underscore in this place to separate the two words (camel_case instead of CamelCase)
						hasUpperAfterLower = true;
					}
					sb.append(Character.toUpperCase(c));
					prevPrevWasUpperOrDigit = prevWasUpper || prevWasDigit;
					prevWasUpper = true;
					prevWasLower = false;
					prevWasDigit = false;

				} else {
					if (prevPrevWasUpperOrDigit && prevWasUpper) {
						// the patterns 'XYz' and '1Xy' also count as a 'sure match', 
						// because they would have to be 'x_yz' and '1_xy' an ABAP style
						hasUpperAfterLower = true;
					}
					sb.append(Character.toLowerCase(c));
					prevPrevWasUpperOrDigit = prevWasUpper || prevWasDigit;
					prevWasUpper = false;
					prevWasLower = true;
					prevWasDigit = false;
				}

			} else {
				sb.append(c);
				prevPrevWasUpperOrDigit = prevWasUpper || prevWasDigit;
				prevWasUpper = false;
				prevWasLower = false;
				prevWasDigit = Character.isDigit(c);
			}

			if (changeToUpper)
				caseBits ^= mask;
			mask *= 2;
		}
		// expect that only the 'length + 1' bit is left: otherwise, the caseBits belong to a different name which happens 
		// to have a hash collision with text, but has a different length (note that the APPROVED_MASK bit was already removed)
		if (caseBits != mask) 
			return null;
		
		if (requireUpperAfterLower && !hasUpperAfterLower)
			return null;

		return sb.toString();
	}

	private static int getUpperCharCount(String text) {
		int upperCount = 0;
		for (char c : text.toCharArray()) {
			if (Character.isUpperCase(c)) {
				++upperCount;
			} else if (c == ABAP.NAMESPACE_SIGN) {
				upperCount = 0;
			}
		}
		return upperCount;
	}
	
	// -------------------------------------------------------------------------

	public final CamelCaseNameType nameType;
	
	/** Each long value stores in its upper 32 bits the hash code of a (field or view) NAME, and in its lower 32 bits the 
	 * 'case bits', with which the CamelCase representation of the name can be retrieved: the 'case bits' contain '0' for a 
	 * lower case character, and 1 for an upper case character. The least significant bit represents the last character of the name. */
	private long[] hashAndCaseBits;

	public final long createDuration_ms;
	
	public int getEntryCount() { return (hashAndCaseBits == null) ? -1 : hashAndCaseBits.length; }
	
	// -------------------------------------------------------------------------

	private CamelCaseNames(CamelCaseNameType nameType, long[] hashAndCaseBits, long createDuration_ms) {
		this.nameType = nameType;
		this.hashAndCaseBits = hashAndCaseBits;
		this.createDuration_ms = createDuration_ms;
	}
	
	public void saveAsResource(String path) throws IOException {
		final int requiredTechnicalVersion = 24;
		final String LINE_SEP = "\n";

		BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path), StandardCharsets.UTF_8));
		bufferedWriter.write(Integer.toString(requiredTechnicalVersion) + LINE_SEP);
		bufferedWriter.write(Integer.toString(Program.TECHNICAL_VERSION) + LINE_SEP);
		bufferedWriter.write(Integer.toString(hashAndCaseBits.length) + LINE_SEP);
		for (long hashAndCase : hashAndCaseBits) {
			bufferedWriter.write(Long.toHexString(hashAndCase) + LINE_SEP);
		}
		bufferedWriter.close();
	}
	
	public String applyCamelCaseTo(String text, boolean requireUpperAfterLower, boolean requireApproval, Profile profile) {
		String camelCase = applyCamelCaseTo(text, getCaseBitsFor(text), requireUpperAfterLower, requireApproval);
		
		// if no name was found, search in the custom names, if any
		if (camelCase == null && profile != null) {
			CustomCamelCaseNames customNames = (nameType == CamelCaseNameType.VIEW) ? profile.customViewNames : profile.customFieldNames;
			if (customNames != null) {
				camelCase = customNames.applyCamelCaseTo(text);
			}
		}

		return camelCase;
	}

	private int getCaseBitsFor(String text) {
		int searchHash = getHash(text);
		long searchValue = ((long)searchHash << 32);

		// the required entry must be a bit higher than the searchValue 
		int fmin = -1;
		int fmax = hashAndCaseBits.length;
		while (fmin + 1 < fmax) {
			int fmid = (fmin + fmax) / 2;
			long midValue = hashAndCaseBits[fmid];
			if (midValue < searchValue) {
				fmin = fmid;
			} else if (midValue > searchValue) {
				fmax = fmid;
			} else { // pro forma
				fmin = fmid;
				fmax = fmid;
				break;
			}
		}
		if (fmax >= hashAndCaseBits.length) 
			return CASE_BITS_NOT_FOUND;
		
		long foundHashAndCaseBits = hashAndCaseBits[fmax]; 
		if ((int)(foundHashAndCaseBits >> 32) == searchHash) {
			// found a suitable entry
			return (int)(foundHashAndCaseBits & LOWER_HALF_MASK);
		} else {
			return CASE_BITS_NOT_FOUND;
		}
	}

	public static String[] getEntityPrefixLetters() { 
		// lazy initialization
		if (entityPrefixes == null) 
			initializeHashSets();
		return entityPrefixLetters; 
	}
	
	public static boolean entityPrefixExists(String entityPrefixLetter) { 
		// lazy initialization
		if (entityPrefixes == null) 
			initializeHashSets();
		return entityPrefixes.contains(entityPrefixLetter.toUpperCase()); 
	}
}
