package com.sap.adt.abapcleaner.rulehelpers;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedReader;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.programbase.PersistencyDouble;

public class CamelCaseNamesTest {
	private static final String LINE_SEP = System.lineSeparator();
	private static final String CSV_SEP = ";";
	private static final String TAB = "\t";

	private static final String colHeaderField = "FIELDNAME_RAW";
	private static final String colHeaderView = "STRUCOBJN_RAW";
	private static final String colHeaderCount = "COUNT";
	
	private static final String camelCaseField1 = "AnyCamelCaseName";
	private static final String camelCaseField2 = "OtherCamelCaseName";

	private PersistencyDouble persistencyDouble;
	private String path1;
	private StringBuilder sb;
	
	// result of callCreateFromFiles()
	private String summary = null;
	private String details = null;
	private CamelCaseNames names = null;
	
	@BeforeEach
	void setUp() {
		sb = new StringBuilder();
		persistencyDouble = PersistencyDouble.create();
		path1 = persistencyDouble.getAnyNewPath();
		summary = null;
		details = null;
		names = null;
	}

	@Test
	public void testHasFieldNameResource() {
		CamelCaseNames fieldNames = CamelCaseNames.getFieldNames();
		assertTrue(fieldNames.getEntryCount() > 0);
		assertEquals("CompanyCode", fieldNames.applyCamelCaseTo("companycode", true, true, null));
		assertEquals(null, fieldNames.applyCamelCaseTo("currency", true, false, null));
	}

	@Test
	public void testHasViewNameResource() {
		CamelCaseNames viewNames = CamelCaseNames.getViewNames();
		assertTrue(viewNames.getEntryCount() > 0);
		assertEquals("I_CompanyCode", viewNames.applyCamelCaseTo("i_companycode", true, true, null));
	}

	@Test
	public void testCreateFromResourceEmpty() {
		BufferedReader noReader = null;
		CamelCaseNames viewNames = CamelCaseNames.createFromResources(CamelCaseNameType.VIEW, noReader);
		assertEquals(0, viewNames.getEntryCount());
	}

	// -------------------------------------------------------------------------
	
	private void addLine(String camelCaseName) {
		sb.append(camelCaseName).append(LINE_SEP);
	}

	private void addLine(String camelCaseName, int count) {
		sb.append(camelCaseName + CSV_SEP + Integer.toString(count)).append(LINE_SEP);
	}

	private void addLine(String camelCaseName, int count, String addition) {
		sb.append(camelCaseName + CSV_SEP + Integer.toString(count) + CSV_SEP + addition).append(LINE_SEP);
	}

	private void addLines(String... lines) {
		for (String line : lines) {
			sb.append(line).append(LINE_SEP); 
		}
	}

	private void addHeaderWithApprovedCol(CamelCaseNameType type) {
		sb.append((type == CamelCaseNameType.VIEW) ? CamelCaseNames.HEADER_VIEW_NAME : CamelCaseNames.HEADER_FIELD_NAME); 
		sb.append(CSV_SEP + CamelCaseNames.HEADER_IS_APPROVED).append(LINE_SEP);
	}

	private void addLine(String camelCaseName, boolean isApproved) {
		sb.append(camelCaseName + CSV_SEP + (isApproved ? "1" : "0")).append(LINE_SEP);
	}

	private void callCreateFromFiles(CamelCaseNameType type, boolean createStringBuilders) {
		persistencyDouble.writeAllTextToFile(path1, sb.toString(), false);
		StringBuilder sbSummary = createStringBuilders ? new StringBuilder() : null;
		StringBuilder sbDetails = createStringBuilders ? new StringBuilder() : null;
		names = CamelCaseNames.createFromTextFiles(type, new String[] { path1 }, sbSummary, sbDetails, true, true);
		summary = createStringBuilders ? sbSummary.toString() : null;
		details = createStringBuilders ? sbDetails.toString() : null; 
	}

	private void retestWithoutStringBuilders(CamelCaseNameType type) {
		callCreateFromFiles(type, false);
		assertNotNull(names);
		assertNull(summary);
		assertNull(details);
	}

	private void assertNameCount(int nameCount) {
		assertNotNull(names);
		assertEquals(nameCount, names.getEntryCount());
	}
	
	private void assertKept(String name, String... nameVariants) {
		assertKept(name, true, false, nameVariants);
	}

	private void assertKept(String name, boolean expUpperAfterLower, boolean expApproval, String... nameVariants) {
		// expect that the same CamelCase name is returned when passing name in given, lower, or upper case
		assertEquals(name, names.applyCamelCaseTo(name, expUpperAfterLower, expApproval, null));
		assertEquals(name, names.applyCamelCaseTo(name.toLowerCase(), expUpperAfterLower, expApproval, null));
		assertEquals(name, names.applyCamelCaseTo(name.toUpperCase(), expUpperAfterLower, expApproval, null));
		
		if (!expUpperAfterLower) {
			// expect that applyCamelCaseTo() returns null if requireUpperAfterLower == true is demanded 
			assertEquals(null, names.applyCamelCaseTo(name, true, expApproval, null));
			assertEquals(null, names.applyCamelCaseTo(name.toLowerCase(), true, expApproval, null));
			assertEquals(null, names.applyCamelCaseTo(name.toUpperCase(), true, expApproval, null));
		}
		if (!expApproval) {
			// expect that applyCamelCaseTo() returns null if requireApproval == true is demanded 
			assertEquals(null, names.applyCamelCaseTo(name, expUpperAfterLower, true, null));
			assertEquals(null, names.applyCamelCaseTo(name.toLowerCase(), expUpperAfterLower, true, null));
			assertEquals(null, names.applyCamelCaseTo(name.toUpperCase(), expUpperAfterLower, true, null));
		}
		
		if (nameVariants != null) {
			for (String nameVariant : nameVariants) {
				assertEquals(name, names.applyCamelCaseTo(nameVariant, expUpperAfterLower, expApproval, null));
			}
		}
	}

	private void assertDiscarded(String name, String... messageBits) {
		String camelCase = names.applyCamelCaseTo(name, true, false, null);
		if (camelCase == null) {
			assertNull(names.applyCamelCaseTo(name, true, false, null));
			assertNull(names.applyCamelCaseTo(name.toLowerCase(), true, false, null));
			assertNull(names.applyCamelCaseTo(name.toUpperCase(), true, false, null));
		} else {
			// if a CamelCase name is returned, expect it to be different (i.e. to have upper case characters in different places) 
			assertFalse(name.equals(camelCase));
		}
		
		if (messageBits == null || messageBits.length == 0)
			return;

		// expect the supplied name to be found in the details
		String nameWithSeps = "\t" + name + "\t";
		int pos = details.indexOf(nameWithSeps);
		assertTrue(pos > 0);
		int nameEnd = pos + nameWithSeps.length();

		// expect the message snippet to be found in the next column(s) after the name
		int lineEnd = details.indexOf('\n', nameEnd);
		assertTrue(lineEnd > 0);
		String detail = details.substring(nameEnd, lineEnd);
		for (String messageBit : messageBits)
			assertTrue(detail.indexOf(messageBit) >= 0);
		
		// expect the name to not be reported twice (unless a hash collision is reported)
		if (detail.indexOf("hash collision") < 0) {
			assertEquals(-1, details.indexOf(nameWithSeps, lineEnd));
		}
	}

	// -------------------------------------------------------------------------
	
	@Test
	public void testEmptyFieldFile() {
		callCreateFromFiles(CamelCaseNameType.FIELD, true);
		
		assertNameCount(0);
		assertNotNull(summary);
		assertNotNull(details);
	}

	@Test
	public void testEmptyFieldFileNoReport() {
		callCreateFromFiles(CamelCaseNameType.FIELD, false);
		
		assertNameCount(0);
		assertNull(summary);
		assertNull(details);
	}

	@Test
	public void testCommentLineOnlyInFieldFile() {
		addLines("# comment", "\" comment", "", "* comment");
		
		callCreateFromFiles(CamelCaseNameType.FIELD, true);
		
		assertNameCount(0);
	}

	@Test
	public void testFieldsHeaderAnd1Entry() {
		addLine(colHeaderField + CSV_SEP + colHeaderCount + CSV_SEP + colHeaderView);
		addLine(camelCaseField1, 10, "I_AnyView");
		
		callCreateFromFiles(CamelCaseNameType.FIELD, true);
		
		assertNameCount(1);
		assertKept(camelCaseField1);
		assertDiscarded(camelCaseField2);

		retestWithoutStringBuilders(CamelCaseNameType.FIELD);
	}
	
	@Test
	public void testFieldNameDiscarded() {
		String overlongName = "ThisIdentifierIsFarTooLongForAField";
		String upperCaseName = "THISNAMEISALLUPPER";
		String lowerCaseName = "thisnameisalllower";
		String startWithZ = "ZNameStartingWithZ";
		String startWithLower = "thisNameStartsWithLower";
		String onlyPrefixes = "ISU_BR_";
		String onlyPrefixesAndSuffix = "ISU_BR__2";
		String singleOverlongWord = "Thisistoolongtojustbeoneword";
		
		addLine(camelCaseField1, 10);
		addLine(overlongName, 1);
		addLine(upperCaseName, 2);
		addLine(lowerCaseName, 3);
		addLine(startWithZ, 4);
		addLine(startWithLower, 5);
		addLine(onlyPrefixes, 6);
		addLine(onlyPrefixesAndSuffix, 7);
		addLine(singleOverlongWord, 8);

		callCreateFromFiles(CamelCaseNameType.FIELD, true);
		
		assertNameCount(1);
		assertKept(camelCaseField1);
		assertDiscarded(overlongName, "too long");
		assertDiscarded(upperCaseName, "all upper");
		assertDiscarded(lowerCaseName, "all lower");
		assertDiscarded(startWithZ, "starts with Z");
		assertDiscarded(startWithLower, "starts with lower");
		assertDiscarded(onlyPrefixes, "all upper");
		assertDiscarded(onlyPrefixesAndSuffix, "all upper");
		assertDiscarded(singleOverlongWord, "is single word with >15 chars");
		
		retestWithoutStringBuilders(CamelCaseNameType.FIELD);
	}
	
	@Test
	public void testFieldWithNamespaceDiscarded() {
		String upperAndLower = "/UCASE/restisalllowercase";
		String lowerAndUpper = "/lcase/RESTISALLUPPERCASE";
		String upperAndCamel = "/UCASE/RestIsValidCamelCase";
		String upperAndLowerStart = "/UCASE/lowerCaseStart";
		String lowerAndZStart = "/lcase/ZNameStartingWithZ";
		String upperAndLower2 = upperAndLower; 
		String endWithNamespace = "/ANY/";
		
		addLine(upperAndLower, 1);
		addLine(lowerAndUpper, 2);
		addLine(camelCaseField2, 10);
		addLine(upperAndCamel, 3);
		addLine(upperAndLowerStart, 4);
		addLine(lowerAndZStart, 5);
		addLine(upperAndLower2, 6);
		addLine(endWithNamespace, 6);
		
		callCreateFromFiles(CamelCaseNameType.FIELD, true);

		assertNameCount(2);
		assertDiscarded(upperAndLower, "all upper/lower");
		assertDiscarded(lowerAndUpper, "all upper/lower");
		assertKept(camelCaseField2);
		assertKept(upperAndCamel);
		assertDiscarded(upperAndLowerStart, "starts with lower");
		assertDiscarded(lowerAndZStart, "starts with Z");
		assertDiscarded(endWithNamespace, "ends with a namespace sign");

		retestWithoutStringBuilders(CamelCaseNameType.FIELD);
	}
	
	@Test
	public void testUnknownPrefixesandSuffixes() {
		String upperAtStartOnly = "Upperstartonly";
		String upperAfterUnderscore = "Upper_After_Underscore";
		String allUpperOrAfterUnderscore = "ALL_Upper_OR_After_Uscore";
		String lowerAfterUnderscore = "PREFIX_lower_AfterUnderscore";
		String lowerAfterLastUnderscore = "HasLowerOnlyAfter_last";
		String twoUpperThenLower = "ANYlower";
		String digitUpperLower = "PREFIX_1Any";
		
		addLine(camelCaseField1, 10);
		addLine(upperAtStartOnly, 1);
		addLine(upperAfterUnderscore, 2);
		addLine(allUpperOrAfterUnderscore, 3);
		addLine(lowerAfterUnderscore, 4);
		addLine(lowerAfterLastUnderscore, 5);
		addLine(twoUpperThenLower, 6);
		addLine(digitUpperLower, 7);
		
		callCreateFromFiles(CamelCaseNameType.FIELD, true);
		
		assertNameCount(3);
		assertKept(camelCaseField1);
		assertKept(upperAtStartOnly, false, false);
		assertDiscarded(upperAfterUnderscore, "unknown prefixes or suffixes");
		assertDiscarded(allUpperOrAfterUnderscore, "unknown prefixes or suffixes");
		assertDiscarded(lowerAfterUnderscore, "unknown prefixes or suffixes");
		assertDiscarded(lowerAfterLastUnderscore, "unknown prefixes or suffixes");
		assertKept(twoUpperThenLower);
		assertDiscarded(digitUpperLower, "unknown prefixes or suffixes");

		retestWithoutStringBuilders(CamelCaseNameType.FIELD);
	}
	
	@Test
	public void testDifferentCount() {
		String name1a = "AnyCamelCaseName";
		String name1b = "AnyCamelCasename";
		String name1c = "AnyCamelcaseName";
		String name2a = "OthercamelCaseName1";
		String name2b = "OtherCamelCaseName1";
		String name2c = "OtherCamelCasename1";
		
		// name1a has the highest count (10 + 10 = 20) 
		addLine(name1a, 10);
		addLine(name1b, 15);
		addLine(name1c, 5, "addition");
		addLine(name1a, 10);
		// name2b has the highest count (3) and most upper case characters
		addLine(name2a);
		addLine(name2b, 3);
		addLine(name2c, 3);
		
		callCreateFromFiles(CamelCaseNameType.FIELD, true);
		
		assertNameCount(2);
		assertKept(name1a, name1b, name1c);
		assertDiscarded(name1b, "less frequent", "15 <=");
		assertDiscarded(name1c, "less frequent", "5 <=");
		assertDiscarded(name2a, "less frequent", "1 <=");
		assertKept(name2b, name2a, name2c);
		assertDiscarded(name2c, "less frequent", "3 <=");
		
		retestWithoutStringBuilders(CamelCaseNameType.FIELD);
	}
	
	@Test
	public void testHashCollision() {
		// test a known hash collision
		String hashCollisionName1 = "ActualGrossVolumeDiffVal";
		String hashCollisionName2 = "SalesDocsExclusionListValue";
		
		// name1a has the highest count (10 + 10 = 20) 
		addLine(hashCollisionName1);
		addLine(camelCaseField1);
		addLine(hashCollisionName2);
		addLine(camelCaseField2);
		
		callCreateFromFiles(CamelCaseNameType.FIELD, true);
		
		assertNameCount(3);
		assertDiscarded(hashCollisionName1, "hash collision");
		assertKept(camelCaseField1);
		assertKept(hashCollisionName2);
		assertKept(camelCaseField2);

		retestWithoutStringBuilders(CamelCaseNameType.FIELD);
	}

	@Test
	public void testAllowedViewNames() {
		String normalViewName = "I_AnyViewName";
		String privateViewName = "P_OtherViewName";
		String allowedPrefixes = "A_ILS_DE_ABA_AnyName_2";
		String extendedPrefixes = "X_INVI_I_ACM_OtherName";
		String fioriNumberSuffix = "C_AnyView_F1234";
		String fioriNumberAndVersion = "C_AnyView_F1234_2";
		String fioriNumberAndLetterSuffix = "C_OtherView_F5678A";
		String shortViewName = "C_Any";
		
		addLine(normalViewName, 1);
		addLine(privateViewName, 2);
		addLine(allowedPrefixes, 3);
		addLine(extendedPrefixes, 4);
		addLine(fioriNumberSuffix, 5);
		addLine(fioriNumberAndVersion, 6);
		addLine(fioriNumberAndLetterSuffix, 7);
		addLine(shortViewName, 8);

		callCreateFromFiles(CamelCaseNameType.VIEW, true);
		
		assertNameCount(8);
		assertKept(normalViewName);
		assertKept(privateViewName);
		assertKept(allowedPrefixes);
		assertKept(extendedPrefixes);
		assertKept(fioriNumberSuffix);
		assertKept(fioriNumberAndVersion);
		assertKept(fioriNumberAndLetterSuffix);
		assertKept(shortViewName, false, false);
		
		retestWithoutStringBuilders(CamelCaseNameType.VIEW);
	}
	
	@Test
	public void testDiscardedViewNames() {
		String noFioriNumber = "I_AnyView_F123A";
		String noFioriNumberEither = "I_OtherView_F456AB";
		String fioriLetterLower = "I_THirdView_F1234c";
		
		addLine(noFioriNumber, 1);
		addLine(noFioriNumberEither, 2);
		addLine(fioriLetterLower, 3);

		callCreateFromFiles(CamelCaseNameType.VIEW, true);
		
		assertNameCount(0);
		assertDiscarded(noFioriNumberEither, "unknown prefixes or suffixes");
		assertDiscarded(noFioriNumber, "unknown prefixes or suffixes");
		assertDiscarded(fioriLetterLower, "unknown prefixes or suffixes");
		
		retestWithoutStringBuilders(CamelCaseNameType.VIEW);
	}
	
	@Test
	public void testWithApprovedCol() {
		addHeaderWithApprovedCol(CamelCaseNameType.VIEW);
		addLine("I_AnyView", true);
		addLine("C_OtherView", false);
		addLine("I_Thirdview", false);
		addLine("I_ThirdView", true);
		addLine("I_FourthvieW", false);
		addLine("I_FourthView", true);
		addLine("I_FourtHviEW", false);
		
		callCreateFromFiles(CamelCaseNameType.VIEW, true);
		
		assertNameCount(4);
		assertKept("I_AnyView", true, true);
		assertKept("C_OtherView", true, false);
		assertDiscarded("I_Thirdview", "is not approved, unlike");
		assertKept("I_ThirdView", true, true);
		assertDiscarded("I_FourthvieW", "is not approved, unlike");
		assertKept("I_FourthView", true, true);
		assertDiscarded("I_FourtHviEW", "is not approved, unlike");
	}
	
	// =========================================================================

	private static final String changeDate1 = "2024-03-12 09:15:00";
	private static final String changeDate2 = "2024-03-22 11:00:32";
	private static final String changeDate3 = "2024-03-22 11:00:50";
	
	private void callPreprocessExpError(CamelCaseNameType nameType, String clip, String expResult) {
		StringBuilder sb = new StringBuilder();
		String actResult = CamelCaseNames.preprocessFromGTNC(nameType, clip, sb);
		assertTrue(sb.length() == 0);
		assertTrue(actResult.indexOf(expResult) >= 0);
	}
	
	@Test
	public void testPreprocessEmpty() {
		callPreprocessExpError(CamelCaseNameType.FIELD, "any" + TAB + "header" + LINE_SEP, "less than 2 lines found");
	}
	
	@Test
	public void testPreprocessWrongHeader() {
		String anyContentLine = LINE_SEP + "any" + TAB + "content" + TAB + "line" + LINE_SEP;
		String expMessage = "expected columns not found";
		callPreprocessExpError(CamelCaseNameType.VIEW, "Field Name" + TAB + "Approval Status" + TAB + "Creation Date" + anyContentLine, expMessage);
		callPreprocessExpError(CamelCaseNameType.VIEW, "Name" + TAB + "Last Change Date (UTC)" + TAB + "Status" + anyContentLine, expMessage);
		callPreprocessExpError(CamelCaseNameType.VIEW, "CDS View Name" + TAB + "Last Change Date (UTC)" + anyContentLine, expMessage);
	}

	@Test
	public void testPreprocessMissingCells() {
		String expMessage = "cells found in line";
		callPreprocessExpError(CamelCaseNameType.VIEW, getHeader(CamelCaseNameType.VIEW) + "AnyName" + TAB + "locApproved" + LINE_SEP, expMessage);
		callPreprocessExpError(CamelCaseNameType.VIEW, getHeader(CamelCaseNameType.VIEW) + "AnyName" + LINE_SEP, expMessage);
	}
	
	@Test
	public void testPreprocessUnknownStatus() {
		String expMessage = "unknown status";
		callPreprocessExpError(CamelCaseNameType.VIEW, getHeader(CamelCaseNameType.VIEW) + getAnyLineWithStatus("unknown"), expMessage);
		callPreprocessExpError(CamelCaseNameType.VIEW, getHeader(CamelCaseNameType.VIEW) + getAnyLineWithStatus("approved"), expMessage);
		callPreprocessExpError(CamelCaseNameType.VIEW, getHeader(CamelCaseNameType.VIEW) + getAnyLineWithStatus(""), expMessage);
	}
	
	private void callPreprocess(CamelCaseNameType nameType, String clip, String expResult, String... summaryBits) {
		StringBuilder sb = new StringBuilder();
		String actSummary = CamelCaseNames.preprocessFromGTNC(nameType, clip, sb);
		assertEquals(sb.toString(), expResult);
		for (String summaryBit : summaryBits) {
			assertTrue(actSummary.indexOf(summaryBit) >= 0);
		}
	}
	
	private String getHeader(CamelCaseNameType nameType, String... addColumns) {
		StringBuilder sb = new StringBuilder();
		sb.append((nameType == CamelCaseNameType.VIEW) ? "CDS View Name" : "Field Name");
		for (String addColumn : addColumns) {
			sb.append(TAB + addColumn);
		}
		if (nameType == CamelCaseNameType.VIEW) {
			sb.append(TAB + "Status" + TAB + "Last Change Date (UTC)" + LINE_SEP);
		} else {
			sb.append(TAB + "Approval Status" + TAB + "Last Change Date (UTC)" + LINE_SEP);
		}
		return sb.toString();
	}

	private String getAnyLineWithStatus(String status) {
		return "AnyName" + TAB + status + TAB + changeDate1 + LINE_SEP;
	}
	
	private String getLine(String name, String status, String date, String... addCells) {
		StringBuilder sb = new StringBuilder();
		sb.append(name);
		for (String addCell : addCells) {
			sb.append(TAB + addCell);
		}
		sb.append(TAB + status + TAB + date + LINE_SEP);
		return sb.toString();
	}

	private String getExpResult(CamelCaseNameType nameType, String... namesWithPlus) {
		StringBuilder sbExpResult = new StringBuilder();
		sbExpResult.append((nameType == CamelCaseNameType.VIEW) ? CamelCaseNames.HEADER_VIEW_NAME : CamelCaseNames.HEADER_FIELD_NAME);
		sbExpResult.append(";" + CamelCaseNames.HEADER_IS_APPROVED + LINE_SEP);
		for (String nameWithPlus : namesWithPlus) {
			if (nameWithPlus.startsWith("+")) {
				sbExpResult.append(nameWithPlus.substring(1) + ";1" + LINE_SEP);
			} else {
				sbExpResult.append(nameWithPlus + ";0" + LINE_SEP);
			}
		}
		return sbExpResult.toString();
	}

	@Test
	public void testPreprocessField7Lines() {
		CamelCaseNameType nameType = CamelCaseNameType.FIELD;
		StringBuilder sbClip = new StringBuilder();
		sbClip.append(getHeader(nameType));
		sbClip.append(getLine("AnyName", "locApproved", changeDate1));
		sbClip.append(getLine("OtherName", "centApproved", changeDate1));
		sbClip.append(getLine("ThirdName", "locApprovalRequested", changeDate2));
		sbClip.append(getLine("FourthName", "centApprovalRequested", changeDate2));
		sbClip.append(getLine("FifthName", "new", changeDate3));
		sbClip.append(getLine("SixthName", "deprecated", changeDate3));
		sbClip.append(getLine("SeventhName", "rejected", changeDate1));
		String expResult = getExpResult(nameType, "+AnyName", "+OtherName", "ThirdName", "FourthName", "FifthName", "SixthName");
		callPreprocess(nameType, sbClip.toString(), expResult, "2 approved", "4 not yet approved", "1 rejected");
	}

	@Test
	public void testPreprocessRedundantLines() {
		// expect that the entries with changeDate3 are used, regardless of the sequence
		CamelCaseNameType nameType = CamelCaseNameType.VIEW;
		StringBuilder sbClip = new StringBuilder();
		sbClip.append(getHeader(nameType));
		sbClip.append(getLine("I_anywrongname", "new", changeDate1));
		sbClip.append(getLine("C_AnyCorrectName", "centApproved", changeDate3));
		sbClip.append(getLine("I_anywrongname", "locApprovalRequested", changeDate2));
		sbClip.append(getLine("C_AnyCorrectName", "centApprovalRequested", changeDate2));
		sbClip.append(getLine("I_anywrongname", "rejected", changeDate3));
		sbClip.append(getLine("C_AnyCorrectName", "new", changeDate1));
		String expResult = getExpResult(nameType, "+C_AnyCorrectName");
		callPreprocess(nameType, sbClip.toString(), expResult, "1 approved", "1 rejected", "4 redundant lines");
	}

	@Test
	public void testPreprocessWithNoise() {
		// insert another column after the name, which contains quotation marks, tabs and line breaks
		CamelCaseNameType nameType = CamelCaseNameType.FIELD;
		StringBuilder sbClip = new StringBuilder();
		sbClip.append(getHeader(nameType, "Comment"));
		sbClip.append(getLine("AnyName", "locApproved", changeDate1, "any comment"));
		sbClip.append(getLine("OtherName", "centApproved", changeDate1, "\"full quote\""));
		sbClip.append(getLine("ThirdName", "locApprovalRequested", changeDate2, "\"text with \"\"quote\"\" inside\""));
		sbClip.append(getLine("FourthName", "centApprovalRequested", changeDate2, "\"text with \t tab \n\n and newline\""));
		sbClip.append(getLine("FifthName", "new", changeDate3, "\"text with single \"\" inside\""));
		sbClip.append(getLine("SixthName", "deprecated", changeDate3, ""));
		sbClip.append(getLine("SeventhName", "rejected", changeDate1, "other comment"));
		String expResult = getExpResult(nameType, "+AnyName", "+OtherName", "ThirdName", "FourthName", "FifthName", "SixthName");
		callPreprocess(nameType, sbClip.toString(), expResult, "2 approved", "4 not yet approved", "1 rejected");
	}
}
