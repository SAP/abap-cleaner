package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class ClassDefinitionTest extends RuleTestBase {
	private ClassDefinitionRule rule;
	
	ClassDefinitionTest() {
		super(RuleID.CLASS_DEFINITION);
		rule = (ClassDefinitionRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMaxLineLength.setValue(120);
		rule.configIndentStyle.setEnumValue(ClassDefIndentStyle.PLUS_2);
		rule.configOneLinerAction.setEnumValue(ClassDefOneLinerAction.KEEP);
		rule.configNewLineForPublic.setValue(true);
		rule.configNewLineForInheriting.setValue(true);
		rule.configNewLineForAbstractOrFinal.setValue(false);
		rule.configNewLineForCreate.setValue(true);
		rule.configNewLineForSharedMemory.setValue(true);
		rule.configNewLineForTesting.setValue(true);
		rule.configNewLineForRiskAndDuration.setValue(false);
		rule.configNewLineForBehavior.setValue(true);
		rule.configNewLineForFriends.setValue(true);
		rule.configNewLineForLocalFriends.setValue(false);
		rule.configNewLineForFriendNames.setValue(true);
	}
	
	@Test
	void testClassDefinitionDeferredAndLoadSkipped() {
		buildSrc("CLASS cl_any_local_class DEFINITION DEFERRED.");
		buildSrc("CLASS cl_other_local_class DEFINITION LOAD.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testCommentSkipped() {
		buildSrc("CLASS cl_any_class_with_friend DEFINITION \" any comment");
		buildSrc("  PUBLIC FINAL CREATE PRIVATE \" other comment");
		buildSrc("  GLOBAL FRIENDS cl_any_friend.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testSequenceCorrectedOneLinerKept() {
		buildSrc("CLASS cl_other_test_class DEFINITION FOR TESTING DURATION SHORT FINAL RISK LEVEL HARMLESS ABSTRACT.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_other_test_class DEFINITION ABSTRACT FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testSequenceCorrectedOneLinerSplit() {
		rule.configMaxLineLength.setValue(80);
		rule.configIndentStyle.setEnumValue(ClassDefIndentStyle.BELOW_DEFINITION);

		buildSrc("CLASS cl_other_test_class DEFINITION FOR TESTING DURATION SHORT FINAL RISK LEVEL HARMLESS ABSTRACT.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_other_test_class DEFINITION ABSTRACT FINAL");
		buildExp("                          FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("ENDCLASS.");

		testRule();
	}


	@Test
	void testOverlengthFriendList() {
		rule.configMaxLineLength.setValue(80);
		rule.configIndentStyle.setEnumValue(ClassDefIndentStyle.BELOW_DEFINITION);
		rule.configNewLineForFriendNames.setValue(false);

		buildSrc("CLASS cl_other_class_with_friend DEFINITION PUBLIC");
		buildSrc("  INHERITING FROM   cl_other_parent CREATE PRIVATE");
		buildSrc("  GLOBAL FRIENDS  cl_any_friend");
		buildSrc("          cl_other_friend  cl_third_friend ");
		buildSrc("   cl_fourth_friend  cl_fifth_friend.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_other_class_with_friend DEFINITION");
		buildExp("                                 PUBLIC");
		buildExp("                                 INHERITING FROM cl_other_parent");
		buildExp("                                 CREATE PRIVATE");
		buildExp("                                 GLOBAL FRIENDS cl_any_friend cl_other_friend");
		buildExp("                                                cl_third_friend cl_fourth_friend");
		buildExp("                                                cl_fifth_friend.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testIndentCorrectedTwoLinerKept() {
		rule.configIndentStyle.setEnumValue(ClassDefIndentStyle.BELOW_NAME);

		buildSrc("CLASS cl_any_class_with_friend DEFINITION");
		buildSrc("PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS cl_any_friend cl_other_friend.");
		buildSrc("  \" comment");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class_with_friend DEFINITION");
		buildExp("      PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS cl_any_friend cl_other_friend.");
		buildExp("  \" comment");
		buildExp("ENDCLASS.");

		testRule();
	}
	
	@Test
	void testSequenceCorrectedSpacesCondensedCommentKept() {
		buildSrc("CLASS  cl_any_test_class   DEFINITION   CREATE   PRIVATE");
		buildSrc("  ABSTRACT   FINAL  INHERITING  FROM  cl_any_test_base");
		buildSrc("  FOR");
		buildSrc("  TESTING  DURATION");
		buildSrc("  SHORT  PUBLIC  RISK    LEVEL");
		buildSrc("  HARMLESS. \"#EC INTF_IN_CLASS");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_test_class DEFINITION");
		buildExp("  PUBLIC");
		buildExp("  INHERITING FROM cl_any_test_base ABSTRACT FINAL");
		buildExp("  CREATE PRIVATE");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT. \"#EC INTF_IN_CLASS");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testCreateOneLinerWithOverlength() {
		rule.configMaxLineLength.setValue(90);
		rule.configIndentStyle.setEnumValue(ClassDefIndentStyle.PLUS_4);
		rule.configOneLinerAction.setEnumValue(ClassDefOneLinerAction.CREATE_ON_SAME_LINE);
		rule.configNewLineForPublic.setValue(false);
		rule.configNewLineForInheriting.setValue(false);
		rule.configNewLineForCreate.setValue(false);
		rule.configNewLineForSharedMemory.setValue(false);
		rule.configNewLineForTesting.setValue(false);
		rule.configNewLineForBehavior.setValue(false);
		rule.configNewLineForFriends.setValue(false);
		rule.configNewLineForFriendNames.setValue(false);

		buildSrc("CLASS cl_any_test_class DEFINITION");
		buildSrc("  FINAL INHERITING FROM cl_any_test_base");
		buildSrc("  FOR TESTING ##ANY_PRAGMA");
		buildSrc("  RISK LEVEL HARMLESS ##OTHER_PRAGMA");
		buildSrc("  DURATION SHORT.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_test_class DEFINITION INHERITING FROM cl_any_test_base FINAL FOR TESTING");
		buildExp("    RISK LEVEL HARMLESS DURATION SHORT ##ANY_PRAGMA ##OTHER_PRAGMA.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testEmptyLineAddedAbovePublicSection() {
		// ensure that for multi-line definitions, an empty line is added above PUBLIC SECTION
		buildSrc("CLASS cl_any_shared_memory_enabled DEFINITION");
		buildSrc("  PUBLIC FINAL CREATE   PUBLIC SHARED");
		buildSrc("  MEMORY ENABLED.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_shared_memory_enabled DEFINITION");
		buildExp("  PUBLIC FINAL");
		buildExp("  CREATE PUBLIC");
		buildExp("  SHARED MEMORY ENABLED.");
		buildExp("");
		buildExp("  PUBLIC SECTION.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testEmptyLinesKeptAbovePublicSection() {
		rule.configOneLinerAction.setEnumValue(ClassDefOneLinerAction.SPLIT);

		buildSrc("CLASS cl_other_class DEFINITION PUBLIC FINAL CREATE PUBLIC. \"#EC INTF_IN_CLASS");
		buildSrc("");
		buildSrc("");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_other_class DEFINITION");
		buildExp("  PUBLIC FINAL");
		buildExp("  CREATE PUBLIC. \"#EC INTF_IN_CLASS");
		buildExp("");
		buildExp("");
		buildExp("  PUBLIC SECTION.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testOneLinerCreatedNoEmptyLineAdded() {
		rule.configOneLinerAction.setEnumValue(ClassDefOneLinerAction.CREATE_ON_SAME_LINE);

		// ensure that after a one-liner CLASS ... DEFINITION, no empty line is added above PUBLIC SECTION
		buildSrc("CLASS cl_any_shared_memory_enabled DEFINITION");
		buildSrc("  PUBLIC FINAL.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_shared_memory_enabled DEFINITION PUBLIC FINAL.");
		buildExp("  PUBLIC SECTION.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testTwoLinerCreatedEmptyLineAdded() {
		rule.configOneLinerAction.setEnumValue(ClassDefOneLinerAction.CREATE_ON_NEXT_LINE);

		buildSrc("CLASS cl_any_test_class DEFINITION");
		buildSrc("  FINAL INHERITING FROM cl_any_test_base");
		buildSrc("  FOR TESTING");
		buildSrc("  RISK LEVEL HARMLESS");
		buildSrc("  DURATION SHORT.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_test_class DEFINITION");
		buildExp("  INHERITING FROM cl_any_test_base FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PUBLIC SECTION.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testLocalFriendsAligned() {
		buildSrc("CLASS cl_public_class DEFINITION LOCAL FRIENDS");
		buildSrc("    cl_any_local_class");
		buildSrc("    cl_other_local_class cl_third_local_class.");

		buildExp("CLASS cl_public_class DEFINITION LOCAL FRIENDS cl_any_local_class");
		buildExp("                                               cl_other_local_class");
		buildExp("                                               cl_third_local_class.");

		testRule();
	}

	@Test
	void testLocalFriendsAlignedBelow() {
		rule.configNewLineForLocalFriends.setValue(true);
		rule.configNewLineForFriendNames.setValue(false);
		
		buildSrc("CLASS cl_public_class DEFINITION LOCAL FRIENDS");
		buildSrc("    cl_any_local_class");
		buildSrc("    cl_other_local_class cl_third_local_class.");

		buildExp("CLASS cl_public_class DEFINITION");
		buildExp("  LOCAL FRIENDS cl_any_local_class cl_other_local_class cl_third_local_class.");

		testRule();
	}
}
