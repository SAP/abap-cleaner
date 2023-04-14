package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class SelfReferenceMeTest extends RuleTestBase {
	SelfReferenceMeTest() {
		super(RuleID.SELF_REFERENCE_ME);
	}
	
	private void buildStart() {
		// the following CLASS DEFINITION is used for all the tests: 
		buildSrc("CLASS any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS:");
		buildSrc("      any_method   IMPORTING !a TYPE i");
		buildSrc("                             !b TYPE string,");
		buildSrc("      other_method IMPORTING !num          TYPE i");
		buildSrc("                   EXPORTING !text         TYPE string");
		buildSrc("                   RETURNING VALUE(result) TYPE i.");
		buildSrc("    METHODS redefined REDEFINITION.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA num TYPE i.");
		buildSrc("    DATA prio TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS any_class IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD other_method.");

		copyExpFromSrc();
	}

	private void buildEnd() {
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");
}
	
	@Test
	void testMethodCallWithParams() {
		buildStart();
		buildSrc("    me->any_method( a = 5");
		buildSrc("                    b = `abc` ).");

		buildExp("    any_method( a = 5");
		buildExp("                b = `abc` ).");
		buildEnd();
		
		testRule();
	}

	@Test
	void testAttributeAccess() {
		buildStart();
		buildSrc("    result = me->prio.");

		buildExp("    result = prio.");
		buildEnd();

		testRule();
	}

	@Test
	void testMethodCallAsOperand() {
		buildStart();
		buildSrc("    result += me->get_count( ).");

		buildExp("    result += get_count( ).");
		buildEnd();

		testRule();
	}

	@Test
	void testExprInParameterAssignment() {
		buildStart();
		buildSrc("    ets_result = VALUE #( ( line_id = 1  price = me->mv_price_1 )");
		buildSrc("                          ( line_id = 2  price = me->create_price( iv_currency = me->get_currency( )");
		buildSrc("                                                                   iv_amount   = me->mv_amount ) ) ).");

		buildExp("    ets_result = VALUE #( ( line_id = 1  price = mv_price_1 )");
		buildExp("                          ( line_id = 2  price = create_price( iv_currency = get_currency( )");
		buildExp("                                                               iv_amount   = mv_amount ) ) ).");
		buildEnd();
		
		testRule();
	}

	@Test
	void testLocalVariableWithSameName() {
		buildStart();
		buildSrc("    DATA prio TYPE i.");
		buildSrc("    prio = 2.");
		buildSrc("    me->prio = 3.");
		buildEnd();
		
		copyExpFromSrc();
		
		testRule();
	}

	@Test
	void testKnownParameterWithSameName() {
		buildStart();
		buildSrc("    me->num = 1.");
		buildSrc("    me->text = `abc`.");
		buildSrc("    me->result = 42.");
		buildSrc("    me->num = ( me->result + num ) * result + ( result + me->num ) * me->result.");
		buildEnd();
		
		copyExpFromSrc();
		
		testRule();
	}

	@Test
	void testAssumedParameterWithSameName() {
		// expect the nothing to happen when the method signature is unknown
		
		// here, we do NOT use buildStart() and buildEnd()! 
		buildSrc("    me->num = num.");
		buildSrc("    me->num = num-component.");
		buildSrc("    me->num = num+4(2).");
		buildSrc("    me->num = num->get_next_num( ).");
		buildSrc("    me->num = any_method( text = |text with { num }| ).");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRedefinedMethod() {
		// as the signature of the redefined method is out of sight, expect me-> to remain unchanged, 
		// since 'num', 'text' and 'result' may be parameters
		
		buildStart();
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD redefined.");
		buildSrc("    me->num = 1.");
		buildSrc("    me->text = `abc`.");
		buildSrc("    me->result = 42.");
		buildEnd();
		
		copyExpFromSrc();

		testRule();
	}
}
