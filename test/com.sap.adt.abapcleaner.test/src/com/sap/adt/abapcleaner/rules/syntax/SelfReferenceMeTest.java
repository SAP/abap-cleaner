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

	@Test
	void testLocalInterfaceMethodConsidered() {
		// expect the signature of method LIF_ANY_INTERFACE~ANY_METHOD to be correctly identified, 
		// meaning that ME can safely be removed from ME->MV_VALUE, but NOT from ME->RESULT
		
		buildSrc("INTERFACE lif_any_interface.");
		buildSrc("  METHODS any_method");
		buildSrc("    EXPORTING result TYPE i.");
		buildSrc("ENDINTERFACE.");
		buildSrc("");
		buildSrc("CLASS lcl_test DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES lif_any_interface.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA mv_value TYPE i.");
		buildSrc("    DATA result TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS lcl_test IMPLEMENTATION.");
		buildSrc("  METHOD lif_any_interface~any_method.");
		buildSrc("    me->mv_value = 1.");
		buildSrc("    me->result = 2.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("INTERFACE lif_any_interface.");
		buildExp("  METHODS any_method");
		buildExp("    EXPORTING result TYPE i.");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("CLASS lcl_test DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES lif_any_interface.");
		buildExp("  PRIVATE SECTION.");
		buildExp("    DATA mv_value TYPE i.");
		buildExp("    DATA result TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS lcl_test IMPLEMENTATION.");
		buildExp("  METHOD lif_any_interface~any_method.");
		buildExp("    mv_value = 1.");
		buildExp("    me->result = 2.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testLocalInterfacesWithCascadingAliases() {
		// expect the cascading ALIASES to be correctly interpreted, meaning that in method M1, "ME->V1" must remain,  
		// in method M2, "ME->V2" must remain, and in method M3, "ME->V3" must remain, while otherwise, ME-> can be removed
		
		buildSrc("INTERFACE i1.");
		buildSrc("  METHODS meth EXPORTING v1 TYPE i.");
		buildSrc("ENDINTERFACE.");
		buildSrc("");
		buildSrc("INTERFACE i2.");
		buildSrc("  INTERFACES i1.");
		buildSrc("  ALIASES m1 FOR i1~meth.");
		buildSrc("  METHODS meth EXPORTING v2 TYPE i.");
		buildSrc("ENDINTERFACE.");
		buildSrc("");
		buildSrc("INTERFACE i3.");
		buildSrc("  INTERFACES i2.");
		buildSrc("  ALIASES: m1 FOR i2~m1,");
		buildSrc("           m2 FOR i2~meth.");
		buildSrc("");
		buildSrc("  METHODS meth EXPORTING v3 TYPE i.");
		buildSrc("ENDINTERFACE.");
		buildSrc("");
		buildSrc("CLASS c1 DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES i3.");
		buildSrc("    ALIASES: m1 FOR i3~m1,");
		buildSrc("             m2 FOR i3~m2,");
		buildSrc("             m3 FOR i3~meth.");
		buildSrc("");
		buildSrc("    DATA: v1 TYPE i,");
		buildSrc("          v2 TYPE i,");
		buildSrc("          v3 TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS c1 IMPLEMENTATION.");
		buildSrc("  METHOD m1.");
		buildSrc("    me->v1 = 1.");
		buildSrc("    me->v2 = 2.");
		buildSrc("    me->v3 = 3.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD m2.");
		buildSrc("    me->v1 = 1.");
		buildSrc("    me->v2 = 2.");
		buildSrc("    me->v3 = 3.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD m3.");
		buildSrc("    me->v1 = 1.");
		buildSrc("    me->v2 = 2.");
		buildSrc("    me->v3 = 3.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("INTERFACE i1.");
		buildExp("  METHODS meth EXPORTING v1 TYPE i.");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("INTERFACE i2.");
		buildExp("  INTERFACES i1.");
		buildExp("  ALIASES m1 FOR i1~meth.");
		buildExp("  METHODS meth EXPORTING v2 TYPE i.");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("INTERFACE i3.");
		buildExp("  INTERFACES i2.");
		buildExp("  ALIASES: m1 FOR i2~m1,");
		buildExp("           m2 FOR i2~meth.");
		buildExp("");
		buildExp("  METHODS meth EXPORTING v3 TYPE i.");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("CLASS c1 DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES i3.");
		buildExp("    ALIASES: m1 FOR i3~m1,");
		buildExp("             m2 FOR i3~m2,");
		buildExp("             m3 FOR i3~meth.");
		buildExp("");
		buildExp("    DATA: v1 TYPE i,");
		buildExp("          v2 TYPE i,");
		buildExp("          v3 TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS c1 IMPLEMENTATION.");
		buildExp("  METHOD m1.");
		buildExp("    me->v1 = 1.");
		buildExp("    v2 = 2.");
		buildExp("    v3 = 3.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD m2.");
		buildExp("    v1 = 1.");
		buildExp("    me->v2 = 2.");
		buildExp("    v3 = 3.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD m3.");
		buildExp("    v1 = 1.");
		buildExp("    v2 = 2.");
		buildExp("    me->v3 = 3.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testGlobalAndRecursiveLocalInterfaces() {
		// expect that the method signatures from the recursively implemented local interfaces are correctly identified
		// and ME-> is removed where possible, while nothing is changed in the global interface method, because its 
		// signature is 'out of sight'

		buildSrc("INTERFACE i1.");
		buildSrc("  METHODS meth EXPORTING v1 TYPE i.");
		buildSrc("ENDINTERFACE.");
		buildSrc("");
		buildSrc("INTERFACE i2.");
		buildSrc("  INTERFACES i1.");
		buildSrc("  METHODS meth EXPORTING v2 TYPE i.");
		buildSrc("ENDINTERFACE.");
		buildSrc("");
		buildSrc("INTERFACE i3.");
		buildSrc("  INTERFACES i2.");
		buildSrc("  METHODS meth EXPORTING v3 TYPE i.");
		buildSrc("ENDINTERFACE.");
		buildSrc("");
		buildSrc("CLASS c1 DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES: i3,");
		buildSrc("                if_global.");
		buildSrc("");
		buildSrc("    DATA: v1 TYPE i,");
		buildSrc("          v2 TYPE i,");
		buildSrc("          v3 TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS c1 IMPLEMENTATION.");
		buildSrc("  METHOD i1~meth.");
		buildSrc("    me->v1 = 1.");
		buildSrc("    me->v2 = 2.");
		buildSrc("    me->v3 = 3.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD i2~meth.");
		buildSrc("    me->v1 = 1.");
		buildSrc("    me->v2 = 2.");
		buildSrc("    me->v3 = 3.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD i3~meth.");
		buildSrc("    me->v1 = 1.");
		buildSrc("    me->v2 = 2.");
		buildSrc("    me->v3 = 3.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD if_global~meth.");
		buildSrc("    me->v1 = 1.");
		buildSrc("    me->v2 = 2.");
		buildSrc("    me->v3 = 3.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("INTERFACE i1.");
		buildExp("  METHODS meth EXPORTING v1 TYPE i.");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("INTERFACE i2.");
		buildExp("  INTERFACES i1.");
		buildExp("  METHODS meth EXPORTING v2 TYPE i.");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("INTERFACE i3.");
		buildExp("  INTERFACES i2.");
		buildExp("  METHODS meth EXPORTING v3 TYPE i.");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("CLASS c1 DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES: i3,");
		buildExp("                if_global.");
		buildExp("");
		buildExp("    DATA: v1 TYPE i,");
		buildExp("          v2 TYPE i,");
		buildExp("          v3 TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS c1 IMPLEMENTATION.");
		buildExp("  METHOD i1~meth.");
		buildExp("    me->v1 = 1.");
		buildExp("    v2 = 2.");
		buildExp("    v3 = 3.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD i2~meth.");
		buildExp("    v1 = 1.");
		buildExp("    me->v2 = 2.");
		buildExp("    v3 = 3.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD i3~meth.");
		buildExp("    v1 = 1.");
		buildExp("    v2 = 2.");
		buildExp("    me->v3 = 3.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD if_global~meth.");
		buildExp("    me->v1 = 1.");
		buildExp("    me->v2 = 2.");
		buildExp("    me->v3 = 3.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}


	@Test
	void testEndlessLoopAvoided() {
		// ensure an endless loop is avoided despite the syntax error of interface i3 implementing itself
		
		buildSrc("INTERFACE i1.");
		buildSrc("  METHODS meth EXPORTING v1 TYPE i.");
		buildSrc("ENDINTERFACE.");
		buildSrc("");
		buildSrc("INTERFACE i2.");
		buildSrc("  INTERFACES i1.");
		buildSrc("  METHODS meth EXPORTING v2 TYPE i.");
		buildSrc("ENDINTERFACE.");
		buildSrc("");
		buildSrc("INTERFACE i3.");
		buildSrc("  \" syntax error: i3 cannot implement itself");
		buildSrc("  INTERFACES: i3, i2.");
		buildSrc("  METHODS meth EXPORTING v3 TYPE i.");
		buildSrc("ENDINTERFACE.");
		buildSrc("");
		buildSrc("CLASS c1 DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES i3.");
		buildSrc("");
		buildSrc("    DATA: v1 TYPE i,");
		buildSrc("          v2 TYPE i,");
		buildSrc("          v3 TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS c1 IMPLEMENTATION.");
		buildSrc("  METHOD i1~meth.");
		buildSrc("    me->v1 = 1.");
		buildSrc("    me->v2 = 2.");
		buildSrc("    me->v3 = 3.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("INTERFACE i1.");
		buildExp("  METHODS meth EXPORTING v1 TYPE i.");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("INTERFACE i2.");
		buildExp("  INTERFACES i1.");
		buildExp("  METHODS meth EXPORTING v2 TYPE i.");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("INTERFACE i3.");
		buildExp("  \" syntax error: i3 cannot implement itself");
		buildExp("  INTERFACES: i3, i2.");
		buildExp("  METHODS meth EXPORTING v3 TYPE i.");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("CLASS c1 DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES i3.");
		buildExp("");
		buildExp("    DATA: v1 TYPE i,");
		buildExp("          v2 TYPE i,");
		buildExp("          v3 TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS c1 IMPLEMENTATION.");
		buildExp("  METHOD i1~meth.");
		buildExp("    me->v1 = 1.");
		buildExp("    v2 = 2.");
		buildExp("    v3 = 3.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

}