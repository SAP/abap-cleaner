package com.sap.adt.abapcleaner.rules.declarations;
 
import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDeclarations;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.Variables;
import com.sap.adt.abapcleaner.rulehelpers.VariableInfo;

public class FinalVariableRule extends RuleForDeclarations {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "FINAL, Inline Declaration for Immutable Variables", "abenfinal_inline.htm"),
			new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.FINAL_VARIABLE; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Use FINAL for immutable variables"; }

	@Override
	public String getDescription() { return "Replaces DATA() inline declarations with FINAL() if no other write access to the respective variable is found."; }

	@Override
	public String getHintsAndRestrictions() { return "This rule requires a NetWeaver version >= 7.57. Note that this rule will skip methods in which macros are used."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 3, 4); }
 
	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
	public int getRequiredAbapRelease() {
		return ABAP.REQUIRED_RELEASE_757; 
	}  

	@Override
	public boolean isActiveByDefault() { return false; }
	
	@Override
   public String getExample() {
      return "" 
 			+ LINE_SEP + "CLASS any_class IMPLEMENTATION." 
			+ LINE_SEP + "  METHOD use_final_for_immutable_vars." 
			+ LINE_SEP + "    DATA(lo_utility) = cl_any_factory=>get( )->get_any_utility( )." 
			+ LINE_SEP + "    DATA(lv_date) = lo_utility->get_any_date( )." 
			+ LINE_SEP + ""
			+ LINE_SEP + "    SELECT carrid, connid, seatsocc"
			+ LINE_SEP + "           FROM sflight"
			+ LINE_SEP + "           WHERE fldate = @lv_date"
			+ LINE_SEP + "           INTO TABLE @DATA(lt_flight)."
			+ LINE_SEP + ""
			+ LINE_SEP + "    DATA(lv_sum) = 0." 
			+ LINE_SEP + "    LOOP AT lt_flight INTO DATA(ls_flight) ##INTO_OK."
			+ LINE_SEP + "      lv_sum += ls_flight-seatsocc."
			+ LINE_SEP + "    ENDLOOP."
			+ LINE_SEP + ""
			+ LINE_SEP + "    GET TIME FIELD DATA(lv_time)."
			+ LINE_SEP + "    rv_result = |{ lv_date } { lv_time }: { lv_sum }|." 
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" if a variable is changed via field-symbol, FINAL would cause a runtime error:"
			+ LINE_SEP + "    DATA(lt_connection) = get_connections( lt_flight )." 
			+ LINE_SEP + "    LOOP AT lt_connection ASSIGNING FIELD-SYMBOL(<ls_connection>)."
			+ LINE_SEP + "      <ls_connection>-used = abap_true."
			+ LINE_SEP + "    ENDLOOP."
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" similarly, FINAL is never introduced if a data reference is created somewhere:"
			+ LINE_SEP + "    DATA(lt_any_table) = get_table( )." 
			+ LINE_SEP + "    any_method( ir_struc = REF #( lt_any_table[ 1 ] ) )."
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" FINAL is not introduced if method calls are found inside of constructor expressions,"
			+ LINE_SEP + "    \" because some of these cases would create syntax errors with FINAL:"
			+ LINE_SEP + "    DATA(ls_struc) = VALUE ty_s_any_struc( comp1 = get_any_value( )" 
			+ LINE_SEP + "                                           comp2 = 'literal' )."
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + "ENDCLASS."; 
   }

	public FinalVariableRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected void executeOn(Code code, Command methodStart, Variables localVariables, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// skip this method if macros or dynamic ASSIGN are used inside the method to avoid making variables FINAL that 
		// are used inside the macros or dynamically (note that macro code may be local or 'out of sight')
		if (localVariables.isEmpty() || localVariables.getMethodUsesMacrosOrTestInjection() || localVariables.getMethodUsesDynamicAssign())
			return;
		
		for (VariableInfo varInfo : localVariables.getLocalsInDeclarationOrder()) {
			if (varInfo.isParameter() || !varInfo.isDeclaredInline || varInfo.isAssignedAfterDeclaration())
				continue;
			Token identifier = varInfo.declarationToken;
			if (identifier == null || !identifier.isAttached())
				continue;
			Token dataKeyword = identifier.getParent(); 
			if (dataKeyword == null || !dataKeyword.isAnyKeyword("DATA(", "@DATA("))
				continue;
			
			// if there may be an indirect write (via data reference or field-symbol) on the memory area of this variable, 
			// we keep the declaration with DATA(), rather than using FINAL() and risking a runtime error MOVE_TO_LIT_NOT_ALLOWED_NODATA
			if (varInfo.mayHaveIndirectWrites())
				continue;
			
			Command command = identifier.getParentCommand();
			if (isCommandBlocked(command))
				continue;
			
			Token firstCode = command.getFirstCodeToken();
			if (firstCode != null && firstCode.matchesOnSiblings(true, "OPEN", "CURSOR")) {
				// skip this Command
			} else if (firstCode != null && firstCode.isKeyword("SELECT") && dataKeyword.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "FROM|WHERE|GROUP BY|HAVING|ORDER BY|%_HINTS|UNION|INTERSECT|EXCEPT") != null) {
				// do NOT introduce @FINAL if the INTO clause is followed by another (mainquery) clause, because that could 
				// lead to a syntax error; cp. documentation on ABAP SQL strict modes 
				// https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_sql_strict_modes.htm
			} else if (command.isAssignment(true, false) && command.containsMethodCallInsideConstructorExp()) {
				// some cases of method calls inside of a constructor expression produce a syntax error if FINAL is used, e.g.
				// FINAL(test_data) = VALUE ty_s_any( id = get_any_integer( ) name = 'Test' ).
				// FINAL(lv_value) = COND i( WHEN lv_condition = abap_true THEN get_any_integer( ) ELSE get_any_integer( ) ).
			} else {
				String newText = dataKeyword.isKeyword("@DATA(") ? "@FINAL(" : "FINAL(";
				dataKeyword.setText(newText, true);
				code.addRuleUse(this, command);
			}
		}
	}
}
