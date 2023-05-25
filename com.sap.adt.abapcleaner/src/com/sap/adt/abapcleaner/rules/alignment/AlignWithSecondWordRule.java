package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class AlignWithSecondWordRule extends Rule {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ALIGN_WITH_SECOND_WORD; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align keywords with second word of first line"; }

	@Override
	public String getDescription() { return "Aligns ABAP keywords with the second word of the first line (e.g. in READ TABLE ... WITH KEY ..., INSERT ... INTO TABLE ...)"; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 21); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD align_with_second_word." 
			+ LINE_SEP + "    READ TABLE lth_any_hash_table ASSIGNING <ls_row>" 
			+ LINE_SEP + "      WITH TABLE KEY item_id = <ls_any_field_symbol>-item_id." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" if keywords in subsequent lines are just indented by 2 spaces, it is very difficult to see" 
			+ LINE_SEP + "    \" where a block starts and ends" 
			+ LINE_SEP + "    LOOP AT mo_item_manager->get_all_items( )" 
			+ LINE_SEP + "      ASSIGNING FIELD-SYMBOL(<lo_item>)." 
			+ LINE_SEP + "      INSERT VALUE #( item_id       = <lo_item>->ms_data-item_id" 
			+ LINE_SEP + "                      item_category = if_any_interface=>cos_item_type-empty )" 
			+ LINE_SEP + "        INTO TABLE lth_any_hash_table." 
			+ LINE_SEP + "    ENDLOOP." 
			+ LINE_SEP 
			+ LINE_SEP + "    LOOP AT lts_any_sorted_table ASSIGNING FIELD-SYMBOL(<ls_row>)" 
			+ LINE_SEP + "    WHERE event_date = '20220330'." 
			+ LINE_SEP + "      \" do something" 
			+ LINE_SEP + "    ENDLOOP." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" only keywords like 'WITH' and 'INTO' will be aligned, not identifiers like 'deferral_cat'" 
			+ LINE_SEP + "    READ TABLE lts_any_sorted_table" 
			+ LINE_SEP + "    WITH TABLE KEY item_type     = if_any_interface=>co_any_item_type" 
			+ LINE_SEP + "                   item_category = if_any_interface=>co_any_item_category" 
			+ LINE_SEP + "    INTO DATA(ls_struc)." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" this rule does NOT change the position of Boolean operators AND, OR, EQUIV" 
			+ LINE_SEP + "    DELETE lts_any_sorted_table" 
			+ LINE_SEP + "    WHERE item_key  < '20220030000101'" 
			+ LINE_SEP + "      AND item_type = 'ABCD'." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	public AlignWithSecondWordRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) {
		if (code == null)
			throw new NullPointerException("code");

		boolean isInMethod = !code.hasMethodFunctionFormOrEventBlockStart();

		Command command = code.firstCommand;
		while (command != null) {
			if (command.isMethodFunctionFormOrEventBlockEnd())
				isInMethod = false;
			// do NOT attach with 'else if':
			if (command.isMethodFunctionFormOrEventBlockStart())
				isInMethod = true;

			commandForErrorMsg = command;
			if (isCommandBlocked(command)) {
				command = command.getNext();
				continue;
			}

			// certain types of commands are not applicable (e.g. declarations, assignments, METHOD, CLASS, IF, ELSEIF, WHILE, ...)
			Token firstToken = command.getFirstToken();
			boolean skip = !isInMethod || command.isDeclaration() || command.isAssignment() || command.isAbapSqlOperation() || command.isInClassDefinition()
					|| command.isDeclarationInClassDef() || (command.getOpensLevel() && !firstToken.isKeyword("LOOP")) 
					|| firstToken.matchesOnSiblings(true, "CALL", "METHOD|FUNCTION|BADI")
					|| firstToken.matchesOnSiblings(true, "CREATE", "OBJECT")
					|| firstToken.getNextSibling() != firstToken.getNext();
			// TODO: or would it be better to use an include list?

			if (!skip && firstToken.isKeyword() && firstToken.getNextSibling() != null && firstToken.getNextSibling().lineBreaks == 0) {
				Token token = firstToken.getNextSibling();
				int indent = token.getStartIndexInLine();

				boolean changed = false;
				while (token != null) {
					if (token.isFirstTokenInLine() && token.spacesLeft != indent && token.isKeyword() && !token.isAnyKeyword("AND", "OR", "EQUIV")) {
						command.addIndent(indent - token.spacesLeft, token.spacesLeft, token);
						changed = true;
					}
					token = token.getNextSibling();
				}
				if (changed)
					code.addRuleUse(this, command);

			}
			command = command.getNext();
		}
	}
}