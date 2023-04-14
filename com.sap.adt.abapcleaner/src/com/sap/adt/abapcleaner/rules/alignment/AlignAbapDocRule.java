package com.sap.adt.abapcleaner.rules.alignment;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;

public class AlignAbapDocRule extends Rule {
		private final static RuleReference[] references = new RuleReference[] { 
				new RuleReference(RuleSource.ABAP_CLEANER) };

		@Override
		public RuleID getID() { return RuleID.ALIGN_ABAP_DOC; }

		@Override
		public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

		@Override
		public String getDisplayName() { return "Align ABAP Doc"; }

		@Override
		public String getDescription() { return "Aligns documentation of parameters and exceptions in ABAP Doc."; }

		@Override
		public LocalDate getDateCreated() { return LocalDate.of(2022, 5, 23); }

		@Override
		public RuleReference[] getReferences() { return references; }

		@Override
		public String getExample() {
			return  LINE_SEP + "CLASS cl_any_class DEFINITION FINAL." 
					+ LINE_SEP + "  PUBLIC SECTION."
					+ LINE_SEP + "    \"! <p class=\"shorttext synchronized\" lang=\"en\">Any method description</p>" 
					+ LINE_SEP + "    \"!" 
					+ LINE_SEP + "    \"! @parameter iv_any_param | <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>" 
					+ LINE_SEP + "    \"! @parameter iv_other_param | <p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>" 
					+ LINE_SEP + "    \"! @parameter ev_param_with_long_name | <p class=\"shorttext synchronized\" lang=\"en\">parameter with a long name</p>" 
					+ LINE_SEP + "    \"! @parameter et_any_table | <p class=\"shorttext synchronized\" lang=\"en\">any result table</p>" 
					+ LINE_SEP + "    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>" 
					+ LINE_SEP + "    METHODS any_method" 
					+ LINE_SEP + "      IMPORTING !iv_any_param            TYPE i" 
					+ LINE_SEP + "                !iv_other_param          TYPE string" 
					+ LINE_SEP + "      EXPORTING !ev_param_with_long_name TYPE i" 
					+ LINE_SEP + "                !et_any_table            TYPE ty_tt_table" 
					+ LINE_SEP + "      RAISING   !cx_any_exception." 
					+ LINE_SEP 
					+ LINE_SEP + "    \"! <p class=\"shorttext synchronized\" lang=\"en\">Other method description</p>" 
					+ LINE_SEP + "    \"!" 
					+ LINE_SEP + "    \"! @parameter iv_any_param | <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>" 
					+ LINE_SEP + "    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>" 
					+ LINE_SEP + "    \"!" 
					+ LINE_SEP + "    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>" 
					+ LINE_SEP + "    METHODS other_method" 
					+ LINE_SEP + "      IMPORTING !iv_any_param   TYPE i" 
					+ LINE_SEP + "                !iv_other_param TYPE string" 
					+ LINE_SEP + "      RAISING   !cx_any_exception." 
					+ LINE_SEP 
					+ LINE_SEP + "    \"! <p class=\"shorttext synchronized\" lang=\"en\">Third method description</p>" 
					+ LINE_SEP + "    \"!" 
					+ LINE_SEP + "    \"! @parameter iv_any_param | <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>" 
					+ LINE_SEP + "    \"! Further description of any parameter. This text is not synchronized, but it is displayed in ADT." 
					+ LINE_SEP + "    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>" 
					+ LINE_SEP + "    \"! Further description of other parameter." 
					+ LINE_SEP + "    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>" 
					+ LINE_SEP + "    \"! Detailed description of the exception." 
					+ LINE_SEP + "    METHODS third_method" 
					+ LINE_SEP + "      IMPORTING !iv_any_param   TYPE i" 
					+ LINE_SEP + "                !iv_other_param TYPE string" 
					+ LINE_SEP + "      RAISING   !cx_any_exception." 
					+ LINE_SEP + "ENDCLASS.";
		}

		final ConfigBoolValue configAlignAcrossEmptyLines = new ConfigBoolValue(this, "AlignAcrossEmptyLines", "Align across empty ABAP Doc lines", true);
		final ConfigBoolValue configAlignAcrossNonEmptyLines = new ConfigBoolValue(this, "AlignAcrossNonEmptyLines", "Align across non-empty ABAP Doc lines", true);

		private final ConfigValue[] configValues = new ConfigBoolValue[] { configAlignAcrossEmptyLines, configAlignAcrossNonEmptyLines };

		@Override
		public ConfigValue[] getConfigValues() { return configValues; }

		public AlignAbapDocRule(Profile profile) {
			super(profile);
			initializeConfiguration();
		}

		@Override
		public void executeOn(Code code, int releaseRestriction) {
			if (code == null)
				throw new NullPointerException("code");

			boolean alignAcrossEmptyLines = configAlignAcrossEmptyLines.getValue();
			boolean alignAcrossNonEmptyLines = configAlignAcrossNonEmptyLines.getValue();
			
			Command command = code.firstCommand;
			while (command != null) {
				commandForErrorMsg = command;

				if (!isParameterDocLine(command)) {
					command = command.getNext();
					continue;
				}

				// find the end of the section
				int lineCount = 1;
				Command endCommand = command;
				do {
					endCommand = endCommand.getNext();
					if (endCommand == null || !isAbapDocLine(endCommand))
						break;
					if (isParameterDocLine(endCommand)) {
						++lineCount;
						continue;
					}
					if (endCommand.getFirstToken().textEquals(ABAP.ABAP_DOC_SIGN) ? !alignAcrossEmptyLines : !alignAcrossNonEmptyLines) 
						break;
				} while (true);
				
				// only align if the whole parameter interface was covered (i.e. if endCommand now contains the declaration 
				// which the ABAP Doc is about)
				if (lineCount > 1 && (endCommand == null || !endCommand.isQuotMarkCommentLine())) {
					alignSection(code, command, endCommand);
				}
				
				command = endCommand;
			}
		}

		private boolean isAbapDocLine(Command command) {
			return command.isQuotMarkCommentLine() && command.getFirstToken().getText().startsWith(ABAP.ABAP_DOC_SIGN);
		}
		
		private boolean isParameterDocLine(Command command) {
			if (!command.isQuotMarkCommentLine())
				return false;
			
			// only align ABAP Doc comments
			String text = command.getFirstToken().getText();
			if (!text.startsWith(ABAP.ABAP_DOC_SIGN))
				return false;
			
			// only align if the line contains a pipe |, and documentation follows the | in the same comment line
			int pipeIndex = text.indexOf(ABAP.ABAP_DOC_SEP);
			if (pipeIndex < 0 || pipeIndex == text.length() - 1)
				return false;

			// only align if between "! and |, exactly two words are found: the keyword and the parameter / exception name
			String[] textParts = StringUtil.split(text.substring(ABAP.ABAP_DOC_SIGN.length(), pipeIndex), ' ', true);
			if (textParts == null || textParts.length != 2) 
				return false;

			// only align expected keywords 
			if (    !textParts[0].equalsIgnoreCase("@parameter")
			     && !textParts[0].equalsIgnoreCase("@raising")
			     && !textParts[0].equalsIgnoreCase("@exception")) {
				return false;
			}
			// textParts[2] is the parameter or exception name
			
			return true;
		}
		
		private void alignSection(Code code, Command startCommand, Command endCommand) {
			// determine maximum keyword and name lengths
			int maxKeywordLength = 0;
			int maxNameLength = 0;
			Command command = startCommand;
			while (command != endCommand) {
				if (isParameterDocLine(command)) {
					String text = command.getFirstToken().getText();
					int pipeIndex = text.indexOf(ABAP.ABAP_DOC_SEP);
					String[] textParts = StringUtil.split(text.substring(ABAP.ABAP_DOC_SIGN.length(), pipeIndex), ' ', true);
					// since isParameterDocLine() is true, we can be sure that textParts has two elements: 
					maxKeywordLength = Math.max(maxKeywordLength, textParts[0].length());
					maxNameLength = Math.max(maxNameLength, textParts[1].length());
				}
				command = command.getNext();
			}

			// change ABAP Doc lines
			Command changeCommand = startCommand;
			while (changeCommand != endCommand) {
				if (isParameterDocLine(changeCommand)) {
					commandForErrorMsg = changeCommand;
	
					String text = changeCommand.getFirstToken().getText();
					int pipeIndex = text.indexOf(ABAP.ABAP_DOC_SEP);
					String[] textParts = StringUtil.split(text.substring(ABAP.ABAP_DOC_SIGN.length(), pipeIndex), ' ', true);
					
					String keyword = textParts[0]; // @parameter, @raising, or @exception
					String name = textParts[1];    // the parameter or (class-based / classic) exception name
					String docu = text.substring(pipeIndex + 1).trim(); // the documentation
	
					// enhance keyword and name with spaces (including the subsequent separator space)
					keyword += StringUtil.repeatChar(' ', maxKeywordLength + 1 - keyword.length());
					name += StringUtil.repeatChar(' ', maxNameLength + 1 - name.length());
					
					String changedText = ABAP.ABAP_DOC_SIGN + " " + keyword + name + ABAP.ABAP_DOC_SEP_STRING + " " + docu;
					if (!isCommandBlocked(changeCommand) && !text.equals(changedText)) {
						changeCommand.getFirstToken().setText(changedText, false);
						code.addRuleUse(this, changeCommand);
					}
				}				
				changeCommand = changeCommand.getNext();
			}
		}
	}
