package com.sap.adt.abapcleaner.rulebase;

import java.util.HashMap;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;

/**
 * base class for a Rule that requires information on method parameter and/or local variable definitions and usages
 */
public abstract class RuleForDeclarations extends Rule {
	/** true if the Rule is to be executed only on class definitions, not on local contexts (i.e. implementations of METHOD, FORM etc.) */
	protected boolean skipLocalVariableContexts() { return false; }

	protected void executeOnClassDefinition(Code code, ClassInfo classOrInterfaceInfo, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// default implementation is empty
	}

	protected void executeOnClassImplementation(Code code, ClassInfo classOrInterfaceInfo, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// default implementation is empty
	}

	protected void executeOn(Code code, Command methodStart, Variables localVariables, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// default implementation is empty
	}

   private final static String[] declarationKeywords = new String[] {"TYPES", "CONSTANTS", "DATA", "FIELD-SYMBOLS", "STATICS"}; // "DATA(", "FINAL(" and "FIELD-SYMBOL(" do NOT belong here!
   private final static String[] typesDeclarationKeywords = new String[] {"TYPES"};
   private final static String[] constantsDeclarationKeywords = new String[] {"CONSTANTS"};

   private static class BlockInfo {
   	private int blockLevel = 0;
   	private VariableInfo topBlockVarInfo = null;
   	
   	private void beginBlock() {
   		++blockLevel;
   	}
   	
   	private void endBlock() {
   		--blockLevel;
   		if (blockLevel == 0) {
   			topBlockVarInfo = null;
   		}
   	}

   	private void setTopBlockVarInfo(VariableInfo topBlockVarInfo) {
   		this.topBlockVarInfo = topBlockVarInfo;
   	}
	}

   protected static String getNameKey(String name) {
      return AbapCult.toUpper(name);
   }

	protected RuleForDeclarations(Profile profile) {
		super(profile);
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (code == null)
			throw new NullPointerException("code");

		CommentIdentifier commentIdentifier = new CommentIdentifier();

		HashMap<String, ClassInfo> classesAndInterfaces = new HashMap<String, ClassInfo>();
		ClassInfo curClassOrInterface = null;

		Variables variables = new Variables(this, null, null); // pro forma
		
		Command command = code.firstCommand;
		Command methodStart = null;
		
		// do NOT initialize isInMethod  with "= (!code.hasMethodFunctionOrFormStart())", because if an incomplete part 
		// of the class declaration is processed, this will delete attributes!
		boolean isInDefinition = false;
		MethodVisibility methodVisibility = MethodVisibility.PUBLIC;
		VariableAccessType variableAccessType = VariableAccessType.PUBLIC;
		boolean isInMethod = false; 
		boolean skipMethod = skipLocalVariableContexts();
		BlockInfo blockInfo = new BlockInfo();

		while (command != null) {
			// determine the current class or interface to add to its method definitions, 
			// or later (in a class implementation section) read from them
			if (command.isClassDefinitionStart() || command.isInterfaceStart()) {
				isInDefinition = true;
				Token parentClassName = command.getFirstCodeToken().getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "INHERITING", "FROM", TokenSearch.ANY_IDENTIFIER);
				ClassInfo parentClass = (parentClassName == null) ? null : classesAndInterfaces.get(getNameKey(parentClassName.getText()));
				curClassOrInterface = new ClassInfo(command, command.getDefinedName(), parentClass);
				classesAndInterfaces.put(getNameKey(curClassOrInterface.name), curClassOrInterface);

				methodVisibility = MethodVisibility.PUBLIC;
				variableAccessType = VariableAccessType.PUBLIC;
				variables = new Variables(this, curClassOrInterface, null);
				curClassOrInterface.setVariables(variables);
				
			} else if (command.isClassImplementationStart()) {
				curClassOrInterface = classesAndInterfaces.get(getNameKey(command.getDefinedName()));
				if (curClassOrInterface != null) 
					curClassOrInterface.setImplementationStart(command);
				executeOnClassImplementation(code, curClassOrInterface, releaseRestriction);

			} else if (command.isClassEnd() || command.isInterfaceEnd()) {
				if (isInDefinition)
					executeOnClassDefinition(code, curClassOrInterface, releaseRestriction);
				isInDefinition = false;
				curClassOrInterface = null;
			}
			
			if (isInDefinition) {
				// determine method visibility section
				Token firstCode = command.getFirstCodeToken();
				if (firstCode == null) {
					// nothing to do
				} else if (firstCode.matchesOnSiblings(true, "PUBLIC", "SECTION")) {
					methodVisibility = MethodVisibility.PUBLIC;
					variableAccessType = VariableAccessType.PUBLIC;
					
				} else if (firstCode.matchesOnSiblings(true, "PROTECTED", "SECTION")) {
					methodVisibility = MethodVisibility.PROTECTED;
					variableAccessType = VariableAccessType.PROTECTED;
				
				} else if (firstCode.matchesOnSiblings(true, "PRIVATE", "SECTION")) {
					methodVisibility = MethodVisibility.PRIVATE;
					variableAccessType = VariableAccessType.PRIVATE;
				
				} else if (firstCode.isAnyKeyword("METHODS", "CLASS-METHODS")) {
					// add one or several method definitions
					addMethodDefinitions(curClassOrInterface, methodVisibility, command);
				}
				
				// process interface declarations, esp. for local interfaces declared in the same code document
				if (command.firstCodeTokenIsKeyword("INTERFACES")) {
					addInterfaces(curClassOrInterface, command, classesAndInterfaces);
				}
				// process alias declarations
				if (command.firstCodeTokenIsKeyword("ALIASES")) {
					addAliases(curClassOrInterface, command, classesAndInterfaces);
				}
			}
			
			if (command.endsLocalVariableContext()) {
				if (!skipMethod)
					callExecuteOnMethod(code, methodStart, variables, releaseRestriction);
				methodStart = null;
				isInMethod = false;
				variables = new Variables(this, curClassOrInterface, null);
			}
			// do NOT attach the next section with "else if", since "AT SELECTION-SCREEN" may both end and start   
			// a "local variable context" at the same time
			if (command.startsLocalVariableContext()) {
				if (!skipMethod)
					callExecuteOnMethod(code, methodStart, variables, releaseRestriction);
				methodStart = command;
				isInMethod = true;
				MethodInfo curMethod = null;
				if (curClassOrInterface != null) {
					curMethod = curClassOrInterface.getMethod(command.getDefinedName()); // may be null
				}
				// reset skipMethod to false, unless methods are generally skipped by this Rule
				skipMethod = skipLocalVariableContexts();
				variables = new Variables(this, curClassOrInterface, curMethod);
			}
			
			if (isInMethod && command.firstCodeTokenIsAnyKeyword("TEST-SEAM", "TEST-INJECTION")) {
				// if test seams / test injections are used in this method, rules like the UnusedVariablesRule and 
				// FinalVariableRule must skip this method, because variable definitions and/or usages may be out of sight
				variables.setMethodUsesMacrosOrTestInjection();
				// skip this section to avoid variable definitions from these sections to be moved by the LocalDeclarationOrderRule
				command = command.getNextSibling();
				continue;

			} else if (isInMethod && command.firstCodeTokenIsKeyword("ASSIGN")) {
				// detect dynamic ASSIGN, e.g. 'ASSIGN ('lv_any') TO FIELD-SYMBOL(<...>).' with which we cannot be sure 
				// whether local variables or parameters are used, so FinalVariableRule, UnusedParameterRule, and UnusedVariablesRule
				// must be skipped for this method 
				Token next = command.getFirstCodeToken().getNextCodeToken();
				if (next.textEquals("(") && next.hasChildren() && next.getFirstChild().isAttached()) {
					variables.setMethodUsesDynamicAssign();
				}

			} else if (command.firstCodeTokenIsKeyword("DEFINE")) {
				// skip macro definitions (i.e. DEFINE ... END-OF-DEFINITION sections), 
				// esp. to avoid variable definitions from these sections to be moved by the LocalDeclarationOrderRule
				command = command.getNextSibling();
				continue;
			} 

			commandForErrorMsg = command;
			
			try {
				if (!isInMethod) {
					// read attributes
					if (command.firstCodeTokenIsAnyKeyword(declarationKeywords)) {
						boolean isTypeDeclaration = command.firstCodeTokenIsAnyKeyword(typesDeclarationKeywords);
						boolean isConstantsDeclaration = command.firstCodeTokenIsAnyKeyword(constantsDeclarationKeywords);
						executeOnDeclarationCommand(command, null, variables, isTypeDeclaration, isConstantsDeclaration, blockInfo, variableAccessType);
					}				

				} else if (isInMethod && !skipMethod) {
					// read local variable declarations or usage from the current Command
					if (command.firstCodeTokenIsAnyKeyword(declarationKeywords)) {
						boolean isTypeDeclaration = command.firstCodeTokenIsAnyKeyword(typesDeclarationKeywords);
						boolean isConstantsDeclaration = command.firstCodeTokenIsAnyKeyword(constantsDeclarationKeywords);
						executeOnDeclarationCommand(command, methodStart, variables, isTypeDeclaration, isConstantsDeclaration, blockInfo, VariableAccessType.LOCAL);
					} else if (command.isAsteriskCommentLine()) {
						executeOnCommentLine(command, variables, commentIdentifier);
					} else if (!command.isAbap()) {
						executeOnNonAbapSection(command, variables);
					} else {
						executeOnOtherCommand(command, variables);
					}
					
					// if macros are used in this method, rules like the UnusedVariablesRule and FinalVariableRule 
					// must skip this method, because variable definitions and/or usages may be out of sight
					if (command.usesMacro()) {
						variables.setMethodUsesMacrosOrTestInjection();
					}
				}
			} catch (UnexpectedSyntaxBeforeChanges ex) {
				ex.addToLog();
				skipMethod = true;
			}

			command = command.getNext();
		}
		if (!variables.isEmpty()) {
			if (!skipMethod)
				callExecuteOnMethod(code, methodStart, variables, releaseRestriction);
			variables = new Variables(this, null, null);
		}
	}

	private void callExecuteOnMethod(Code code, Command methodStart, Variables localVariables, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (methodStart != null && !methodStart.startsAMDPMethod()) {
			executeOn(code, methodStart, localVariables, releaseRestriction);
		}
	}

	private void addInterfaces(ClassInfo curClassOrInterface, Command command, HashMap<String, ClassInfo> classesAndInterfaces) {
		// INTERFACES intf [PARTIALLY IMPLEMENTED] 
		// { {[ABSTRACT METHODS meth1 meth2 ... ] [FINAL METHODS meth1 meth2 ... ]} | [ALL METHODS {ABSTRACT|FINAL}] } 
		// [DATA VALUES attr1 = val1 attr2 = val2 ...].
		
		// move to the first identifier
		Token identifier = command.getFirstToken().getNextCodeSibling();
		if (identifier.isChainColon())
			identifier = identifier.getNextCodeSibling();

		while (identifier != null && identifier.isIdentifier()) {
			// add interface implementation
			ClassInfo interfaceInfo = classesAndInterfaces.get(getNameKey(identifier.getText()));
			if (interfaceInfo != null) {
				curClassOrInterface.addInterface(interfaceInfo);
			}

			// move behind the next comma or period
			identifier = identifier.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",|.");
			if (identifier != null) {
				identifier = identifier.getNextCodeSibling();
			}
		}
	}

	private void addAliases(ClassInfo curClassOrInterface, Command command, HashMap<String, ClassInfo> classesAndInterfaces) {
		// aliases can be declared for both methods and attributes both in classes and in interfaces (regarding the interfaces they implement themselves)
		// ALIASES alias FOR intf~comp. 
		
		// move to the first identifier
		Token identifier = command.getFirstToken().getNextCodeSibling();
		if (identifier.isChainColon())
			identifier = identifier.getNextCodeSibling();

		while (identifier != null && identifier.matchesOnSiblings(true, TokenSearch.ANY_IDENTIFIER, "FOR", TokenSearch.ANY_IDENTIFIER)) {
			// add alias
			String alias = identifier.getText();
			identifier = identifier.getNextCodeSibling().getNextCodeSibling();
			String interfaceAndMethod = identifier.getText();
			curClassOrInterface.addAlias(alias, interfaceAndMethod);
			
			// move behind the next comma or period
			identifier = identifier.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",|.");
			if (identifier != null) {
				identifier = identifier.getNextCodeSibling();
			}
		}
	}

	private void addMethodDefinitions(ClassInfo curClassOrInterface, MethodVisibility methodVisibility, Command command) {
		Token token = command.getFirstCodeToken().getNextCodeSibling();
		while (token != null && token.isChainColon())
			token = token.getNextCodeSibling();
		token = token.getPrevCodeSibling();
		
		while (token != null && !token.isPeriod()) {
			// move to the method name 
			token = token.getNextCodeSibling();
			if (token == null || !token.isIdentifier())
				return;

			// determine some of the qualifiers:
			// - METHODS meth [ABSTRACT|FINAL] | [DEFAULT IGNORE|FAIL] [FOR EVENT evt OF {class|intf}] ...
			// - METHODS meth [FINAL] REDEFINITION.
			// - METHODS meth FOR TESTING.
			Token next = token.getNextCodeSibling();
			boolean isAbstract = next.isKeyword("ABSTRACT");
			boolean isFinal = next.isKeyword("FINAL");
			boolean isRedefinition = next.matchesOnSiblings(true, TokenSearch.makeOptional("FINAL"), "REDEFINITION");
			boolean isForTesting = next.matchesOnSiblings(true, "FOR", "TESTING");
			
			MethodInfo methodInfo = new MethodInfo(token, methodVisibility, isAbstract, isFinal, isRedefinition, isForTesting);
			curClassOrInterface.addMethod(methodInfo);
			
			// read the parameters and (class-based or non-class-based) exceptions
			VariableAccessType accessType = VariableAccessType.IMPORTING;
			while (!token.isCommaOrPeriod()) {
				if (token.isKeyword()) {
					// determine parameter access type
					if (token.isKeyword("IMPORTING"))
						accessType = VariableAccessType.IMPORTING;
					else if (token.isKeyword("EXPORTING"))
						accessType = VariableAccessType.EXPORTING;
					else if (token.isKeyword("CHANGING"))
						accessType = VariableAccessType.CHANGING;
					else if (token.isKeyword("RETURNING"))
						accessType = VariableAccessType.RETURNING;

					// the parameter name is the Token before "TYPE" or "LIKE", possibly inside "VALUE(...)" or "REFERENCE(...)",
					// and possibly with an escape char ! before it (e.g. "IMPORTING !iv_number TYPE i")
					if (token.isAnyKeyword("TYPE", "LIKE")) {
						Token parameterName = token.getPrevCodeSibling();
						boolean isByValue = false;
						boolean isReference = false;
						if (parameterName.closesLevel()) { // VALUE(...) or REFERENCE(...)
							parameterName = parameterName.getPrevSibling();
							isByValue = parameterName.isKeyword("VALUE(");
							isReference = parameterName.isKeyword("REFERENCE(");
							parameterName = parameterName.getFirstCodeChild(); 
						}
						String parameterNameText = parameterName.getText();
						if (parameterNameText.startsWith(ABAP.OPERAND_ESCAPE_CHAR_STRING))
							parameterNameText = parameterNameText.substring(ABAP.OPERAND_ESCAPE_CHAR_STRING.length());
						boolean isNeeded = isNeededPragmaOrPseudoCommentFound(parameterName, true);
						methodInfo.addParameter(new ParameterInfo(parameterName, parameterNameText, accessType, isNeeded, isByValue, isReference));
					}
					
					if (token.isKeyword("RAISING")) {
						// read class-based exceptions
						token = token.getNextCodeSibling();
						while (!token.isCommaOrPeriod()) {
							if (token.isKeyword("RESUMABLE(") && token.hasCodeChildren() && token.getFirstCodeChild().isIdentifier()) {
								methodInfo.addException(ExceptionInfo.createClassBased(token.getFirstCodeChild().getText(), true));
							} else if (token.isIdentifier()) {
								methodInfo.addException(ExceptionInfo.createClassBased(token.getText(), false));
							}
							token = token.getNextCodeSibling();
						}
						break;
					} else if (token.isKeyword("EXCEPTIONS")) {
						// read non-class-based exceptions
						token = token.getNextCodeSibling();
						while (!token.isCommaOrPeriod()) {
							if (token.isIdentifier()) {
								methodInfo.addException(ExceptionInfo.createNonClassBased(token.getText()));
							}
							token = token.getNextCodeSibling();
						}
						break;
					}
				}
				token = token.getNextCodeSibling();
			}
			// in case the token is a comma, continue with the next method definition
		}
	}

	private void executeOnDeclarationCommand(Command command, Command methodStart, Variables variables, boolean isTypeDeclaration, boolean isConstantsDeclaration, BlockInfo blockInfo, VariableAccessType variableAccessType) throws UnexpectedSyntaxBeforeChanges {
		Token token = command.getFirstCodeToken().getNextCodeToken();
		boolean isChain = token.isChainColon();
		if (isChain)
			token = token.getNextCodeToken();

		boolean isInOOContext = command.isInOOContext();
		while (token != null) {
			boolean isBoundStructuredData = false;
			boolean isTopBlockStart = false;
			boolean isBlockEnd = false;
			
			if (token.matchesOnSiblings(true, "END", "OF")) {
				blockInfo.endBlock();
				isBlockEnd = true;
				
			} else if (token.matchesOnSiblings(true, "BEGIN", "OF")) {
				blockInfo.beginBlock();

				if (blockInfo.blockLevel == 1) {
					// move token to the name of the structure to add its declaration below
					isTopBlockStart = true;
					token = token.getNextCodeSibling();
					token = token.getNextCodeSibling();
					isBoundStructuredData = true;
					
					if (token.isKeyword("ENUM")) {
						token = token.getNextCodeSibling();
					}
				}
			}
			
			if (blockInfo.blockLevel > 0 && !isTopBlockStart) {
				// declarations inside BEGIN OF ... blocks may contain usages of constants in LIKE ... or LENGTH ... clauses
				token = executeOnDeclarationLine(methodStart, variables, token, isInOOContext, blockInfo.topBlockVarInfo);
				
			} else if (!isBlockEnd) {
				// add declaration
				String varName = token.getText();
				VariableInfo varInfo = variables.addDeclaration(token, false, isTypeDeclaration, isConstantsDeclaration, isBoundStructuredData, isInOOContext, false, variableAccessType);
				if (isTopBlockStart) 
					blockInfo.setTopBlockVarInfo(varInfo);

				// if the pragma ##NEEDED or the pseudo comment "#EC NEEDED is defined for this variable, add a "usage" 
				// to prevent it from being commented out or deleted
				if (isNeededPragmaOrPseudoCommentFound(token, false)) {
					variables.setNeeded(varName);
				}

				// find usages of other variables or constants in LIKE ... or VALUE ... clauses
				token = executeOnDeclarationLine(methodStart, variables, token, isInOOContext, varInfo); 
			}
				
			if (!isChain || token == null) 
				return;
			
			// move to the next identifier
			token = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",|.");
			if (token != null) {
				token = token.getNextCodeToken();
			}
		}
		return;
	}

	private Token executeOnDeclarationLine(Command methodStart, Variables variables, Token token, boolean isInOOContext, VariableInfo varInfo) {
		// length may be supplied in parentheses: "DATA lv_text(lc_length)."
		Token firstChild = token.getFirstChild();
		if (token.getOpensLevel() && firstChild != null && firstChild.isAttached() && firstChild.isIdentifier()) {
			String usedObjectName = Variables.getObjectName(firstChild.getText(), isInOOContext);
			variables.addUsageInLikeOrValueClause(firstChild, usedObjectName, methodStart, varInfo);
			
			// continue behind the parenthesis
			token = token.getNextCodeSibling();
		}
		
		// if the declaration uses "LIKE ...", count that as a usage of that variable or constant
		Token next = token.getNextCodeSibling();
		if (next != null && next.isKeyword("LIKE") && next.getNextCodeSibling() != null) {
			token = next.getNextCodeSibling();
			// skip any keywords before the identifier of the data object, e.g. LINE OF, RANGE OF, REF TO, 
			// { STANDARD | SORTED | HASHED } TABLE OF 
			while (token.isKeyword() && token.getNextCodeSibling() != null) {
				token = token.getNextCodeSibling();
			}
			// the Token may contain more than the object name, e.g. 'DATA ls_struc LIKE LINE OF lr_ref->lt_table.'
			String usedObjectName = Variables.getObjectName(token.getText(), isInOOContext);
			variables.addUsageInLikeOrValueClause(token, usedObjectName, methodStart, varInfo);
		} 
		
		// if the declaration uses "LENGTH <constant>" and/or "VALUE <constant>", count that as a usage of <constant>
		while (token != null && !token.isCommaOrPeriod()) {
			if (token.isAnyKeyword("LENGTH", "VALUE")) {
				token = token.getNextCodeSibling();
				if (token != null && token.isIdentifier()) {
					// the Token may contain more than the object name, e.g. 'DATA lv_value TYPE i VALUE if_any_interface=>co_any_value.'
					// however, no calculations (lc_any + 1) or substrings (lc_data+4(2)) are possible
					String usedObjectName = Variables.getObjectName(token.getText(), isInOOContext);
					variables.addUsageInLikeOrValueClause(token, usedObjectName, methodStart, varInfo);
				}
			}
			token = token.getNextCodeSibling();
		}
		return token;
	}

	private void executeOnCommentLine(Command command, Variables variables, CommentIdentifier commentIdentifier) {
		// ignore * comment lines if no commentIdentifier is passed
		if (commentIdentifier == null)
			return;

		// determine whether the comment line looks like ABAP code
		CommentIdentification identification = commentIdentifier.identifyComment(command.getFirstToken().getText(), false, Language.ABAP);
		if (identification.isTextual() || identification.words == null || identification.words.length == 0)
			return;

		// do not process this line if it is a declaration line
		String firstWord = identification.words[0];
		for (String declarationKeyword : declarationKeywords) {
			if (AbapCult.stringEquals(firstWord, declarationKeyword, true))
				return;
		}
		// treat all words as if they were a usage of a local variable
		boolean isClearCommand = AbapCult.stringEqualsAny(true, firstWord, "CLEAR", "CLEAR:");
		boolean isAddCommand = AbapCult.stringEquals(firstWord, "ADD", true);
		boolean isSubtractCommand = AbapCult.stringEquals(firstWord, "SUBTRACT", true);
		boolean isMultiplyOrDivideCommand = AbapCult.stringEqualsAny(true, firstWord, "MULTIPLY", "DIVIDE");
		// TODO: the next line does not work, because identification does not include "=" 
		boolean isAssignmentCommand = identification.words.length > 2 && ABAP.isAssignmentOperator(identification.words[1]);
		String assignedToVar = isAssignmentCommand ? identification.words[0] : null;

		String prevWordText = "";
		for (int i = 0; i < identification.words.length; ++i) {
			String word = identification.words[i];
			boolean isAssignment = isClearCommand || (isAssignmentCommand && i == 0) || (isAddCommand && AbapCult.stringEquals(prevWordText, "TO", true))
					|| (isSubtractCommand && AbapCult.stringEquals(prevWordText, "FROM", true)) || (isMultiplyOrDivideCommand && i == 1);
			boolean isUsageInSelfAssignment = !isAssignment && AbapCult.stringEquals(word, assignedToVar, true);
			variables.addUsage(command.getFirstToken(), word, isAssignment, isUsageInSelfAssignment, true, false, false);
			prevWordText = word;
		}
	}

	private void executeOnNonAbapSection(Command command, Variables localVariables) throws UnexpectedSyntaxBeforeChanges {
		final String hostVarEscape = " :";
		
		if (localVariables.isEmpty())
			return;
		
		// try to find any (write or read) access to host variables, e.g. in a line like "WHERE bukrs = :lv_company_code"; 
		// note that for non-ABAP sections, each token may represent a full line
		// TODO: SQL comments are NOT yet considered here
		boolean isInOOContext = command.isInOOContext();
		Token token = command.getFirstToken();
		while (token != null) {
			String text = token.getText();
			int colonPos = 0;
			while (colonPos < text.length()) {
				colonPos = text.indexOf(hostVarEscape, colonPos);
				if (colonPos < 0)
					break;
				colonPos += hostVarEscape.length();
				String varName = ABAP.readTillEndOfVariableName(text, colonPos, true, isInOOContext);
				if (!StringUtil.isNullOrEmpty(varName)) {
					colonPos += varName.length();
					localVariables.addUsage(token, varName);
				}
			}
			
			token = token.getNext();
		}
	}
	
	private void executeOnOtherCommand(Command command, Variables localVariables) throws UnexpectedSyntaxBeforeChanges {
		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null)
			return;
		
		// in case of 'RETURN expr.', add a usage on the RETURNING parameter
		if (firstCode.isKeyword("RETURN")) {
			Token nextCode = firstCode.getNextCodeToken();
			if (nextCode != null && !nextCode.isPeriod() && localVariables.returningParameter != null) {
				localVariables.returningParameter.addUsage(firstCode, true, false, false, false, false);
			}
		}
		
		// determine whether the Command is an assignment (without inline declaration, which will be handled below) 
		boolean isAssignmentCommand = command.isAssignment(false, true);
		boolean isInOOContext = command.isInOOContext();

		// determine the receiving variable of an assignment, e.g. "lv_receiver" or "ls_receiver" in statements like 
		// - "lv_receiver = ..."
		// - "ls_receiver-component = ..."
		// - "lv_receiver+4(2) = ..."
		// note, however, that in "lo_instance->member = ...", lo_instance is NOT the receiver, because the assignment only happens to the memory 
		// which is *referenced* by lo_instance (so, in this case, lo_instance is *used*, NOT assigned to)  
		String assignedToVar = isAssignmentCommand ? Variables.getObjectName(firstCode.getText(), command.isInOOContext()) : null;
		if (assignedToVar != null && !isAccessToVarMemory(firstCode.getText(), assignedToVar.length(), isInOOContext)) // e.g. in "var->member = ...", "var" is not assigned to, but *used*
			assignedToVar = null;

		Token token = firstCode;
		while (token != null) {
			if (token.isKeyword("REF")) {
				addRefConstructor(token, localVariables);
				
			} else if (token.opensInlineDeclaration()) {
				boolean isAssignedInMessageInto = firstCode.isKeyword("MESSAGE") && token.getPrevCodeToken() != null && token.getPrevCodeToken().isKeyword("INTO");

				// process inline declaration
				token = token.getNext();
				localVariables.addDeclaration(token, true, false, false, false, isInOOContext, isAssignedInMessageInto, VariableAccessType.LOCAL);

				// if the pragma ##NEEDED is defined for this variable, add a "usage" to prevent it from being commented out or deleted
				if (isNeededPragmaOrPseudoCommentFound(token, false))
					localVariables.setNeeded(token.getText());

				// determine whether a field-symbol is assigned directly at its declaration position, using 
				// ASSIGN ... TO FIELD-SYMBOL(<...>) or LOOP AT ... ASSIGNING FIELD-SYMBOL(<...>) etc., 
				// or a data reference is created
				MemoryAccessType accessType = token.getMemoryAccessType();
				if (accessType == MemoryAccessType.ASSIGN_TO_FS_OR_DREF) {
					addAssignedFieldSymbolOrDataRef(token, localVariables);
					localVariables.addInlineDeclaration(token, token.getText(), isAssignedInMessageInto);
				}
	
			} else if (token.isIdentifier()) {
				// process non-comment, non-declaration code
				
				// get the name of the used object, e.g. "struc" from "struc-component", "var" from "var->member" or "var+offset(length)" etc.
				String tokenText = token.getText();
				int readPos = 0; 

				// remove the @ used in SQL statements, or the ! used for escaping identifiers
				if (AbapCult.stringStartsWithAny(tokenText, "@", ABAP.OPERAND_ESCAPE_CHAR_STRING))
					++readPos;

				// reduce the identifier to its first part, i.e.
				// - get the structure variable in cases of "structure-component",
				// - get the identifier of the instance in cases of "instance_var->member_var",
				// - get the first part of a substring expression like lv_date+4(2)
				String objectName = ABAP.readTillEndOfVariableName(tokenText, readPos, true, isInOOContext);
				readPos += objectName.length();
				
				// determine from the tokenText and the context whether this is an assignment
				boolean isAssignment = false;
				boolean writesToReferencedMemory = false; 
				boolean isFieldSymbol = ABAP.isFieldSymbol(objectName); 
				boolean isAssignedInMessageInto = false;
				if (isFieldSymbol) {
					// any mentioning of a field symbol is a 'usage', except in "ASSIGN ... TO <...>", "... ASSIGNING <...>" and "UNASSIGN <...>" commands: 
					// even a write to the memory referenced by the field symbol is not a write to the field symbol itself, as the reference is unchanged
					MemoryAccessType accessType = token.getMemoryAccessType();
					isAssignment = (accessType == MemoryAccessType.ASSIGN_TO_FS_OR_DREF);
					if (isAssignment) {
						addAssignedFieldSymbolOrDataRef(token, localVariables);
						writesToReferencedMemory = false;
					} else {
						writesToReferencedMemory = accessType.mayWrite;
					}
					
				} else if (isAccessToVarMemory(tokenText, readPos, isInOOContext)) {
					MemoryAccessType accessType = token.getMemoryAccessType();
					if (accessType == MemoryAccessType.ASSIGN_TO_FS_OR_DREF) {
						isAssignment = true;
						addAssignedFieldSymbolOrDataRef(token, localVariables);
					} else if (accessType == MemoryAccessType.WRITE) {
						isAssignment = true;
						isAssignedInMessageInto = firstCode.isKeyword("MESSAGE") && token.getPrevCodeToken() != null && token.getPrevCodeToken().isKeyword("INTO");
					} else if (accessType == MemoryAccessType.READ_WRITE || accessType == MemoryAccessType.READ_WRITE_POSSIBLE) {
						// if the variable is used as an actual CHANGING parameter, add both a usage and an assignment (below); 
						// same in case of PERFORM ... USING ..., where a write is not prevented by the syntax check (READ_WRITE_POSSIBLE) 
						localVariables.addUsage(token, objectName);
						isAssignment = true;
					}
				}

				boolean isUsageInSelfAssignment = !isAssignment && !isFieldSymbol && AbapCult.stringEquals(objectName, assignedToVar, true);
				if (!isAssignment && !isUsageInSelfAssignment && readPos + 1 == tokenText.length() && token.startsFunctionalMethodCall(true)) {
					// the functional method call "xyz(" is NOT a usage of the local variable "xyz"; however, this is only true if  
					// readPos points to the opening parenthesis, because "lo_instance->any_method(" is a usage of "lo_instance"
				} else {
					localVariables.addUsage(token, objectName, isAssignment, isUsageInSelfAssignment, false, writesToReferencedMemory, isAssignedInMessageInto);
				}
				
				// in "var+offset(length)" or "struc-component+offset(length), "offset" may be an identifier as well
				if (readPos < tokenText.length()) {
					int substringPos = tokenText.indexOf(ABAP.SUBSTRING_OFFSET, readPos);
					if (substringPos >= readPos) {
						objectName = ABAP.readTillEndOfVariableName(tokenText, substringPos + 1, false, isInOOContext);
						if (!StringUtil.isNullOrEmpty(objectName)) {
							localVariables.addUsage(token, objectName);
							readPos = substringPos + objectName.length();
						} else {
							readPos = tokenText.indexOf(ABAP.SUBSTRING_LENGTH_OPEN, substringPos);
						}
					}
				}
				
				// in "var+offset(length)", "length" may also be an identifier
				if (readPos > 0 && readPos < tokenText.length() && tokenText.charAt(readPos) == ABAP.SUBSTRING_LENGTH_OPEN) {
					objectName = ABAP.readTillEndOfVariableName(tokenText, readPos + 1, false, isInOOContext);
					if (!StringUtil.isNullOrEmpty(objectName)) {
						localVariables.addUsage(token, objectName);
					}
				}
			}
			token = token.getNextCodeToken();
		}
	}

	private boolean isAccessToVarMemory(String tokenText, int readPosAfterObjName, boolean isInOOContext) {
		if (readPosAfterObjName == tokenText.length()) {
			// tokenText contains the object name only
			return true;
			
		} else if (tokenText.charAt(readPosAfterObjName) == ABAP.SUBSTRING_OFFSET) {
			// tokenText defines a substring of object name, e.g. "lv_date+4(2)" - which is still an access to the memory represented by the variable 
			return true;
			
		} else if (tokenText.charAt(readPosAfterObjName) == ABAP.COMPONENT_SELECTOR && readPosAfterObjName + 1 < tokenText.length() 
				&& ABAP.isCharAllowedForVariableNames(tokenText, readPosAfterObjName + 1, true, false, isInOOContext)) {
			// tokenText defines a component of object name, e.g. "struc-component" - which also is an access to the memory represented by the variable
			return true;
			
		} else if (tokenText.charAt(readPosAfterObjName) == ABAP.TABLE_EXPR_BRACKET_OPEN) {
			// tokenText starts a table expression, e.g. "lt_any[ 1 ]" or "lt_any[ id = 1 ]-comp" or "lt_any[ 1 ]-inner[ 2 ]-comp";
			// which means an access to the memory represented by the variable; this is even considered true for cases of 
			// "lt_any[ 1 ]-oref->mv_attribute = 1."
			return true;
			
		} else {
			// in cases like "lo_instance->member_var", the assignment only happens to the memory which is *referenced* by lo_instance 
			// (so, in such a case, lo_instance is *used*, NOT assigned to)   
			return false;
		}
	}
	
	private boolean isNeededPragmaOrPseudoCommentFound(Token startToken, boolean stopAtLineBreak) {
		// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenpragma.htm:
		// A pragma applies to the current statement, that is to the statement that ends at the next . or ,. 
		// Pragmas in front of the: of a chained statement apply to the entire chained statement.
		
		// determine whether ##NEEDED or #EC NEEDED are found before the startToken (stopping the search at a comma or colon) 
		Token token = startToken;
		while (token != null) {
			if (token.isComma() || token.isChainColon()) 
				break;
			else if (token.isPragma() && token.textEquals("##NEEDED"))
				return true;
			if (stopAtLineBreak && token.lineBreaks > 0)
				break;
			token = token.getPrev();
		}

		// determine whether ##NEEDED or #EC NEEDED are found before the next comma or period 
		// (or #EC NEEDED within the comments following it),
		boolean commaOrPeriodFound = false;
		token = startToken;
		while (token != null) {
			if (token.isCommaOrPeriod()) {
				commaOrPeriodFound = true;
			} else if (commaOrPeriodFound && !token.isComment()) {
				// at this position, #EC NEEDED no more applies to the startToken  
				break;
			}
			// ##NEEDED belongs before the comma or period, while #EC NEEDED can be part of the comments following it 
			if (!commaOrPeriodFound && token.isPragma() && token.textEquals("##NEEDED"))
				return true;
			if (token.isComment() && token.textStartsWith(ABAP.PSEUDO_COMMENT_EC_PREFIX + "NEEDED"))
				return true;
			token = token.getNext();
			if (stopAtLineBreak && token != null && token.lineBreaks > 0) {
				break;
			}
		}

		// determine whether ##NEEDED or #EC NEEDED are found before the chain colon 
		// (or #EC NEEDED within the comments following it) - they then apply to all elements of the chain
		boolean chainColonFound = false;
		Command command = startToken.getParentCommand();
		if (command.containsChainColon()) {
			token = command.getFirstToken();
			while (token != null) {
				if (token.isChainColon()) {
					chainColonFound = true;
				} else if (chainColonFound && !token.isComment()) {
					// at this position, #EC NEEDED no more applies to the chain
					break;
				}
				if (!chainColonFound && token.isPragma() && token.textEquals("##NEEDED"))
					return true;
				if (token.isComment() && token.textStartsWith(ABAP.PSEUDO_COMMENT_EC_PREFIX + "NEEDED"))
					return true;
				token = token.getNext();
			}
		}
		return false;
	}

	private void addRefConstructor(Token refKeyword, Variables localVariables) {
		// In cases like 'DATA(dref) = REF #( dobj )' or 'any_method( ir_data = REF #( dobj ) )', note down that 
		// a data reference is created to the memory area of the local variable; in such a case, the FinalVariableRule 
		// will never change DATA(dobj) to FINAL(dobj), because the data reference could change the memory anywhere. 
		Token dataType = refKeyword.getNextCodeSibling();
		if (dataType.getOpensLevel() && dataType.hasCodeChildren()) {
			Token identifier = dataType.getFirstCodeChild(); 
			VariableInfo varInfo = localVariables.getVariableInfo(identifier, false);
			if (varInfo != null) {
				varInfo.addReferenceByDataRef();
			}
		}
	}
	
	private void addAssignedFieldSymbolOrDataRef(Token fieldSymbolOrDataRef, Variables localVariables) {
		// a) In cases like 'ASSIGN itab[ ... ] TO <fs>', notify itab about the assignment of a <fs>, so indirect  
		// write accesses to itab with '<fs>-component = 1' can later be identified as (potential) write accesses to itab.

		// b) In cases like 'GET REFERENCE OF dobj INTO dref', note down that a data reference points to the memory area
		// of the local variable; in such a case, the FinalVariableRule will never change DATA(dobj) to FINAL(dobj), 
		// because the data reference could be passed on and the data changed from any place. This is relevant for: 

		// Both a) and b) is only relevant in the following cases:
		// - ASSIGN COMPONENT ... OF STRUCTURE struc TO <fs>
		// - ASSIGN ...itab... TO <fs>
		// - LOOP AT [GROUP] itab ... ASSIGNING <fs> / REFERENCE INTO dref
		// - READ TABLE itab ... ASSIGNING <fs> / REFERENCE INTO dref
		// - GET REFERENCE OF dobj INTO dref
		
		// By contrast, we do NOT have to consider cases where the itab itself is already in a write position, such as
		// - INSERT ... INTO [TABLE] itab ASSIGNING <fs> / REFERENCE INTO dref
		// - MODIFY [TABLE] itab ... ASSIGNING <fs> / REFERENCE INTO dref
		// - COLLECT ... INTO itab ASSIGNING <fs> / REFERENCE INTO dref

		// The REF #( ) constructor expression is handled in addRefConstructor()
		
		// determine the last keyword of ASSIGN / LOOP AT [GROUP] / READ TABLE / GET REFERENCE OF / dref = REF ...
		Token firstCode = fieldSymbolOrDataRef.getParentCommand().getFirstCodeToken();
		if (firstCode == null)
			return;
		Token lastKeyword = firstCode.getLastTokenOnSiblings(true, "ASSIGN", "COMPONENT", TokenSearch.ASTERISK, "OF", "STRUCTURE");
		if (lastKeyword == null)
			lastKeyword = firstCode.getLastTokenOnSiblings(true, "ASSIGN");
		if (lastKeyword == null)
			lastKeyword = firstCode.getLastTokenOnSiblings(true, "LOOP", "AT", TokenSearch.makeOptional("GROUP"));
		if (lastKeyword == null)
			lastKeyword = firstCode.getLastTokenOnSiblings(true, "READ", "TABLE");
		if (lastKeyword == null)
			lastKeyword = firstCode.getLastTokenOnSiblings(true, "GET", "REFERENCE", "OF");
		if (lastKeyword == null)
			return;			

		// determine the identifier after the last keyword - this is the memory area that is being assigned or referenced
		Token identifier = lastKeyword.getNextCodeSibling();
		if (identifier == null || !identifier.isIdentifier())
			return;

		// get the VariableInfo of that identifier, if it is a local variable (in that case, it must already be known)
		VariableInfo varInfo = localVariables.getVariableInfo(identifier, false);
		if (varInfo == null)
			return;
		
		if (ABAP.isFieldSymbol(fieldSymbolOrDataRef.getText())) {
			// enter the field-symbol to the list of field-symbols that are assigned to a memory area of the local variable
			VariableInfo fieldSymbolInfo = localVariables.getVariableInfo(fieldSymbolOrDataRef.getText(), false);
			varInfo.addAssignedFieldSymol(fieldSymbolInfo);

		} else {
			// note down that a data reference points to the memory area of the local variable
			varInfo.addReferenceByDataRef();
		}
	}
}