# Available Cleanup Rules

ABAP cleaner offers 100 cleanup rules with a total of 468 configuration options:

## Empty Lines

* [Standardize empty lines within methods](rules/EmptyLinesWithinMethodsRule.md)
* [Separate methods and classes with empty lines](rules/EmptyLinesOutsideMethodsRule.md)
* [Standardize empty lines in class definitions](rules/EmptyLinesInClassDefinitionRule.md)
* [Standardize test classes for CDS views](rules/CdsTestClassLinesRule.md)
* [Move commands to own lines](rules/OneCommandPerLineRule.md)

## Spaces

* [Put spaces around text literals](rules/SpaceAroundTextLiteralRule.md)
* [Close brackets at line end](rules/ClosingBracketsPositionRule.md)
* [Remove space before commas and period](rules/SpaceBeforePeriodRule.md)
* [Put spaces around " comment sign](rules/SpaceAroundCommentSignRule.md)
* [Remove needless spaces](rules/NeedlessSpacesRule.md)

## Declarations

* [Unchain into multiple statements](rules/ChainRule.md)
* [Remove needless CLEAR](rules/NeedlessClearRule.md)
* [Rearrange local declarations](rules/LocalDeclarationOrderRule.md)
* [Report unused parameters](rules/UnusedParametersRule.md)
* [Delete unused variables](rules/UnusedVariablesRule.md)
* [Simplify a chain with one element](rules/ChainOfOneRule.md)
* [Make implicit type explicit](rules/ImplicitTypeRule.md)
* [Use FINAL for immutable variables](rules/FinalVariableRule.md)
* [Standardize CLASS ... DEFINITION](rules/ClassDefinitionRule.md)
* [Standardize escaping of \!parameters](rules/EscapeCharForParametersRule.md)
* [Remove empty class definition SECTIONs](rules/EmptySectionsInClassDefRule.md)
* [Add missing parameters to ABAP Doc](rules/AbapDocParametersRule.md)
* [Remove lang="en" from ABAP Doc](rules/AbapDocLangRule.md)

## Syntax

* [Comment with ", not with \* \(for text\)](rules/CommentTypeRule.md)
* [Remove end-of comments](rules/EndOfCommentRule.md)
* [Replace obsolete pseudo comments with pragmas](rules/PseudoCommentRule.md)
* [Move pragmas to correct position](rules/PragmaPositionRule.md)
* [Correct frequent typos](rules/TypoRule.md)
* [Resolve equals sign chain into several commands](rules/EqualsSignChainRule.md)
* [Prefer calculation assignment operators \(\+=, -= etc.\)](rules/CalculationAssignmentRule.md)
* [Use consistent set of comparison operators](rules/ComparisonOperatorRule.md)
* [Prefer IS NOT to NOT IS](rules/NotIsRule.md)
* [Move AND/OR etc. from line end to next line start](rules/LogicalOperatorPositionRule.md)
* [Use string templates to assemble text](rules/StringTemplateRule.md)
* [Remove needless parentheses](rules/NeedlessParenthesesRule.md)
* [Remove empty commands](rules/EmptyCommandRule.md)
* [Shorten VALUE statements](rules/ValueStatementRule.md)
* [Remove the self-reference me->](rules/SelfReferenceMeRule.md)
* [Omit RECEIVING](rules/ReceivingKeywordRule.md)
* [Omit optional EXPORTING](rules/ExportingKeywordRule.md)

## Commands

* [Replace EXIT outside loop with RETURN](rules/ExitOutsideLoopRule.md)
* [Convert CHECK outside loop to IF NOT ... RETURN](rules/CheckOutsideLoopRule.md)
* [Convert CHECK in loop to IF NOT ... CONTINUE](rules/CheckInLoopRule.md)
* [Replace long IF blocks at loop end](rules/IfBlockAtLoopEndRule.md)
* [Replace long IF blocks at method end](rules/IfBlockAtMethodEndRule.md)
* [Replace CALL METHOD with functional call](rules/CallMethodRule.md)
* [Replace CREATE OBJECT with NEW constructor](rules/CreateObjectRule.md)
* [Replace RAISE ... TYPE with RAISE ... NEW](rules/RaiseTypeRule.md)
* [Replace obsolete ADD ... TO etc. with \+= etc.](rules/AddToEtcRule.md)
* [Replace obsolete MOVE ... TO with =](rules/MoveToRule.md)
* [Replace TRANSLATE with string functions](rules/TranslateRule.md)
* [Replace CONDENSE with string function](rules/CondenseRule.md)
* [Replace DESCRIBE TABLE ... LINES with lines\( \)](rules/DescribeTableRule.md)
* [Replace READ TABLE with table expression](rules/ReadTableRule.md)
* [Use assert\_true and assert\_false](rules/AssertEqualsBooleanRule.md)
* [Use assert\_subrc instead of assert\_equals](rules/AssertEqualsSubrcRule.md)
* [Use assert class instead of ASSERT](rules/AssertClassRule.md)
* [Standardize assertion parameter order](rules/AssertParameterOrderRule.md)

## Pretty Printer

* [Convert upper and lower case](rules/UpperAndLowerCaseRule.md)
* [Use CamelCase for known CDS names](rules/CamelCaseNameRule.md)
* [Use CamelCase in test class for CDS view](rules/CamelCaseInCdsTestRule.md)
* [Indent lines](rules/IndentRule.md)

## Alignment

* [Align ABAP Doc](rules/AlignAbapDocRule.md)
* [Align METHODS declarations](rules/AlignMethodsDeclarationRule.md)
* [Align METHODS ... FOR TESTING](rules/AlignMethodsForTestingRule.md)
* [Align METHODS ... REDEFINITION](rules/AlignMethodsRedefinitionRule.md)
* [Align ALIASES ... FOR ...](rules/AlignAliasesForRule.md)
* [Align declarations](rules/AlignDeclarationsRule.md)
* [Align assignments to the same object](rules/AlignAssignmentsRule.md)
* [Align keywords with second word of first line](rules/AlignWithSecondWordRule.md)
* [Align CLEAR:, FREE:, SORT and CATCH](rules/AlignClearFreeAndSortRule.md)
* [Align SELECT clauses](rules/AlignSelectClausesRule.md)
* [Align SELECT ... FROM ... JOIN](rules/AlignSelectFromRule.md)
* [Align SELECT lists](rules/AlignSelectListsRule.md)
* [Align parameters and components](rules/AlignParametersRule.md)
* [Align logical expressions](rules/AlignLogicalExpressionsRule.md)
* [Align conditional expressions](rules/AlignCondExpressionsRule.md)
* [Align FORM declarations](rules/AlignFormDeclarationRule.md)
* [Align PERFORM parameters](rules/AlignPerformRule.md)

## DDL Annotations

* [Standardize annotation layout](rules/DdlAnnotationLayoutRule.md)
* [Rearrange annotations](rules/DdlAnnotationNestingRule.md)

## DDL Line Breaks and Indent

* [Break before DEFINE etc.](rules/DdlPositionDefineRule.md)
* [Break before AS SELECT etc.](rules/DdlPositionSelectRule.md)
* [Break before JOINs](rules/DdlPositionJoinRule.md)
* [Break before ASSOCIATIONs](rules/DdlPositionAssociationRule.md)
* [Break before select list braces](rules/DdlPositionBracesRule.md)
* [Break before WHERE clause etc.](rules/DdlPositionClausesRule.md)

## DDL Spaces and Spelling

* [Standardize spaces around colon, comma etc.](rules/DdlSpacesAroundSignsRule.md)
* [Standardize spaces around brackets](rules/DdlSpacesAroundBracketsRule.md)
* [Use CamelCase for known entity and field names](rules/DdlCamelCaseNameRule.md)
* [Correct frequent typos in DDL comments](rules/DdlTypoRule.md)

## DDL Alignment

* [Align view parameters](rules/DdlAlignEntityParametersRule.md)
* [Align source parameters](rules/DdlAlignSourceParametersRule.md)
* [Align function parameters after =>](rules/DdlAlignFunctionParametersRule.md)
* [Align logical expressions in views](rules/DdlAlignLogicalExpressionsRule.md)
* [Align name list and GROUP BY list](rules/DdlAlignFieldListsRule.md)
* [Align JOINs and ASSOCIATIONs](rules/DdlAlignDataSourcesRule.md)
* [Align select list](rules/DdlAlignSelectListRule.md)

## DDL Empty Lines

* [Standardize empty lines between sections](rules/DdlEmptyLinesBetweenSectionsRule.md)
* [Standardize empty lines within sections](rules/DdlEmptyLinesWithinSectionsRule.md)

[**Back to first page**](../README.md)

