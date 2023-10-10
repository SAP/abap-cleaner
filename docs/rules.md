# Available Cleanup Rules

ABAP cleaner offers 60 cleanup rules with a total of 191 configuration options:

## Empty Lines

* [Standardize empty lines within methods](rules/EmptyLinesWithinMethodsRule.md)
* [Separate methods and classes with empty lines](rules/EmptyLinesOutsideMethodsRule.md)
* [Standardize empty lines in class definitions](rules/EmptyLinesInClassDefinitionRule.md)

## Spaces

* [Standardize spaces next to parentheses](rules/SpacesInEmptyBracketsRule.md)
* [Close brackets at line end](rules/ClosingBracketsPositionRule.md)
* [Remove space before commas and period](rules/SpaceBeforePeriodRule.md)
* [Put spaces around " comment sign](rules/SpaceAroundCommentSignRule.md)
* [Remove needless spaces](rules/NeedlessSpacesRule.md)

## Declarations

* [Unchain into multiple statements](rules/ChainRule.md)
* [Rearrange local declarations](rules/LocalDeclarationOrderRule.md)
* [Delete unused variables](rules/UnusedVariablesRule.md)
* [Simplify a chain with one element](rules/ChainOfOneRule.md)
* [Make implicit type explicit](rules/ImplicitTypeRule.md)
* [Use FINAL for immutable variables](rules/FinalVariableRule.md)
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
* [Prefer =, <>, <= etc. to EQ, NE, LE etc.](rules/ComparisonOperatorRule.md)
* [Prefer IS NOT to NOT IS](rules/NotIsRule.md)
* [Move AND/OR etc. from line end to next line start](rules/LogicalOperatorPositionRule.md)
* [Remove empty commands](rules/EmptyCommandRule.md)
* [Shorten VALUE statements](rules/ValueStatementRule.md)
* [Remove the self-reference me->](rules/SelfReferenceMeRule.md)
* [Omit RECEIVING](rules/ReceivingKeywordRule.md)
* [Omit optional EXPORTING](rules/ExportingKeywordRule.md)

## Commands

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
* [Use assert\_true and assert\_false](rules/AssertEqualsBooleanRule.md)
* [Use assert\_subrc instead of assert\_equals](rules/AssertEqualsSubrcRule.md)
* [Use assert class instead of ASSERT](rules/AssertClassRule.md)

## Pretty Printer

* [Convert upper and lower case](rules/UpperAndLowerCaseRule.md)
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
* [Align CLEAR:, FREE: and SORT](rules/AlignClearFreeAndSortRule.md)
* [Align parameters and components](rules/AlignParametersRule.md)
* [Align logical expressions](rules/AlignLogicalExpressionsRule.md)
* [Align conditional expressions](rules/AlignCondExpressionsRule.md)

[**Back to first page**](../README.md)

