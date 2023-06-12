[<-- previous rule](EscapeCharForParametersRule.md) | [overview](../rules.md) | [next rule -->](AbapDocParametersRule.md)

# Remove empty class definition SECTIONs

Removes empty PUBLIC / PROTECTED / PRIVATE SECTIONs from class definitions \(except from public classes that are non-final or inheriting\).

SECTIONs that contain comments will be kept, except for the auto-generated 'do not include other source files here\!\!\!' comments.

## Options

* Scope: \[Remove any empty SECTION from non-empty class definitions\]

## Examples


```ABAP

" it makes no sense for a FINAL class to have a PROTECTED SECTION
CLASS cl_any_final_class DEFINITION FINAL.
  PUBLIC SECTION.
*"* public components of class CL_ANY_FINAL_CLASS
*"* do not include other source files here!!!
    INTERFACES if_any_interface.

  PROTECTED SECTION.
*"* protected components of class CL_ANY_FINAL_CLASS
*"* do not include other source files here!!!

  PRIVATE SECTION.
*"* private components of class CL_ANY_FINAL_CLASS
*"* do not include other source files here!!!

ENDCLASS.


" the following class only has definitions in the PRIVATE SECTION,
" so the other two sections may not be needed
CLASS cl_any_non_final_class DEFINITION FOR TESTING.
  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_any_data TYPE i.

    METHODS setup.
    METHODS any_test_method FOR TESTING.
ENDCLASS.


" the following class was probably freshly created with a template,
" so it can be assumed that its sections should be kept for now
CLASS cl_any_empty_class DEFINITION.
  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.
```

Resulting code:

```ABAP

" it makes no sense for a FINAL class to have a PROTECTED SECTION
CLASS cl_any_final_class DEFINITION FINAL.
  PUBLIC SECTION.
*"* public components of class CL_ANY_FINAL_CLASS
*"* do not include other source files here!!!
    INTERFACES if_any_interface.

ENDCLASS.


" the following class only has definitions in the PRIVATE SECTION,
" so the other two sections may not be needed
CLASS cl_any_non_final_class DEFINITION FOR TESTING.
  PRIVATE SECTION.
    DATA mv_any_data TYPE i.

    METHODS setup.
    METHODS any_test_method FOR TESTING.
ENDCLASS.


" the following class was probably freshly created with a template,
" so it can be assumed that its sections should be kept for now
CLASS cl_any_empty_class DEFINITION.
  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/EmptySectionsInClassDefRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/EmptySectionsInClassDefTest.java)

