[<-- previous rule](LocalDeclarationOrderRule.md) | [overview](../rules.md) | [next rule -->](UnusedVariablesRule.md)

# Report unused parameters

Adds TODO comments for parameters that are never used or assigned. Comments are suppressed for parameters declared as \#\#NEEDED in the method signature.

Parameters can only be checked if the method signature is found in the same code document. Methods that use any macros are skipped.

## References

* [Clean ABAP Styleguide: Clear or overwrite EXPORTING reference parameters](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#clear-or-overwrite-exporting-reference-parameters)
* [Clean ABAP Styleguide: Don't clear VALUE parameters](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-clear-value-parameters)

## Options

* Report IMPORTING parameters: \[in non-interface methods\]
* Report EXPORTING parameters: \[in all methods\]
* Report CHANGING parameters: \[in non-interface methods\]
* Report RETURNING parameters: \[never\]
* \[X\] Only add TODO comments in methods with executable statements
* \[X\] Do not add TODO comments for EXPORTING VALUE\(...\) parameters

## Examples


```ABAP

CLASS cl_report_unused_parameters DEFINITION.
  PUBLIC SECTION.
    METHODS any_method
      IMPORTING its_any_table        TYPE ty_ts_table
                iv_unused_but_needed TYPE string ##NEEDED " suppresses a TODO comment on this parameter
      EXPORTING ev_count             TYPE i
      CHANGING  cs_any_struc         TYPE ty_s_struc
      RETURNING VALUE(rv_result)     TYPE i.

  PROTECTED SECTION.
    METHODS other_method
      IMPORTING iv_any_value     TYPE i
      EXPORTING et_error         TYPE ty_tt_error
                VALUE(ev_status) TYPE i  " this is automatically cleared, because it is passed by value
      RETURNING VALUE(rv_result) TYPE i. " the same is always true for RETURNING parameters

  PRIVATE SECTION.
    METHODS third_method
      IMPORTING iv_any_text      TYPE string
      EXPORTING ev_any_value     TYPE i
      CHANGING  cs_any_struc     TYPE ty_s_struc
      RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.


CLASS cl_report_unused_parameters IMPLEMENTATION.
  METHOD any_method.
    CLEAR ev_count.

*    LOOP AT its_any_table ASSIGNING <ls_any_struc>.
*      lv_commented_out += 1.
*    ENDLOOP.
*
*    this comment mentions the parameter cs_any_struc, but that does not count:
*    only parameters in commented-out ABAP code count, not parameters in text comments!
  ENDMETHOD.

  METHOD other_method.
    " the next line implicitly assigns the returning parameter
    RETURN 42.
  ENDMETHOD.

  METHOD third_method.
    " This method does not yet contain an executable statement,
    " maybe because it was not yet implemented, or because it is obsolete.
    " You may therefore not want TODO comments to be added here.
  ENDMETHOD.
ENDCLASS.
```

Resulting code:

```ABAP

CLASS cl_report_unused_parameters DEFINITION.
  PUBLIC SECTION.
    METHODS any_method
      IMPORTING its_any_table        TYPE ty_ts_table
                iv_unused_but_needed TYPE string ##NEEDED " suppresses a TODO comment on this parameter
      EXPORTING ev_count             TYPE i
      CHANGING  cs_any_struc         TYPE ty_s_struc
      RETURNING VALUE(rv_result)     TYPE i.

  PROTECTED SECTION.
    METHODS other_method
      IMPORTING iv_any_value     TYPE i
      EXPORTING et_error         TYPE ty_tt_error
                VALUE(ev_status) TYPE i  " this is automatically cleared, because it is passed by value
      RETURNING VALUE(rv_result) TYPE i. " the same is always true for RETURNING parameters

  PRIVATE SECTION.
    METHODS third_method
      IMPORTING iv_any_text      TYPE string
      EXPORTING ev_any_value     TYPE i
      CHANGING  cs_any_struc     TYPE ty_s_struc
      RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.


CLASS cl_report_unused_parameters IMPLEMENTATION.
  METHOD any_method.
    " TODO: parameter ITS_ANY_TABLE is only used in commented-out code (ABAP cleaner)
    " TODO: parameter CS_ANY_STRUC is never used or assigned (ABAP cleaner)

    CLEAR ev_count.

*    LOOP AT its_any_table ASSIGNING <ls_any_struc>.
*      lv_commented_out += 1.
*    ENDLOOP.
*
*    this comment mentions the parameter cs_any_struc, but that does not count:
*    only parameters in commented-out ABAP code count, not parameters in text comments!
  ENDMETHOD.

  METHOD other_method.
    " TODO: parameter IV_ANY_VALUE is never used (ABAP cleaner)
    " TODO: parameter ET_ERROR is never cleared or assigned (ABAP cleaner)

    " the next line implicitly assigns the returning parameter
    RETURN 42.
  ENDMETHOD.

  METHOD third_method.
    " This method does not yet contain an executable statement,
    " maybe because it was not yet implemented, or because it is obsolete.
    " You may therefore not want TODO comments to be added here.
  ENDMETHOD.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/UnusedParametersRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/UnusedParametersTest.java)

