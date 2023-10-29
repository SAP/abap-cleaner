[<-- previous rule](ClassDefinitionRule.md) | [overview](../rules.md) | [next rule -->](EmptySectionsInClassDefRule.md)

# Standardize escaping of \!parameters

Removes or adds the \! escape character for parameter names in method declarations, making its usage independent from which IDE the method was created with.

## Options

* Use \! escape character for parameters \[only if parameter name is an ABAP word\]

## Examples


```ABAP

CLASS cl_escape_char_for_params DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " SAP GUI creates method declarations with ! before each parameter name,
      " but not before VALUE(...), REFERENCE(...) or in the RAISING / EXCEPTIONS section:
      any_chained_method
        IMPORTING
          !it_source_table  TYPE ty_tt_any OPTIONAL
          !iv_name          TYPE string
        EXPORTING
          !et_error_table   TYPE ty_tt_any
          !ev_value         TYPE i
        CHANGING
          !cts_result_table TYPE ty_ts_any,

      " ABAP Development Tools creates method declarations without escape characters:
      other_chained_method
        IMPORTING
          iv_any_param     TYPE i      OPTIONAL
          iv_other_param   TYPE string DEFAULT 'abc'
        RETURNING
          VALUE(rv_result) TYPE char10
        RAISING
          cx_any_exception,

      " the escape char ! is recommendable if the parameter name is also an ABAP word:
      third_chained_method
        IMPORTING
          min    TYPE i DEFAULT 0
          max    TYPE i DEFAULT 100
          sum    TYPE i
        EXPORTING
          output TYPE string.

    " in some rather theoretical cases, ! is strictly necessary to prevent syntax errors:
    METHODS fourth_method
      IMPORTING
        !optional   TYPE i
        !default    TYPE i
        !preferred  TYPE i OPTIONAL
        !exporting  TYPE i OPTIONAL
      EXPORTING
        !changing   TYPE string
      CHANGING
        !raising    TYPE i
        !exceptions TYPE string.
ENDCLASS.
```

Resulting code:

```ABAP

CLASS cl_escape_char_for_params DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " SAP GUI creates method declarations with ! before each parameter name,
      " but not before VALUE(...), REFERENCE(...) or in the RAISING / EXCEPTIONS section:
      any_chained_method
        IMPORTING
          it_source_table  TYPE ty_tt_any OPTIONAL
          iv_name          TYPE string
        EXPORTING
          et_error_table   TYPE ty_tt_any
          ev_value         TYPE i
        CHANGING
          cts_result_table TYPE ty_ts_any,

      " ABAP Development Tools creates method declarations without escape characters:
      other_chained_method
        IMPORTING
          iv_any_param     TYPE i      OPTIONAL
          iv_other_param   TYPE string DEFAULT 'abc'
        RETURNING
          VALUE(rv_result) TYPE char10
        RAISING
          cx_any_exception,

      " the escape char ! is recommendable if the parameter name is also an ABAP word:
      third_chained_method
        IMPORTING
          !min    TYPE i DEFAULT 0
          !max    TYPE i DEFAULT 100
          !sum    TYPE i
        EXPORTING
          !output TYPE string.

    " in some rather theoretical cases, ! is strictly necessary to prevent syntax errors:
    METHODS fourth_method
      IMPORTING
        !optional   TYPE i
        !default    TYPE i
        !preferred  TYPE i OPTIONAL
        !exporting  TYPE i OPTIONAL
      EXPORTING
        !changing   TYPE string
      CHANGING
        !raising    TYPE i
        !exceptions TYPE string.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/EscapeCharForParametersRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/EscapeCharForParametersTest.java)

