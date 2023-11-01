[<-- previous rule](AlignCondExpressionsRule.md) | [overview](../rules.md) | [next rule -->](AlignPerformRule.md)

# Align FORM declarations

Aligns obsolete subroutine declarations with FORM.

## Options

* Continue after FORM name for up to \[3\] parameters
* \[X\] Continue after TABLES / USING / CHANGING / RAISING
* \[X\] Align TYPEs
* \[X\] Add empty line after multi-line FORM declaration
* \[X\] Remove empty line after one-line FORM declaration

## Examples


```ABAP

FORM any_subroutine USING iv_any_value TYPE string.

  " any subroutine implementation
ENDFORM.


FORM other_subroutine USING iv_any_value TYPE i iv_other_value TYPE string CHANGING cv_third_value TYPE i.
  " other subroutine implementation
ENDFORM.


FORM third_subr_with_a_long_name TABLES it_any_table STRUCTURE ty_s_any_struc
  it_other_table TYPE STANDARD TABLE it_third_table it_fourth_table TYPE ty_tt_any
  CHANGING ct_table TYPE ty_tt_table cs_struc TYPE LINE OF ty_tt_any cs_other_struc LIKE cs_any
    cs_third_struc LIKE LINE OF ct_table.
  " third subroutine implementation
ENDFORM.


FORM fourth_subroutine
  USING
    VALUE(iv_any) TYPE string
    iv_other TYPE REF TO object
  RAISING
    cx_any_exception RESUMABLE(cx_other_exception) cx_third_exception.
  " fourth subroutine implementation
ENDFORM.
```

Resulting code:

```ABAP

FORM any_subroutine USING iv_any_value TYPE string.
  " any subroutine implementation
ENDFORM.


FORM other_subroutine USING    iv_any_value   TYPE i
                               iv_other_value TYPE string
                      CHANGING cv_third_value TYPE i.

  " other subroutine implementation
ENDFORM.


FORM third_subr_with_a_long_name
  TABLES   it_any_table    STRUCTURE ty_s_any_struc
           it_other_table  TYPE STANDARD TABLE
           it_third_table
           it_fourth_table TYPE ty_tt_any
  CHANGING ct_table        TYPE ty_tt_table
           cs_struc        TYPE LINE OF ty_tt_any
           cs_other_struc  LIKE cs_any
           cs_third_struc  LIKE LINE OF ct_table.

  " third subroutine implementation
ENDFORM.


FORM fourth_subroutine
  USING   VALUE(iv_any) TYPE string
          iv_other      TYPE REF TO object
  RAISING cx_any_exception
          RESUMABLE(cx_other_exception)
          cx_third_exception.

  " fourth subroutine implementation
ENDFORM.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignFormDeclarationRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignFormDeclarationTest.java)

