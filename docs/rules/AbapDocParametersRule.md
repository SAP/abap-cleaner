[<-- previous rule](EmptySectionsInClassDefRule.md) | [overview](../rules.md) | [next rule -->](AbapDocLangRule.md)

# Add missing parameters to ABAP Doc

Adds missing parameters and exceptions to existing ABAP Doc comments and updates their order.

No ABAP Doc comments are added for exceptions of FOR TESTING methods.

## Options

* Add missing parameters to ABAP Doc \[Only to non-synchronized ABAP Doc\]
* Add missing exceptions to ABAP Doc \[Only to non-synchronized ABAP Doc\]
* CAUTION: option 'Always' may delete synchronized descriptions that were added with SAP GUI
* \[ \] Only add if at least one parameter or exception is already documented
* \[X\] Update order of parameters and exceptions in ABAP Doc
* Delete obsolete parameters from ABAP Doc \[Only if description is empty\]
* Delete obsolete exceptions from ABAP Doc \[Only if description is empty\]

## Examples


```ABAP

CLASS cl_any_class DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    "! <p class="shorttext synchronized">any method documentation (synchronized)</p>
    "!
    "! @parameter iv_any_parameter | <p class="shorttext synchronized">any parameter documentation</p>
    "! @raising cx_any_exception | <p class="shorttext synchronized">exception documentation</p>
    "! @raising cx_obsolete_exception | <p class="shorttext synchronized">obsolete exception which was deleted</p>
    "! @raising cx_also_obsolete | <p class="shorttext synchronized"></p>
    METHODS any_method
      IMPORTING iv_any_parameter   TYPE i
                iv_other_parameter TYPE i
      RAISING   cx_any_exception
                cx_other_exception.

    METHODS:
      "! other method documentation (not synchronized)
      "!
      "! @parameter iv_second_parameter | second parameter documentation
      "! @parameter iv_first_parameter | first parameter documentation
      "! @raising cx_other_exception | other exception documentation
      other_method
        IMPORTING iv_first_parameter  TYPE c
                  iv_second_parameter TYPE c
        EXPORTING ev_third_parameter  TYPE c
        RAISING   cx_any_exception
                  cx_other_exception,

      "! other method documentation (not synchronized)
      "!
      "! @parameter iv_obsolete_parameter | obsolete parameter which was deleted
      "! @parameter iv_also_obsolete  |
      third_method,

      "! <p class="shorttext synchronized" lang="en">fourth method documentation (synchronized with lang)</p>
      fourth_method
        IMPORTING  iv_param_a TYPE i
                   iv_param_b TYPE i
        EXCEPTIONS any_exception.
ENDCLASS.
```

Resulting code:

```ABAP

CLASS cl_any_class DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    "! <p class="shorttext synchronized">any method documentation (synchronized)</p>
    "!
    "! @parameter iv_any_parameter | <p class="shorttext synchronized">any parameter documentation</p>
    "! @raising cx_any_exception | <p class="shorttext synchronized">exception documentation</p>
    "! @raising cx_obsolete_exception | <p class="shorttext synchronized">obsolete exception which was deleted</p>
    METHODS any_method
      IMPORTING iv_any_parameter   TYPE i
                iv_other_parameter TYPE i
      RAISING   cx_any_exception
                cx_other_exception.

    METHODS:
      "! other method documentation (not synchronized)
      "!
      "! @parameter iv_first_parameter | first parameter documentation
      "! @parameter iv_second_parameter | second parameter documentation
      "! @parameter ev_third_parameter |
      "! @raising cx_any_exception |
      "! @raising cx_other_exception | other exception documentation
      other_method
        IMPORTING iv_first_parameter  TYPE c
                  iv_second_parameter TYPE c
        EXPORTING ev_third_parameter  TYPE c
        RAISING   cx_any_exception
                  cx_other_exception,

      "! other method documentation (not synchronized)
      "!
      "! @parameter iv_obsolete_parameter | obsolete parameter which was deleted
      third_method,

      "! <p class="shorttext synchronized" lang="en">fourth method documentation (synchronized with lang)</p>
      fourth_method
        IMPORTING  iv_param_a TYPE i
                   iv_param_b TYPE i
        EXCEPTIONS any_exception.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/AbapDocParametersRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/AbapDocParametersTest.java)

