[<-- previous rule](IndentRule.md) | [overview](../rules.md) | [next rule -->](AlignMethodsDeclarationRule.md)

# Align ABAP Doc

Aligns documentation of parameters and exceptions in ABAP Doc.

## Options

* \[X\] Align across empty ABAP Doc lines
* \[X\] Align across non-empty ABAP Doc lines

## Examples


```ABAP

CLASS cl_any_class DEFINITION FINAL.
  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Any method description</p>
    "!
    "! @parameter iv_any_param | <p class="shorttext synchronized" lang="en">any parameter</p>
    "! @parameter iv_other_param | <p class="shorttext synchronized" lang="en">other parameter</p>
    "! @parameter ev_param_with_long_name | <p class="shorttext synchronized" lang="en">parameter with a long name</p>
    "! @parameter et_any_table | <p class="shorttext synchronized" lang="en">any result table</p>
    "! @raising cx_any_exception | <p class="shorttext synchronized" lang="en">any exception</p>
    METHODS any_method
      IMPORTING !iv_any_param            TYPE i
                !iv_other_param          TYPE string
      EXPORTING !ev_param_with_long_name TYPE i
                !et_any_table            TYPE ty_tt_table
      RAISING   !cx_any_exception.

    "! <p class="shorttext synchronized" lang="en">Other method description</p>
    "!
    "! @parameter iv_any_param | <p class="shorttext synchronized" lang="en">any parameter</p>
    "! @parameter iv_other_parameter | <p class="shorttext synchronized" lang="en">other parameter</p>
    "!
    "! @raising cx_any_exception | <p class="shorttext synchronized" lang="en">any exception</p>
    METHODS other_method
      IMPORTING !iv_any_param   TYPE i
                !iv_other_param TYPE string
      RAISING   !cx_any_exception.

    "! <p class="shorttext synchronized" lang="en">Third method description</p>
    "!
    "! @parameter iv_any_param | <p class="shorttext synchronized" lang="en">any parameter</p>
    "! Further description of any parameter. This text is not synchronized, but it is displayed in ADT.
    "! @parameter iv_other_parameter | <p class="shorttext synchronized" lang="en">other parameter</p>
    "! Further description of other parameter.
    "! @raising cx_any_exception | <p class="shorttext synchronized" lang="en">any exception</p>
    "! Detailed description of the exception.
    METHODS third_method
      IMPORTING !iv_any_param   TYPE i
                !iv_other_param TYPE string
      RAISING   !cx_any_exception.
ENDCLASS.
```

Resulting code:

```ABAP

CLASS cl_any_class DEFINITION FINAL.
  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Any method description</p>
    "!
    "! @parameter iv_any_param            | <p class="shorttext synchronized" lang="en">any parameter</p>
    "! @parameter iv_other_param          | <p class="shorttext synchronized" lang="en">other parameter</p>
    "! @parameter ev_param_with_long_name | <p class="shorttext synchronized" lang="en">parameter with a long name</p>
    "! @parameter et_any_table            | <p class="shorttext synchronized" lang="en">any result table</p>
    "! @raising   cx_any_exception        | <p class="shorttext synchronized" lang="en">any exception</p>
    METHODS any_method
      IMPORTING !iv_any_param            TYPE i
                !iv_other_param          TYPE string
      EXPORTING !ev_param_with_long_name TYPE i
                !et_any_table            TYPE ty_tt_table
      RAISING   !cx_any_exception.

    "! <p class="shorttext synchronized" lang="en">Other method description</p>
    "!
    "! @parameter iv_any_param       | <p class="shorttext synchronized" lang="en">any parameter</p>
    "! @parameter iv_other_parameter | <p class="shorttext synchronized" lang="en">other parameter</p>
    "!
    "! @raising   cx_any_exception   | <p class="shorttext synchronized" lang="en">any exception</p>
    METHODS other_method
      IMPORTING !iv_any_param   TYPE i
                !iv_other_param TYPE string
      RAISING   !cx_any_exception.

    "! <p class="shorttext synchronized" lang="en">Third method description</p>
    "!
    "! @parameter iv_any_param       | <p class="shorttext synchronized" lang="en">any parameter</p>
    "! Further description of any parameter. This text is not synchronized, but it is displayed in ADT.
    "! @parameter iv_other_parameter | <p class="shorttext synchronized" lang="en">other parameter</p>
    "! Further description of other parameter.
    "! @raising   cx_any_exception   | <p class="shorttext synchronized" lang="en">any exception</p>
    "! Detailed description of the exception.
    METHODS third_method
      IMPORTING !iv_any_param   TYPE i
                !iv_other_param TYPE string
      RAISING   !cx_any_exception.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignAbapDocRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignAbapDocTest.java)

