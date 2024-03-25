[<-- previous rule](AssertParameterOrderRule.md) | [overview](../rules.md) | [next rule -->](CamelCaseNameRule.md)

# Convert upper and lower case

Changes ABAP keywords and identifiers to upper or lower case, depending on their context inside or outside the CLASS ... DEFINITION section.

## References

* [Clean ABAP Styleguide: Use the Pretty Printer before activating](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-the-pretty-printer-before-activating)
* [Clean ABAP Styleguide: Use your Pretty Printer team settings](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-your-pretty-printer-team-settings)
* [ABAP Keyword Documentation: Use uppercase for keywords and lowercase for operands](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlower_upper_case_guidl.htm)

## Options

* Auto-determine upper/lower case \[do not auto-determine\]
* Keywords in CLASS ... DEFINITION section \[upper case\]
* Identifiers in CLASS ... DEFINITION section \[lower case\]
* Keywords in all other places \[upper case\]
* Identifiers in all other places \[lower case\]
* Pragmas \[upper case\]
* Pragma parameters \[upper case\]
* \[X\] Keep camel case identifiers

## Examples


```ABAP

class CL_UPPER_AND_LOWER_CASE definition public create protected.
  public section.
    methods pretty_print_case
      importing !iv_change_camel_case type abap_bool.
  PRIVATE SECTION.
    DATA mv_change_definition TYPE ABAP_BOOL.
    methods INSERT ##SHADOW[insert].
endclass.

CLASS CL_UPPER_AND_LOWER_CASE IMPLEMENTATION.
  method pretty_print_case.
    constants lc_any_constant value 'abcde' ##no_text.

    DATA lv_counter type i.
    DATA lvMixedCaseIdentifier TYPE i.
    DATA(lo_inline) = get_object( ) ##needed.

    IF iv_count = abap_true AND io_inline IS NOT BOUND.
      loop at mts_data ASSIGNING FIELD-SYMBOL(<ls_data>).
        lv_counter += 1.
      endloop.
    ENDIF.

    SORT LT_ITEM BY FISCAL_YEAR ASCENDING
                    PERIOD      DESCENDING
                    ANY_FLAG    DESCENDING.
  ENDMETHOD.
ENDCLASS.
```

Resulting code:

```ABAP

CLASS cl_upper_and_lower_case DEFINITION PUBLIC CREATE PROTECTED.
  PUBLIC SECTION.
    METHODS pretty_print_case
      IMPORTING !iv_change_camel_case TYPE abap_bool.
  PRIVATE SECTION.
    DATA mv_change_definition TYPE abap_bool.
    METHODS insert ##SHADOW[INSERT].
ENDCLASS.

CLASS cl_upper_and_lower_case IMPLEMENTATION.
  METHOD pretty_print_case.
    CONSTANTS lc_any_constant VALUE 'abcde' ##NO_TEXT.

    DATA lv_counter TYPE i.
    DATA lvMixedCaseIdentifier TYPE i.
    DATA(lo_inline) = get_object( ) ##NEEDED.

    IF iv_count = abap_true AND io_inline IS NOT BOUND.
      LOOP AT mts_data ASSIGNING FIELD-SYMBOL(<ls_data>).
        lv_counter += 1.
      ENDLOOP.
    ENDIF.

    SORT lt_item BY fiscal_year ASCENDING
                    period      DESCENDING
                    any_flag    DESCENDING.
  ENDMETHOD.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/prettyprinter/UpperAndLowerCaseRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/prettyprinter/UpperAndLowerCaseTest.java)

