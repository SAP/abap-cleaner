[<-- previous rule](ClosingBracketsPositionRule.md) | [overview](../rules.md) | [next rule -->](SpaceAroundCommentSignRule.md)

# Remove space before commas and period

Removes spaces before chain commas and before the period at the end of a statement.

## References

* [Clean ABAP Styleguide: Condense your code](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#condense-your-code)

## Options

* \[X\] Remove space before period
* \[X\] Remove space before chain commas
* \[X\] Execute on CLASS ... DEFINITION sections, too

## Examples


```ABAP

CLASS any_class DEFINITION .
  PUBLIC SECTION .
    DATA: mv_any_data TYPE i   ,
          mv_other_data TYPE string . 

    METHODS space_before_period_or_comma .
ENDCLASS.


CLASS any_class IMPLEMENTATION.
  METHOD space_before_period_or_comma .
    DATA: lo_object TYPE cl_any_class ##NEEDED
          , lo_other_object TYPE cl_other_class .

    CLEAR:
      ev_any_value  ,
      ev_other_value " comment
      , ev_third_value
    .

    TRY .
        any_operation( ) .
      CATCH cx_any .
    ENDTRY .

    lv_value = 42 " comment
    .
  ENDMETHOD.
ENDCLASS.
```

Resulting code:

```ABAP

CLASS any_class DEFINITION.
  PUBLIC SECTION.
    DATA: mv_any_data TYPE i,
          mv_other_data TYPE string.

    METHODS space_before_period_or_comma.
ENDCLASS.


CLASS any_class IMPLEMENTATION.
  METHOD space_before_period_or_comma.
    DATA: lo_object TYPE cl_any_class ##NEEDED,
          lo_other_object TYPE cl_other_class.

    CLEAR:
      ev_any_value,
      ev_other_value, " comment
      ev_third_value.

    TRY.
        any_operation( ).
      CATCH cx_any.
    ENDTRY.

    lv_value = 42. " comment
  ENDMETHOD.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/spaces/SpaceBeforePeriodRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/spaces/SpaceBeforePeriodTest.java)

