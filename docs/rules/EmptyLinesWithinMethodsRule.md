<-- previous rule | [overview](../rules.md) | [next rule -->](EmptyLinesOutsideMethodsRule.md)

# Standardize empty lines within methods

Restricts the number of consecutive empty lines within methods, and adds an empty line between declarations and the first executable statement \(or the comments preceding it\).

For function modules, additional empty lines are kept at the beginning to align with ADT and SE37 behavior.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Add a single blank line to separate things, but not more](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#add-a-single-blank-line-to-separate-things-but-not-more)
* [Clean ABAP Styleguide: Don't obsess with separating blank lines](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-obsess-with-separating-blank-lines)

## Options

* Max. empty lines at method start: \[0\] 
* Max. empty lines within methods: \[1\] 
* Max. empty lines at method end: \[0\] 
* \[X\] Insert empty line between declarations and first executable statement

## Examples


```ABAP

  METHOD empty_lines_within_methods.

    DATA lv_any_integer TYPE i.
    DATA lv_any_string  TYPE string.
    do_something( ).


    do_something_else( ).


    finish_doing_something( ).

  ENDMETHOD.


  METHOD empty_lines.
    FIELD-SYMBOLS <ls_data> TYPE any_type.
    " comment above executable statement
    DATA(lo_utility) = cl_factory=>get( )->get_utility( ).

  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD empty_lines_within_methods.
    DATA lv_any_integer TYPE i.
    DATA lv_any_string  TYPE string.

    do_something( ).

    do_something_else( ).

    finish_doing_something( ).
  ENDMETHOD.


  METHOD empty_lines.
    FIELD-SYMBOLS <ls_data> TYPE any_type.

    " comment above executable statement
    DATA(lo_utility) = cl_factory=>get( )->get_utility( ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/emptylines/EmptyLinesWithinMethodsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/emptylines/EmptyLinesWithinMethodsTest.java)

