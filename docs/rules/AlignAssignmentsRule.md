[<-- previous rule](AlignDeclarationsRule.md) | [overview](../rules.md) | [next rule -->](AlignWithSecondWordRule.md)

# Align assignments to the same object

Aligns assignments to the same object, e.g. to various components of the same structure.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Align assignments to the same object, but not to different ones](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#align-assignments-to-the-same-object-but-not-to-different-ones)

## Options

* \[X\] Align across empty lines
* \[X\] Align across comment lines

## Examples


```ABAP

  METHOD align_assignments_to_same_obj.
    ro_instance->mv_name = iv_name.
    ro_instance->mv_id = iv_id.
    ro_instance->mv_price = iv_price.

    ls_struc-component = 1.
    ls_struc-comp2 = 'a'.
    ls_struc-start_date+6(2) = 31.
    ls_struc-c3 = VALUE #( ).

    <ls_field_symbol>-component = 1.
    <ls_field_symbol>-comp2 = 'a'.
    <ls_field_symbol>-c3 = VALUE #( ).

    " alignment across comments and empty lines (depending on configuration):
    ls_struc-component += 1.
    "comment
    ls_struc-comp2 = 'a'.

    ls_struc-c3 = VALUE #( ).

    " comment
    ls_struc-comp4 -= 1.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD align_assignments_to_same_obj.
    ro_instance->mv_name  = iv_name.
    ro_instance->mv_id    = iv_id.
    ro_instance->mv_price = iv_price.

    ls_struc-component       = 1.
    ls_struc-comp2           = 'a'.
    ls_struc-start_date+6(2) = 31.
    ls_struc-c3              = VALUE #( ).

    <ls_field_symbol>-component = 1.
    <ls_field_symbol>-comp2     = 'a'.
    <ls_field_symbol>-c3        = VALUE #( ).

    " alignment across comments and empty lines (depending on configuration):
    ls_struc-component += 1.
    "comment
    ls_struc-comp2      = 'a'.

    ls_struc-c3         = VALUE #( ).

    " comment
    ls_struc-comp4     -= 1.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignAssignmentsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignAssignmentsTest.java)

