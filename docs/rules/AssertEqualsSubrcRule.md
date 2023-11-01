[<-- previous rule](AssertEqualsBooleanRule.md) | [overview](../rules.md) | [next rule -->](AssertClassRule.md)

# Use assert\_subrc instead of assert\_equals

Replaces calls to cl\_abap\_unit\_assert=>assert\_equals with more concise calls to the dedicated method assert\_subrc, where applicable.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Use the right assert type](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-the-right-assert-type)

## Options

* \[X\] Remove 'exp = 0'

## Examples


```ABAP

  METHOD use_assert_subrc.
    cl_abap_unit_assert=>assert_equals( act = sy-subrc
                                        exp = 0 ).

    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = sy-subrc ).

    cl_abap_unit_assert=>assert_equals( act  = sy-subrc
                                        exp  = 0
                                        msg  = 'message'
                                        quit = lv_quit ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD use_assert_subrc.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_subrc( exp = 4 ).

    cl_abap_unit_assert=>assert_subrc( msg  = 'message'
                                       quit = lv_quit ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/AssertEqualsSubrcRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/AssertEqualsSubrcTest.java)

