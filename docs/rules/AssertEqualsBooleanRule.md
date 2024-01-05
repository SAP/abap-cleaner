[<-- previous rule](TranslateRule.md) | [overview](../rules.md) | [next rule -->](AssertEqualsSubrcRule.md)

# Use assert\_true and assert\_false

Replaces calls to cl\_abap\_unit\_assert=>assert\_equals with more concise calls to the dedicated methods assert\_true and assert\_false, where applicable.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Use the right assert type](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-the-right-assert-type)

## Options

* \[X\] Remove 'act =' if only this parameter is passed

## Examples


```ABAP

  METHOD use_assert_true_and_false.
    cl_abap_unit_assert=>assert_equals( act = mo_any_instance->is_valid( )
                                        exp = abap_true ).

    cl_abap_unit_assert=>assert_equals( exp = abap_false
                                        act = lv_value ).

    cl_abap_unit_assert=>assert_equals( act  = lv_value
                                        exp  = abap_true
                                        msg  = 'message' 
                                        quit = lv_quit ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD use_assert_true_and_false.
    cl_abap_unit_assert=>assert_true( mo_any_instance->is_valid( ) ).

    cl_abap_unit_assert=>assert_false( lv_value ).

    cl_abap_unit_assert=>assert_true( act  = lv_value
                                      msg  = 'message'
                                      quit = lv_quit ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/AssertEqualsBooleanRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/AssertEqualsBooleanTest.java)

