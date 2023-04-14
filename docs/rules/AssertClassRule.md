[<-- previous rule](AssertEqualsSubrcRule.md) | [overview](../rules.md) | [next rule -->](UpperAndLowerCaseRule.md)

# Use assert class instead of ASSERT

Replaces ASSERT statements \(in productive code\) with static calls to an assert class. Note that the Assert class name must be adjusted to the respective application.

## Options

* Assert class name: \[cx\_assert\]

## Examples


```ABAP

  METHOD use_productive_assert_class.
    ASSERT lo_instance IS BOUND.
    ASSERT is_any_structure-component IS NOT BOUND.

    ASSERT is_parameters-component_name IS INITIAL.
    ASSERT io_any_instance IS NOT INITIAL.

    ASSERT sy-subrc = 0. " defitem must exist
    ASSERT sy-subrc = 4.
    ASSERT sy-subrc = get_expected_subrc_value( param_a = 'abc' 
                                                param_b = 'def' ).

    ASSERT lv_was_initialized = abap_true.
    ASSERT mv_is_valid = abap_false.
    ASSERT line_exists( lts_table[ iv_param_a = 1 
                                   iv_param_b = 'abc' ] ) = abap_true.

    ASSERT ms_data-item_type = if_any_interface=>co_any_item_type.
    ASSERT lv_timestamp(7) = lts_table[ 1 ]-start_timestamp(7).

    ASSERT lo_any_item_instance->ms_data-item_category <> if_any_interface=>co_any_item_category.
    ASSERT sy-subrc <> 0.

    ASSERT 1 = 2.

    ASSERT <ls_any_field_symbol> IS ASSIGNED.
    ASSERT <ls_other_field_symbol> IS NOT ASSIGNED.
    ASSERT is_any_structure IS NOT INITIAL OR is_other_structure IS NOT INITIAL.
    ASSERT <ls_row>-item_key <= ms_parameters-last_item_key.
    ASSERT lv_quantity <= 100.
    ASSERT abs( <ls_any_field_symbol>-sum_quantity ) > 0.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD use_productive_assert_class.
    cx_assert=>assert_bound( lo_instance ).
    cx_assert=>assert_not_bound( is_any_structure-component ).

    cx_assert=>assert_initial( is_parameters-component_name ).
    cx_assert=>assert_not_initial( io_any_instance ).

    cx_assert=>assert_subrc( ). " defitem must exist
    cx_assert=>assert_subrc( 4 ).
    cx_assert=>assert_subrc( get_expected_subrc_value( param_a = 'abc'
                                                       param_b = 'def' ) ).

    cx_assert=>assert_true( lv_was_initialized ).
    cx_assert=>assert_false( mv_is_valid ).
    cx_assert=>assert_true( line_exists( lts_table[ iv_param_a = 1
                                                    iv_param_b = 'abc' ] ) ).

    cx_assert=>assert_equals( act = ms_data-item_type
                              exp = if_any_interface=>co_any_item_type ).
    cx_assert=>assert_equals( act = lv_timestamp(7)
                              exp = lts_table[ 1 ]-start_timestamp(7) ).

    cx_assert=>assert_differs( act = lo_any_item_instance->ms_data-item_category
                               exp = if_any_interface=>co_any_item_category ).
    cx_assert=>assert_differs( act = sy-subrc
                               exp = 0 ).

    cx_assert=>fail( ).

    cx_assert=>assert_true( xsdbool( <ls_any_field_symbol> IS ASSIGNED ) ).
    cx_assert=>assert_true( xsdbool( <ls_other_field_symbol> IS NOT ASSIGNED ) ).
    cx_assert=>assert_true( xsdbool( is_any_structure IS NOT INITIAL OR is_other_structure IS NOT INITIAL ) ).
    cx_assert=>assert_true( xsdbool( <ls_row>-item_key <= ms_parameters-last_item_key ) ).
    cx_assert=>assert_true( xsdbool( lv_quantity <= 100 ) ).
    cx_assert=>assert_true( xsdbool( abs( <ls_any_field_symbol>-sum_quantity ) > 0 ) ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/AssertClassRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/AssertClassTest.java)

