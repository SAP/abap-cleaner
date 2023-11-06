[<-- previous rule](AssertClassRule.md) | [overview](../rules.md) | [next rule -->](UpperAndLowerCaseRule.md)

# Standardize assertion parameter order

Rearranges parameters in CL\_ABAP\_UNIT\_ASSERT calls to follow a standardized order for expected and actual values.

If an assert class for product code exists with signatures similar to CL\_ABAP\_UNIT\_ASSERT \(see rule 'Use assert class instead of ASSERT'\), calls to this class can be processed, too.

## Options

* Ensure order of parameters for expectation and actual value:
* Methods comparing values \(ASSERT\_EQUALS..., \_DIFFERS, \_THAT, \_CHAR...\) \[expectation first \(EXP\)\]
* Methods checking numbers, text and tables \(\_NUMBER..., \_TEXT\_..., \_TABLE\_...\) \[expectation first \(LOWER..UPPER, PATTERN, LINE\)\]
* Methods checking return code \(ASSERT\_SUBRC, \_RETURN\_CODE\): \[expectation first \(EXP\)\]
* Product code assert class name: \[\]

## Examples


```ABAP

  METHOD test_anything.
    " given
    prepare_test_case( ).

    " when
    call_method_under_test( ).

    " then
    " 1. ASSERT_EQUALS[_FLOAT], ASSERT_DIFFERS, ASSERT_CHAR_CP, ASSERT_CHAR_NP, ASSERT_THAT, ASSUME_THAT:
    " the signatures of these methods start with parameter ACT, but you may want calls to start with EXP:
    cl_abap_unit_assert=>assert_equals( act = ms_data-item_type
                                        exp = if_any_interface=>co_any_item_type ).

    cl_abap_unit_assert=>assert_equals( msg = 'unexpected value for component1!'
                                        exp = 'new value' " comment on exp
                                        act = lts_act_table[ 1 ]-component1 ).

    cl_abap_unit_assert=>assert_differs( act  = lo_atc_item_instance->ms_data-item_category
                                         exp  = if_any_interface=>co_any_item_category
                                         msg  = 'unexpected item category'
                                         quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_differs( exp = 3 act = lines( lts_act_table ) ).

    cl_abap_unit_assert=>assert_char_cp( act = lt_result[ 1 ] exp = |*;'-z2;*| ).

    " 2. ASSERT_NUMBER_BETWEEN, ASSERT_TEXT[_NOT]_MATCHES, ASSERT_TABLE[_NOT]_CONTAINS:
    " the signatures of these methods start with the parameters for the expectation (UPPER, LOWER;
    " PATTERN; LINE), followed by the actual value (NUMBER; TEXT; TABLE)
    cl_abap_unit_assert=>assert_number_between( " must be between 5 and 10 incl.
                                                number = lv_result_value
                                                upper  = 10
                                                lower  = 5 ).

    cl_abap_unit_assert=>assert_text_matches( text    = lv_act_message_text
                                              pattern = lv_exp_message_pattern ).

    cl_abap_unit_assert=>assert_table_not_contains( line  = ls_deleted_table_line
                                                    table = lt_act_table
                                                    msg   = 'deletion of line failed' ).

    " 3. ASSERT_SUBRC, ASSERT_RETURN_CODE, ASSUME_RETURN_CODE: parameter ACT has SY-SUBRC as its default value
    " these methods use SY-SUBRC as the default value for parameter SY-SUBRC; the signature starts with EXP
    cl_abap_unit_assert=>assert_subrc( quit = if_aunit_constants=>quit-no
                                       exp  = 4 ).

    cl_abap_unit_assert=>assert_return_code( exp = 4
                                             act = lv_return_code ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD test_anything.
    " given
    prepare_test_case( ).

    " when
    call_method_under_test( ).

    " then
    " 1. ASSERT_EQUALS[_FLOAT], ASSERT_DIFFERS, ASSERT_CHAR_CP, ASSERT_CHAR_NP, ASSERT_THAT, ASSUME_THAT:
    " the signatures of these methods start with parameter ACT, but you may want calls to start with EXP:
    cl_abap_unit_assert=>assert_equals( exp = if_any_interface=>co_any_item_type
                                        act = ms_data-item_type ).

    cl_abap_unit_assert=>assert_equals( exp = 'new value' " comment on exp
                                        act = lts_act_table[ 1 ]-component1
                                        msg = 'unexpected value for component1!' ).

    cl_abap_unit_assert=>assert_differs( exp  = if_any_interface=>co_any_item_category
                                         act  = lo_atc_item_instance->ms_data-item_category
                                         msg  = 'unexpected item category'
                                         quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_differs( exp = 3 act = lines( lts_act_table ) ).

    cl_abap_unit_assert=>assert_char_cp( exp = |*;'-z2;*| act = lt_result[ 1 ] ).

    " 2. ASSERT_NUMBER_BETWEEN, ASSERT_TEXT[_NOT]_MATCHES, ASSERT_TABLE[_NOT]_CONTAINS:
    " the signatures of these methods start with the parameters for the expectation (UPPER, LOWER;
    " PATTERN; LINE), followed by the actual value (NUMBER; TEXT; TABLE)
    cl_abap_unit_assert=>assert_number_between( " must be between 5 and 10 incl.
                                                lower  = 5
                                                upper  = 10
                                                number = lv_result_value ).

    cl_abap_unit_assert=>assert_text_matches( pattern = lv_exp_message_pattern
                                              text    = lv_act_message_text ).

    cl_abap_unit_assert=>assert_table_not_contains( line  = ls_deleted_table_line
                                                    table = lt_act_table
                                                    msg   = 'deletion of line failed' ).

    " 3. ASSERT_SUBRC, ASSERT_RETURN_CODE, ASSUME_RETURN_CODE: parameter ACT has SY-SUBRC as its default value
    " these methods use SY-SUBRC as the default value for parameter SY-SUBRC; the signature starts with EXP
    cl_abap_unit_assert=>assert_subrc( exp  = 4
                                       quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_return_code( exp = 4
                                             act = lv_return_code ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/AssertParameterOrderRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/AssertParameterOrderTest.java)

