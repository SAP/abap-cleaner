[<-- previous rule](AlignClearFreeAndSortRule.md) | [overview](../rules.md) | [next rule -->](AlignLogicalExpressionsRule.md)

# Align parameters and components

Aligns parameter assignments in method calls, as well as component assignments in VALUE expressions, table expressions, table keys etc.

## References

* [Clean ABAP Styleguide: Align parameters](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#align-parameters)
* [Clean ABAP Styleguide: Keep parameters behind the call](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#keep-parameters-behind-the-call)
* [Clean ABAP Styleguide: If you break, indent parameters under the call](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#if-you-break-indent-parameters-under-the-call)
* [Clean ABAP Styleguide: Line-break multiple parameters](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#line-break-multiple-parameters)
* [Clean ABAP Styleguide: Stick to a reasonable line length](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#stick-to-a-reasonable-line-length)
* [Clean ABAP Styleguide: Indent and snap to tab](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#indent-and-snap-to-tab)
* [Clean ABAP Styleguide: Indent in-line declarations like method calls](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#indent-in-line-declarations-like-method-calls)

## Options

* Maximum line length A \(normal\) \[120\] 
* Maximum line length B \(for tabular style\) \[160\] 
* Procedural call: continue behind the call for up to \[0\] parameters
* Functional call: continue behind the call for up to \[100\] parameters
* \[ \] Procedural call: put keywords \(EXPORTING etc.\) on own line
* \[ \] Functional call: put keywords \(EXPORTING etc.\) on own line
* \[X\] Align assignments
* \[X\] Align assignments across rows of table constructors
* Table rows: Keep multiple components on single line \[if maximum line length B is observed\]
* Allow line starts left of assignment operator \[only to keep maximum line length\]

## Examples


```ABAP

  METHOD align_parameters.
    lts_table = cl_any_class=>create_table(
      EXPORTING
        iv_contract_id = lo_contract1->ms_data-contract_id
        iv_contract_type = if_any_interface=>co_any_contract_type
        iv_item_type = lo_item->get_item_data( )-item_type ).

    " commented-out parameters are aligned, too:
    lts_other_table = cl_other_class=>create_table(
      EXPORTING
        iv_item_key = '12345'
        iv_category = 'ABC'
*        iv_size = 100'
        iv_name = 'ANY_NAME'
        iv_qty = 8 ).

    CALL METHOD procedural_call_example
      EXPORTING
        iv_contract_id = lo_contract1->ms_data-contract_id
        iv_item_key = '13579'
      IMPORTING
        ev_category = lv_any_category
        ev_item_type = lv_any_item_type
      CHANGING
        cv_qty = lv_quantity.

    ets_table = VALUE #( date = gc_any_date
                           id = gc_any_id

                           ( item_name = 'ANY'
*                      size = 'M'
                              quantity = 1 )
                             ( item_name = 'OTHER'
                                quantity = 2
                                reference_id = '12345' )
                                   ( item_name = 'THIRD'
                                  quantity = 3 ) ).

    ets_fulfillment = VALUE #( event_date = gc_any_event_date
                                amount = gc_any_amount
                               ( fulfillment_number = lc_fulfill_num_1  qty = lc_fulfill_qty_1 )
                                 ( fulfillment_number = lc_fulfill_num_2  qty = lc_fulfill_qty_2 )
                                     ( fulfillment_number = lc_fulfill_num_3  qty = lc_fulfill_qty_3 ) ).

    " tabular style improves readability, therefore some overlength may be tolerated here (max. line length B):
    mts_table = VALUE #( ( item_key = '20220030000101'  event_date = '20220301'  total_qty = '30'  qty_unit = 'DAY'  amount = '1000.00'  currency = 'EUR' )
                         ( item_key = '20220040000101'  event_date = '20220401'  total_qty = '30'  qty_unit = 'DAY'  amount = '1500.00'  currency = 'EUR' )
                         ( item_key = '20220050000101'  event_date = '20220501'  total_qty = '30'  qty_unit = 'DAY'  amount = '2000.00'  currency = 'EUR' ) ).

    READ TABLE lt_any_table_name ASSIGNING <ls_table_row> 
         WITH KEY field1 = ls_any_structure-field1
                  fld2 = ls_any_structure-fld2
                  long_field_name3 = ls_any_structure-long_field_name_3.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD align_parameters.
    lts_table = cl_any_class=>create_table( EXPORTING iv_contract_id   = lo_contract1->ms_data-contract_id
                                                      iv_contract_type = if_any_interface=>co_any_contract_type
                                                      iv_item_type     = lo_item->get_item_data( )-item_type ).

    " commented-out parameters are aligned, too:
    lts_other_table = cl_other_class=>create_table( EXPORTING iv_item_key = '12345'
                                                              iv_category = 'ABC'
*                                                              iv_size     = 100'
                                                              iv_name     = 'ANY_NAME'
                                                              iv_qty      = 8 ).

    CALL METHOD procedural_call_example
      EXPORTING iv_contract_id = lo_contract1->ms_data-contract_id
                iv_item_key    = '13579'
      IMPORTING ev_category    = lv_any_category
                ev_item_type   = lv_any_item_type
      CHANGING  cv_qty         = lv_quantity.

    ets_table = VALUE #( date = gc_any_date
                         id   = gc_any_id

                         ( item_name    = 'ANY'
*                           size         = 'M'
                           quantity     = 1 )
                         ( item_name    = 'OTHER'
                           quantity     = 2
                           reference_id = '12345' )
                         ( item_name    = 'THIRD'
                           quantity     = 3 ) ).

    ets_fulfillment = VALUE #( event_date = gc_any_event_date
                               amount     = gc_any_amount
                               ( fulfillment_number = lc_fulfill_num_1  qty = lc_fulfill_qty_1 )
                               ( fulfillment_number = lc_fulfill_num_2  qty = lc_fulfill_qty_2 )
                               ( fulfillment_number = lc_fulfill_num_3  qty = lc_fulfill_qty_3 ) ).

    " tabular style improves readability, therefore some overlength may be tolerated here (max. line length B):
    mts_table = VALUE #(
        ( item_key = '20220030000101'  event_date = '20220301'  total_qty = '30'  qty_unit = 'DAY'  amount = '1000.00'  currency = 'EUR' )
        ( item_key = '20220040000101'  event_date = '20220401'  total_qty = '30'  qty_unit = 'DAY'  amount = '1500.00'  currency = 'EUR' )
        ( item_key = '20220050000101'  event_date = '20220501'  total_qty = '30'  qty_unit = 'DAY'  amount = '2000.00'  currency = 'EUR' ) ).

    READ TABLE lt_any_table_name ASSIGNING <ls_table_row>
         WITH KEY field1           = ls_any_structure-field1
                  fld2             = ls_any_structure-fld2
                  long_field_name3 = ls_any_structure-long_field_name_3.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignParametersRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignParametersTest.java)

