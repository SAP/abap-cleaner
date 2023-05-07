[<-- previous rule](AlignAssignmentsRule.md) | [overview](../rules.md) | [next rule -->](AlignClearFreeAndSortRule.md)

# Align keywords with second word of first line

Aligns ABAP keywords with the second word of the first line \(e.g. in READ TABLE ... WITH KEY ..., INSERT ... INTO TABLE ...\)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD align_with_second_word.
    READ TABLE lth_any_hash_table ASSIGNING <ls_row>
      WITH TABLE KEY item_id = <ls_any_field_symbol>-item_id.

    " if keywords in subsequent lines are just indented by 2 spaces, it is very difficult to see
    " where a block starts and ends
    LOOP AT mo_item_manager->get_all_items( )
      ASSIGNING FIELD-SYMBOL(<lo_item>).
      INSERT VALUE #( item_id       = <lo_item>->ms_data-item_id
                      item_category = if_any_interface=>cos_item_type-empty )
        INTO TABLE lth_any_hash_table.
    ENDLOOP.

    LOOP AT lts_any_sorted_table ASSIGNING FIELD-SYMBOL(<ls_row>)
    WHERE event_date = '20220330'.
      " do something
    ENDLOOP.

    " only keywords like 'WITH' and 'INTO' will be aligned, not identifiers like 'deferral_cat'
    READ TABLE lts_any_sorted_table
    WITH TABLE KEY item_type     = if_any_interface=>co_any_item_type
                   item_category = if_any_interface=>co_any_item_category
    INTO DATA(ls_struc).

    " this rule does NOT change the position of Boolean operators AND, OR, EQUIV
    DELETE lts_any_sorted_table
    WHERE item_key  < '20220030000101'
      AND item_type = 'ABCD'.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD align_with_second_word.
    READ TABLE lth_any_hash_table ASSIGNING <ls_row>
         WITH TABLE KEY item_id = <ls_any_field_symbol>-item_id.

    " if keywords in subsequent lines are just indented by 2 spaces, it is very difficult to see
    " where a block starts and ends
    LOOP AT mo_item_manager->get_all_items( )
         ASSIGNING FIELD-SYMBOL(<lo_item>).
      INSERT VALUE #( item_id       = <lo_item>->ms_data-item_id
                      item_category = if_any_interface=>cos_item_type-empty )
             INTO TABLE lth_any_hash_table.
    ENDLOOP.

    LOOP AT lts_any_sorted_table ASSIGNING FIELD-SYMBOL(<ls_row>)
         WHERE event_date = '20220330'.
      " do something
    ENDLOOP.

    " only keywords like 'WITH' and 'INTO' will be aligned, not identifiers like 'deferral_cat'
    READ TABLE lts_any_sorted_table
         WITH TABLE KEY item_type     = if_any_interface=>co_any_item_type
                        item_category = if_any_interface=>co_any_item_category
         INTO DATA(ls_struc).

    " this rule does NOT change the position of Boolean operators AND, OR, EQUIV
    DELETE lts_any_sorted_table
           WHERE item_key  < '20220030000101'
             AND item_type = 'ABCD'.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignWithSecondWordRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignWithSecondWordTest.java)

