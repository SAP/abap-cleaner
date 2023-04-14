[<-- previous rule](AlignParametersRule.md) | [overview](../rules.md) | [next rule -->](AlignCondExpressionsRule.md)

# Align logical expressions

Aligns logical expressions, especially if they span multiple lines, to express operator priority in the layout.

## Options

* Align AND / OR / EQUIV with IF \[do not align\]
* Align AND / OR / EQUIV with ELSEIF \[do not align\]
* Align AND / OR / EQUIV with CHECK \[do not align\]
* Align AND / OR / EQUIV with WHILE \[do not align\]
* Align AND / OR / EQUIV with WHERE \[do not align\]
* \[X\] Right-align comparison operators / IS
* \[ \] Only align comparisons on same object
* Do not align if more than \[20\] inner spaces would be required

## Examples


```ABAP

  METHOD align_logical_expressions.
    CHECK is_buffer-item_id IS NOT INITIAL
       AND is_buffer-any_flag = abap_false
           AND is_buffer-other_flag = abap_false
                 AND is_buffer-was_changed = abap_true.

    CHECK line_exists( its_table[ 0 ] )    AND  its_table[ 0 ]-processed  =     abap_true
                OR line_exists( its_other_table[ 1 ] ) AND lines( its_other_table )   >     2 .

    IF a = abap_false AND b > 3 
         OR a = abap_true AND b <= 10.
      " do something
    ENDIF.

    IF  ( c IS NOT SUPPLIED 
       OR b IS INITIAL ) 
       AND ( d IS SUPPLIED 
       OR b IS NOT INITIAL ).
      " do something

    ELSEIF line_exists( its_table[ 0 ] ) 
       OR ( lines( its_table ) > 2
    AND line_exists( its_table[ 1 ] ) ).
      " do something
    ENDIF.

    WHILE ( a = abap_true      OR b > 3 AND ( c IS BOUND OR d IS INSTANCE OF cl_x ) )
       AND ( a = abap_false OR b <= 10 AND ( c_alt IS NOT BOUND OR e_alt IS INSTANCE OF cl_xyz ) )
          OR ( c IS NOT SUPPLIED OR b IS INITIAL )
       AND ( d IS SUPPLIED OR b IS NOT INITIAL )
       AND line_exists( its_table[ 0 ] )
            EQUIV     lines( its_table ) > 2
       AND line_exists( its_table[ 1 ] ).
      " do something
    ENDWHILE.

    IF mo_item->ms_data-item_id = if_any_interface=>co_any_item_id
    AND mo_item->ms_data-name = if_any_interface=>co_any_name
    AND ( ( lv_quantity >= 0 AND iv_total_quantity >= lv_quantity ) 
    OR ( lv_quantity < 0 AND iv_total_quantity <= lv_quantity ) ). 
      " do something
    ENDIF.

    LOOP AT mts_data ASSIGNING FIELD-SYMBOL(<ls_data>) USING KEY any_key_name
         WHERE is_valid = abap_true
             AND category = if_any_interface=>co_any_category
              AND name <> if_any_interface=>co_any_name
               AND item_id = if_any_interface=>co_any_item_id
                AND statistic = abap_false.
      " do something
    ENDLOOP.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD align_logical_expressions.
    CHECK     is_buffer-item_id     IS NOT INITIAL
          AND is_buffer-any_flag     = abap_false
          AND is_buffer-other_flag   = abap_false
          AND is_buffer-was_changed  = abap_true.

    CHECK    line_exists( its_table[ 0 ] )       AND its_table[ 0 ]-processed = abap_true
          OR line_exists( its_other_table[ 1 ] ) AND lines( its_other_table ) > 2 .

    IF    a = abap_false AND b  > 3
       OR a = abap_true  AND b <= 10.
      " do something
    ENDIF.

    IF     (    c IS NOT SUPPLIED
             OR b IS INITIAL )
       AND (    d IS SUPPLIED
             OR b IS NOT INITIAL ).
      " do something

    ELSEIF    line_exists( its_table[ 0 ] )
           OR (     lines( its_table ) > 2
                AND line_exists( its_table[ 1 ] ) ).
      " do something
    ENDIF.

    WHILE               ( a  = abap_true    OR b  > 3  AND ( c     IS BOUND     OR d     IS INSTANCE OF cl_x ) )
                    AND ( a  = abap_false   OR b <= 10 AND ( c_alt IS NOT BOUND OR e_alt IS INSTANCE OF cl_xyz ) )
                OR      ( c IS NOT SUPPLIED OR b IS INITIAL )
                    AND ( d IS SUPPLIED     OR b IS NOT INITIAL )
                    AND line_exists( its_table[ 0 ] )
          EQUIV     lines( its_table ) > 2
                AND line_exists( its_table[ 1 ] ).
      " do something
    ENDWHILE.

    IF     mo_item->ms_data-item_id = if_any_interface=>co_any_item_id
       AND mo_item->ms_data-name    = if_any_interface=>co_any_name
       AND (    ( lv_quantity >= 0 AND iv_total_quantity >= lv_quantity )
             OR ( lv_quantity  < 0 AND iv_total_quantity <= lv_quantity ) ).
      " do something
    ENDIF.

    LOOP AT mts_data ASSIGNING FIELD-SYMBOL(<ls_data>) USING KEY any_key_name
         WHERE     is_valid   = abap_true
               AND category   = if_any_interface=>co_any_category
               AND name      <> if_any_interface=>co_any_name
               AND item_id    = if_any_interface=>co_any_item_id
               AND statistic  = abap_false.
      " do something
    ENDLOOP.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignLogicalExpressionsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignLogicalExpressionsTest.java)

