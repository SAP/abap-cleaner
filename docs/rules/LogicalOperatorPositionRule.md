[<-- previous rule](NotIsRule.md) | [overview](../rules.md) | [next rule -->](NeedlessParenthesesRule.md)

# Move AND/OR etc. from line end to next line start

Moves Boolean operators \(AND, OR, EQUIV\) and keywords starting the logical expression \(WHERE, UNTIL, WHILE\) from the end of a line to the start of the next line.

After moving Boolean operators, the expression is aligned according to the settings of the rule 'Align logical expressions' \(even if the rule is otherwise deactivated\).

## Options

* \[X\] Move keywords \(WHERE, UNTIL, WHILE\)
* \[X\] Move Boolean operators \(AND, OR, EQUIV\)

## Examples


```ABAP

  METHOD move_bool_ops_to_next_line.
    IF ls_item_data-category = if_any_interface=>co_any_category OR
       ls_item_data-start_date IS NOT INITIAL OR
       ls_item_data-end_date IS INITIAL OR
       ls_item_data-processing_method = if_other_interface=>co_any_processing_method.

    ELSEIF  ( c IS NOT SUPPLIED OR
              b IS INITIAL ) AND
            ( d IS SUPPLIED OR
              b IS NOT INITIAL ).

    ENDIF.

    " with WHERE at line end, 'component = 1.' looks like an assignment
    LOOP AT lts_any_table ASSIGNING FIELD-SYMBOL(<ls_any>) WHERE
      component = 1.
      <ls_any>-component = 2.
    ENDLOOP.

    " proper alignment helps to understand the logic and to check whether it is correct
    DELETE lts_other_table WHERE
           status = 'a' OR
           status = 'b' AND
           obsolete = abap_true.

    " if WHERE is at line end, the logical expression looks like a table line
    rt_result = VALUE #( FOR ls_struc IN lt_table WHERE
                         ( status = 'a' )
                         ( ls_struc ) ).

    lt_other_result = FILTER #( lt_any_table USING KEY any_key WHERE
                                active = abap_true AND used = abap_true ).

    lv_sum = REDUCE i( INIT s = 0
                       FOR i = 1 UNTIL
                       i = 10 OR
                       i >= iv_max
                       NEXT s += i ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD move_bool_ops_to_next_line.
    IF    ls_item_data-category           = if_any_interface=>co_any_category
       OR ls_item_data-start_date        IS NOT INITIAL
       OR ls_item_data-end_date          IS INITIAL
       OR ls_item_data-processing_method  = if_other_interface=>co_any_processing_method.

    ELSEIF     (    c IS NOT SUPPLIED
                 OR b IS INITIAL )
           AND (    d IS SUPPLIED
                 OR b IS NOT INITIAL ).

    ENDIF.

    " with WHERE at line end, 'component = 1.' looks like an assignment
    LOOP AT lts_any_table ASSIGNING FIELD-SYMBOL(<ls_any>)
      WHERE component = 1.
      <ls_any>-component = 2.
    ENDLOOP.

    " proper alignment helps to understand the logic and to check whether it is correct
    DELETE lts_other_table
           WHERE    status = 'a'
                 OR     status   = 'b'
                    AND obsolete = abap_true.

    " if WHERE is at line end, the logical expression looks like a table line
    rt_result = VALUE #( FOR ls_struc IN lt_table
                         WHERE ( status = 'a' )
                         ( ls_struc ) ).

    lt_other_result = FILTER #( lt_any_table USING KEY any_key
                                WHERE active = abap_true AND used = abap_true ).

    lv_sum = REDUCE i( INIT s = 0
                       FOR i = 1
                       UNTIL    i  = 10
                             OR i >= iv_max
                       NEXT s += i ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/LogicalOperatorPositionRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/LogicalOperatorPositionTest.java)

