[<-- previous rule](NotIsRule.md) | [overview](../rules.md) | [next rule -->](EmptyCommandRule.md)

# Move AND/OR from line end to next line start

Moves boolean operators \(AND, OR, EQUIV\) from the end of a line to the start of the next line.
The expression is then aligned according to the settings of the rule 'Align logical expressions' \(even if the rule is otherwise deactivated\).

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD move_bool_ops_to_next_line.
    IF ls_item_data-category = if_any_interface=>co_any_category OR
       ls_item_data-start_date IS NOT INITIAL OR
       ls_item_data-end_date IS INITIAL OR
       ls_item_data-processing_method = if_other_interface=>co_any_processing_method.
      " do something
    ENDIF.

    IF  ( c IS NOT SUPPLIED OR
          b IS INITIAL ) AND
        ( d IS SUPPLIED OR
          b IS NOT INITIAL ).
      " do something
    ENDIF.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD move_bool_ops_to_next_line.
    IF    ls_item_data-category           = if_any_interface=>co_any_category
       OR ls_item_data-start_date        IS NOT INITIAL
       OR ls_item_data-end_date          IS INITIAL
       OR ls_item_data-processing_method  = if_other_interface=>co_any_processing_method.
      " do something
    ENDIF.

    IF     (    c IS NOT SUPPLIED
             OR b IS INITIAL )
       AND (    d IS SUPPLIED
             OR b IS NOT INITIAL ).
      " do something
    ENDIF.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/LogicalOperatorPositionRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/LogicalOperatorPositionTest.java)

