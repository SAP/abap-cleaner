[<-- previous rule](LogicalOperatorPositionRule.md) | [overview](../rules.md) | [next rule -->](EmptyCommandRule.md)

# Remove needless parentheses

Removes needless parentheses from logical expressions.

If parentheses are removed, 'Align logical expressions' is executed, even if it is otherwise deactivated.

## References

* [ABAP Keyword Documentation: log\_exp - Boolean Operators and Parentheses](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlogexp_boole.htm)
* [ABAP Keyword Documentation: log\_exp - Logical Expressions](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlogexp.htm)

## Options

* \[X\] Remove needless parentheses around the entire logical expression
* \[ \] Remove needless parentheses around relational expressions \(i.e. comparisons or predicates\)
* \[ \] Remove needless parentheses from OR  \( ... AND ... \)
* \[ \] Remove needless parentheses from AND \( ... AND ... \) /  OR  \( ... OR ... \)
* CAUTION: While the logic remains unchanged, readability may suffer in some cases.

## Examples


```ABAP

  METHOD remove_needless_parentheses.
    " in ABAP, there is no need to put parentheses around entire logical expressions,
    " with the exception of table iterations with 'FOR ... IN ... WHERE ( log_expr )' ...:
    CHECK ( sy-subrc <> 0 ).
    CHECK ( lt_any_table IS NOT INITIAL ).
    CHECK ( lt_any_table IS REQUESTED OR lt_other_table IS REQUESTED ).

    WHILE ( lv_continue = abap_true ).
      IF ( prepare_next_item( ) < 0 ).
        lv_continue = abap_false.
      ELSEIF ( iv_any_value >= 0 AND iv_other_value <= 0 ).
        process_item( ).
      ENDIF.
    ENDWHILE.

    " relational expressions (i.e. comparisons and predicates) do not need parentheses around them, either,
    " because they are evaluated before the Boolean operators EQUIV / OR / AND join them, or NOT negates them:
    CHECK     ( lv_any_value <> lv_other_value )
          AND ( lv_any_value <> lv_third_value ).

    lv_result = xsdbool( ( lv_any_value IS INITIAL ) AND ( lt_any_table IS NOT INITIAL ) ).

    IF    ( iv_state = lc_invalid_state )
       OR ( iv_state = lc_obsolete_state ).
      RETURN.
    ENDIF.

    " from lowest to highest, operator precedence is: EQUIV < OR < AND < NOT < comparison / predicate;
    " therefore, no parentheses are needed in the frequent case of OR ( ... AND ... ).
    " It usually helps readability to start a new line for every OR in such cases:
    IF    lv_event_year < lv_final_year
       OR (     lv_event_year  = lv_final_year
            AND lv_event_month < lv_final_month ).
      process_past_event( ). " the event happened before the final year / month

    ELSEIF    ( lv_any_amount < 0 AND lv_other_amount > 0 )
           OR ( lv_any_amount > 0 AND lv_other_amount < 0 ).
      process_different_signs( ).

    ELSEIF ( sy-subrc <> 0 OR ( sy-subrc = 0 AND lv_any_value IS INITIAL ) OR lv_stop = abap_true ).
      RETURN.
    ENDIF.

    LOOP AT lt_any_table INTO DATA(ls_struc)
         WHERE    ( comp1 IS INITIAL     AND comp2 = 'A' )
               OR ( comp1 IS NOT INITIAL AND comp2 = 'B' ).
    ENDLOOP.

    " parentheses must NEVER be removed from AND ( ... OR ... ), as that would completely change the logic:
    IF iv_check_year_range = abap_true AND ( lv_year < iv_min_year OR lv_year > iv_max_year ).
      RETURN.
    ENDIF.

    " if all is AND (or all is OR), parentheses can be removed as well, evaluation order stays the same;
    " however, the parentheses might serve readability by grouping similar things together:
    IF    (    lt_table1  IS INITIAL
            OR lt_table2  IS INITIAL )
       OR (    lv_amount1 <= 0
            OR lv_amount2 <= 0 ).
      RETURN.

    ELSEIF     ( lv_year  >= iv_min_year  AND lv_year  <= iv_max_year )
           AND ( iv_count >= iv_min_count AND iv_count <= iv_max_count ).
      process_match( ).
    ENDIF.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD remove_needless_parentheses.
    " in ABAP, there is no need to put parentheses around entire logical expressions,
    " with the exception of table iterations with 'FOR ... IN ... WHERE ( log_expr )' ...:
    CHECK sy-subrc <> 0.
    CHECK lt_any_table IS NOT INITIAL.
    CHECK lt_any_table IS REQUESTED OR lt_other_table IS REQUESTED.

    WHILE lv_continue = abap_true.
      IF prepare_next_item( ) < 0.
        lv_continue = abap_false.
      ELSEIF iv_any_value >= 0 AND iv_other_value <= 0.
        process_item( ).
      ENDIF.
    ENDWHILE.

    " relational expressions (i.e. comparisons and predicates) do not need parentheses around them, either,
    " because they are evaluated before the Boolean operators EQUIV / OR / AND join them, or NOT negates them:
    CHECK     ( lv_any_value <> lv_other_value )
          AND ( lv_any_value <> lv_third_value ).

    lv_result = xsdbool( ( lv_any_value IS INITIAL ) AND ( lt_any_table IS NOT INITIAL ) ).

    IF    ( iv_state = lc_invalid_state )
       OR ( iv_state = lc_obsolete_state ).
      RETURN.
    ENDIF.

    " from lowest to highest, operator precedence is: EQUIV < OR < AND < NOT < comparison / predicate;
    " therefore, no parentheses are needed in the frequent case of OR ( ... AND ... ).
    " It usually helps readability to start a new line for every OR in such cases:
    IF    lv_event_year < lv_final_year
       OR (     lv_event_year  = lv_final_year
            AND lv_event_month < lv_final_month ).
      process_past_event( ). " the event happened before the final year / month

    ELSEIF    ( lv_any_amount < 0 AND lv_other_amount > 0 )
           OR ( lv_any_amount > 0 AND lv_other_amount < 0 ).
      process_different_signs( ).

    ELSEIF sy-subrc <> 0 OR ( sy-subrc = 0 AND lv_any_value IS INITIAL ) OR lv_stop = abap_true.
      RETURN.
    ENDIF.

    LOOP AT lt_any_table INTO DATA(ls_struc)
         WHERE    ( comp1 IS INITIAL     AND comp2 = 'A' )
               OR ( comp1 IS NOT INITIAL AND comp2 = 'B' ).
    ENDLOOP.

    " parentheses must NEVER be removed from AND ( ... OR ... ), as that would completely change the logic:
    IF iv_check_year_range = abap_true AND ( lv_year < iv_min_year OR lv_year > iv_max_year ).
      RETURN.
    ENDIF.

    " if all is AND (or all is OR), parentheses can be removed as well, evaluation order stays the same;
    " however, the parentheses might serve readability by grouping similar things together:
    IF    (    lt_table1  IS INITIAL
            OR lt_table2  IS INITIAL )
       OR (    lv_amount1 <= 0
            OR lv_amount2 <= 0 ).
      RETURN.

    ELSEIF     ( lv_year  >= iv_min_year  AND lv_year  <= iv_max_year )
           AND ( iv_count >= iv_min_count AND iv_count <= iv_max_count ).
      process_match( ).
    ENDIF.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/NeedlessParenthesesRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/NeedlessParenthesesTest.java)

