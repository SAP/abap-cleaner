[<-- previous rule](AlignSelectFromRule.md) | [overview](../rules.md) | [next rule -->](AlignParametersRule.md)

# Align SELECT lists

Aligns the field lists after SELECT, FIELDS, ORDER BY, GROUP BY, and INTO \( ... \) of the ABAP SQL statements SELECT, WITH, and OPEN CURSOR.

GROUPING SETS \( ... \) is kept unchanged.

## Options

* Maximum line length \[120\] 
* Select lists with complex fields: \[multi-line\]
* Select lists with simple fields only: \[derive from majority\]
* GROUP BY lists with complex fields: \[multi-line\]
* GROUP BY lists with simple fields only: \[derive from majority\]
* ORDER BY lists with complex fields: \[multi-line\]
* ORDER BY lists with simple fields only: \[derive from majority\]
* INTO \(...\) list: \[derive from majority\]
* \[X\] Consider 'tabalias~col' as complex
* \[X\] Align AS in multi-line select list
* \[ \] Align ASCENDING, DESCENDING etc. in multi-line ORDER BY list

## Examples


```ABAP

  METHOD align_select_lists.
    " when this rule is executed, the position of the keywords FROM, FIELDS, WHERE etc.
    " was already determined by the rule 'Align SELECT clauses';
    " this rule now aligns the field lists after SELECT, FIELDS, ORDER BY, GROUP BY, and INTO ( ... ):
    SELECT first_column,
      second_column,
      third_column, fourth_column,
      fifth_column, sixth_column,
*  seventh_column
      eigth_column
      FROM any_dtab
      ORDER BY seventh_column ASCENDING NULLS FIRST, first_column DESCENDING NULLS LAST, second_column DESCENDING
      INTO TABLE @DATA(lt_any_table).

    SELECT t1~any_col AS any_alias, t1~other_col AS other_alias,
       t2~third_col,
       SUM( fourth_col ) AS fourth_col, SUM( fifth_col ) AS fifth_col
      FROM any_dtab AS t1
           INNER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col
      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table ##too_many_itab_fields
      WHERE t1~any_col    = @ms_any_struc-any_comp
        AND t2~third_col IN @lt_other_table
      GROUP BY t1~any_col,
      t1~other_col,
      t2~third_col,
      fourth_col, fifth_col.

    SELECT any_col, other_col, third_col, fourth_col,
      fifth_col, sixth_column_with_long_name, seventh_column_with_long_name
      FROM any_dtab
      WHERE any_col   IN @ir_any_ref
        AND other_col IN @ir_other_ref
      ORDER BY fifth_col,
             any_col,
              other_col
      INTO TABLE @DATA(lt_any_table).

    SELECT SINGLE t1~any_col t2~other_col t1~third_col t2~fourth_col
      FROM any_dtab
           INNER JOIN fifth_col ON t2~col_6 = t1~col_6
      WHERE col_7 = @is_any_struc-any_comp
        AND col_8 = @is_any_struc-other_comp
      INTO ( @lv_any_value, @lv_other_value,
      @DATA(lv_third_value),
     @FINAL(lv_fourth_value) ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD align_select_lists.
    " when this rule is executed, the position of the keywords FROM, FIELDS, WHERE etc.
    " was already determined by the rule 'Align SELECT clauses';
    " this rule now aligns the field lists after SELECT, FIELDS, ORDER BY, GROUP BY, and INTO ( ... ):
    SELECT first_column,
           second_column,
           third_column,
           fourth_column,
           fifth_column,
           sixth_column,
*           seventh_column
           eigth_column
      FROM any_dtab
      ORDER BY seventh_column ASCENDING NULLS FIRST,
               first_column DESCENDING NULLS LAST,
               second_column DESCENDING
      INTO TABLE @DATA(lt_any_table).

    SELECT t1~any_col        AS any_alias,
           t1~other_col      AS other_alias,
           t2~third_col,
           SUM( fourth_col ) AS fourth_col,
           SUM( fifth_col )  AS fifth_col
      FROM any_dtab AS t1
           INNER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col
      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table ##too_many_itab_fields
      WHERE t1~any_col    = @ms_any_struc-any_comp
        AND t2~third_col IN @lt_other_table
      GROUP BY t1~any_col,
               t1~other_col,
               t2~third_col,
               fourth_col,
               fifth_col.

    SELECT any_col, other_col, third_col, fourth_col, fifth_col, sixth_column_with_long_name,
           seventh_column_with_long_name
      FROM any_dtab
      WHERE any_col   IN @ir_any_ref
        AND other_col IN @ir_other_ref
      ORDER BY fifth_col,
               any_col,
               other_col
      INTO TABLE @DATA(lt_any_table).

    SELECT SINGLE t1~any_col
                  t2~other_col
                  t1~third_col
                  t2~fourth_col
      FROM any_dtab
           INNER JOIN fifth_col ON t2~col_6 = t1~col_6
      WHERE col_7 = @is_any_struc-any_comp
        AND col_8 = @is_any_struc-other_comp
      INTO ( @lv_any_value,
             @lv_other_value,
             @DATA(lv_third_value),
             @FINAL(lv_fourth_value) ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignSelectListsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignSelectListsTest.java)

