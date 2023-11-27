[<-- previous rule](AlignClearFreeAndSortRule.md) | [overview](../rules.md) | [next rule -->](AlignSelectFromRule.md)

# Align SELECT clauses

Aligns the keywords SELECT, FROM, FIELDS, WHERE, GROUP BY etc. in the \(main- or sub-\)query clauses of the ABAP SQL statements SELECT, WITH, and OPEN CURSOR.

Field lists, joins and logical expressions in the SELECT clauses are aligned by dedicated rules: 'Align SELECT ... FROM ... JOIN', 'Align SELECT lists' and 'Align logical expressions'.

## Options

* Maximum line length for one-liners: \[120\] 
* Mainquery one-liners: \[keep existing\]
* Subquery one-liners: \[keep existing\]
* Put simple FROM behind SELECT list with up to \[30\] chars width
* Put simple INTO behind SELECT list with up to \[30\] chars width \(non-strict mode\)
* \[X\] Always start new line for FROM with joins
* Indent of clause keywords: \[add 2\]
* Indent of UNION / INTERSECT / EXCEPT: \[add 0\]
* Position of next SELECT after UNION etc.: \[below UNION etc.\]
* Indent of final INTO after UNION etc.: \[add 0\]

## Examples


```ABAP

  METHOD align_select_clauses.
    SELECT SINGLE any_col INTO lv_any_value FROM any_dtab WHERE other_col = gc_any_constant.

    " one-liners can be created from short SELECT statements that fit into one line; otherwise,
    " if configured, FROM (or INTO) can be moved behind a short and simple SELECT list
    SELECT any_col
    FROM any_dtab
    INTO TABLE lt_any_table.

    SELECT any_col, other_col, third_col
    INTO TABLE lt_any_table
    FROM any_dtab
    WHERE any_col < 10.

    SELECT any_col, other_col, third_col FROM any_dtab APPENDING CORRESPONDING FIELDS OF TABLE @lt_any_table
           WHERE other_col IN ( SELECT other_col FROM other_dtab WHERE fourth_col = @lv_any_value )
             AND third_col IN ( SELECT third_col FROM third_dtab WHERE fifth_col > @lv_other_value ). "#EC CI_BUFFSUBQ.

    SELECT *
           FROM any_dtab
         WHERE any_col = @lv_any_value
            INTO CORRESPONDING FIELDS OF TABLE NEW @lv_other_value.

    SELECT any_col
      FROM any_dtab
        WHERE any_col IN ( SELECT any_col
          FROM other_dtab
         WHERE other_col = 'X' )
          INTO CORRESPONDING FIELDS OF @ls_any_struc. "#EC CI_BUFFSUBQ
    ENDSELECT.

    " the alignment of joins can be configured with rule 'Align SELECT ... FROM ... JOIN'
    SELECT FROM any_dtab AS t1
                INNER JOIN other_dtab AS t2 ON  t1~any_col   = t2~any_col
                                            AND t1~other_col = t2~other_col
                LEFT OUTER JOIN third_dtab AS t3 ON t1~third_col = t3~third_col
      FIELDS (lt_fields)
       GROUP BY (lt_group)
             INTO CORRESPONDING FIELDS OF TABLE @lts_any_table.

    " UNION, INTERSECT and EXCEPT combine the result sets of multiple SELECTs:
    SELECT a AS c1, b AS c2, c AS c3
           FROM any_dtab
           UNION DISTINCT
    SELECT d AS c1, e AS c2, f AS c3
          FROM other_dtab
      UNION DISTINCT
        SELECT g AS c1, h AS c2, i AS c3
          FROM third_dtab
            INTO TABLE @FINAL(lt_distinct_result).

    " the final INTO ... clause belongs to the whole statement, not to the last SELECT!
    SELECT a AS c1, b AS c2, c AS c3, d AS c4
            FROM any_dtab
    INTERSECT ( SELECT d AS c1, e AS c2, f AS c3, g AS c4
        FROM other_dtab
    UNION
    SELECT i AS c1, j AS c2, k AS c3, l AS c4
               FROM third_dtab )
               INTO TABLE @FINAL(lt_result).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD align_select_clauses.
    SELECT SINGLE any_col INTO lv_any_value FROM any_dtab WHERE other_col = gc_any_constant.

    " one-liners can be created from short SELECT statements that fit into one line; otherwise,
    " if configured, FROM (or INTO) can be moved behind a short and simple SELECT list
    SELECT any_col FROM any_dtab
      INTO TABLE lt_any_table.

    SELECT any_col, other_col, third_col
      INTO TABLE lt_any_table
      FROM any_dtab
      WHERE any_col < 10.

    SELECT any_col, other_col, third_col
      FROM any_dtab
      APPENDING CORRESPONDING FIELDS OF TABLE @lt_any_table
      WHERE other_col IN ( SELECT other_col FROM other_dtab WHERE fourth_col = @lv_any_value )
        AND third_col IN ( SELECT third_col FROM third_dtab WHERE fifth_col > @lv_other_value ). "#EC CI_BUFFSUBQ.

    SELECT * FROM any_dtab
      WHERE any_col = @lv_any_value
      INTO CORRESPONDING FIELDS OF TABLE NEW @lv_other_value.

    SELECT any_col FROM any_dtab
      WHERE any_col IN ( SELECT any_col FROM other_dtab
                           WHERE other_col = 'X' )
      INTO CORRESPONDING FIELDS OF @ls_any_struc. "#EC CI_BUFFSUBQ
    ENDSELECT.

    " the alignment of joins can be configured with rule 'Align SELECT ... FROM ... JOIN'
    SELECT
      FROM any_dtab AS t1
           INNER JOIN other_dtab AS t2 ON  t1~any_col   = t2~any_col
                                       AND t1~other_col = t2~other_col
           LEFT OUTER JOIN third_dtab AS t3 ON t1~third_col = t3~third_col
      FIELDS (lt_fields)
      GROUP BY (lt_group)
      INTO CORRESPONDING FIELDS OF TABLE @lts_any_table.

    " UNION, INTERSECT and EXCEPT combine the result sets of multiple SELECTs:
    SELECT a AS c1, b AS c2, c AS c3
      FROM any_dtab
    UNION DISTINCT
    SELECT d AS c1, e AS c2, f AS c3
      FROM other_dtab
    UNION DISTINCT
    SELECT g AS c1, h AS c2, i AS c3
      FROM third_dtab
    INTO TABLE @FINAL(lt_distinct_result).

    " the final INTO ... clause belongs to the whole statement, not to the last SELECT!
    SELECT a AS c1, b AS c2, c AS c3, d AS c4
      FROM any_dtab
    INTERSECT
    ( SELECT d AS c1, e AS c2, f AS c3, g AS c4
        FROM other_dtab
      UNION
      SELECT i AS c1, j AS c2, k AS c3, l AS c4
        FROM third_dtab )
    INTO TABLE @FINAL(lt_result).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignSelectClausesRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignSelectClausesTest.java)

