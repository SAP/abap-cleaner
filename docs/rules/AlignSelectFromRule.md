[<-- previous rule](AlignSelectClausesRule.md) | [overview](../rules.md) | [next rule -->](AlignSelectListsRule.md)

# Align SELECT ... FROM ... JOIN

Aligns the FROM clause, including joins, in the ABAP SQL statements SELECT, WITH, and OPEN CURSOR.

One-liners are kept. Logical expressions after JOIN ... ON are aligned by the rule 'Align logical expressions'. HIERARCHY...\( \) is kept unchanged.

## Options

* Position of first table name: \[continue after FROM\]
* Position of INNER etc. JOIN: \[next line \+2\]
* Position of further tables names: \[below join \+2\]
* Position of ON condition: \[continue after table name\]
* \[ \] Align AS across multiple joins
* \[ \] Align ON across multiple joins \(if ON is behind join\)
* Own line for CLIENT additions: \[only after joins\]

## Examples


```ABAP

  METHOD align_select_from.
    " when this rule is executed, the position of the keywords FROM, FIELDS, WHERE etc.
    " was already determined by the rule 'Align SELECT clauses';
    " this rule now aligns the FROM clause and possible JOINs in it:
    SELECT FROM first_dtab AS t1
             CROSS JOIN second_dtab AS t2
           FIELDS t1~a AS a1,
                  t1~b AS b1,
                  t2~c AS c2,
                  t2~d AS d2
           WHERE t2~d = t1~d
           ORDER BY t1~d
           INTO CORRESPONDING FIELDS OF TABLE @lt_table.

    SELECT
      FROM any_dtab AS t1
        INNER JOIN other_dtab AS t2 ON t2~a = t1~a
          LEFT OUTER JOIN third_dtab AS t3 ON t3~b = t2~a
            INNER JOIN fourth_dtab AS t4 ON  t4~c = t3~b
                                         AND t4~d = t3~d
      FIELDS t1~a AS a1,
             t1~b AS b1,
             t2~c AS c2,
             t3~d AS d3,
             t3~e AS e3,
             t4~f AS f4
      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.

    SELECT
      FROM ( any_dtab AS t1
           INNER JOIN
             other_dtab AS t2 ON t2~any_col = t1~any_col )
           LEFT OUTER JOIN
           third_dtab AS t3 ON t3~other_col = t2~any_col
             INNER JOIN
             fourth_dtab AS t4 ON t4~other_col = t3~other_col
      FIELDS t1~third_col AS any_alias,
             t4~fifth_col AS other_alias
      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.

    SELECT
      FROM any_dtab AS t1 LEFT OUTER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col USING CLIENT '123'
      FIELDS t2~other_col, t1~any_col, t2~third_col
      INTO TABLE @FINAL(lt_any_table).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD align_select_from.
    " when this rule is executed, the position of the keywords FROM, FIELDS, WHERE etc.
    " was already determined by the rule 'Align SELECT clauses';
    " this rule now aligns the FROM clause and possible JOINs in it:
    SELECT FROM first_dtab AS t1
                  CROSS JOIN
                    second_dtab AS t2
           FIELDS t1~a AS a1,
                  t1~b AS b1,
                  t2~c AS c2,
                  t2~d AS d2
           WHERE t2~d = t1~d
           ORDER BY t1~d
           INTO CORRESPONDING FIELDS OF TABLE @lt_table.

    SELECT
      FROM any_dtab AS t1
             INNER JOIN
               other_dtab AS t2 ON t2~a = t1~a
                 LEFT OUTER JOIN
                   third_dtab AS t3 ON t3~b = t2~a
                     INNER JOIN
                       fourth_dtab AS t4 ON  t4~c = t3~b
                                AND t4~d = t3~d
      FIELDS t1~a AS a1,
             t1~b AS b1,
             t2~c AS c2,
             t3~d AS d3,
             t3~e AS e3,
             t4~f AS f4
      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.

    SELECT
      FROM ( any_dtab AS t1
               INNER JOIN
                 other_dtab AS t2 ON t2~any_col = t1~any_col )
             LEFT OUTER JOIN
               third_dtab AS t3 ON t3~other_col = t2~any_col
                 INNER JOIN
                   fourth_dtab AS t4 ON t4~other_col = t3~other_col
      FIELDS t1~third_col AS any_alias,
             t4~fifth_col AS other_alias
      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.

    SELECT
      FROM any_dtab AS t1
             LEFT OUTER JOIN
               other_dtab AS t2 ON t1~any_col = t2~any_col
           USING CLIENT '123'
      FIELDS t2~other_col, t1~any_col, t2~third_col
      INTO TABLE @FINAL(lt_any_table).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignSelectFromRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignSelectFromTest.java)

