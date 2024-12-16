[<-- previous rule](AlignWithSecondWordRule.md) | [overview](../rules.md) | [next rule -->](AlignSelectClausesRule.md)

# Align CLEAR:, FREE:, SORT and CATCH

Aligns lists of variables after CLEAR: and FREE:, lists of components after SORT ... BY, and lists of exception classes after CATCH.

## Options

* Maximum line length \[120\] 
* CLEAR: Use one line per variable: \[always\]
* FREE: Use one line per variable: \[always\]
* SORT: Use one line per variable: \[always\]
* CATCH: Use one line per exception: \[always\]

## Examples


```ABAP

  METHOD align_clear_free_sort_catch.
    CLEAR: mv_any_value,
      mv_other_value,
         ls_any_structure-any_component,
        ls_any_structure-other_component,
        mt_any_table, mt_other_table, mt_third_table.

    CLEAR:   mv_any_value,
      mv_other_value WITH lv_initial_value IN CHARACTER MODE,
      mv_third_value WITH NULL,
         mv_fourth_value WITH lv_initial_value IN BYTE MODE.

    FREE: mt_any_table, mts_other_table,
         mth_third_table.

    SORT mt_any_table STABLE BY comp1 comp2
     comp3
     comp4.

    SORT: mt_other_table BY   comp1 comp2 DESCENDING
     comp3 comp4 AS TEXT,
          mt_third_table BY  component1 AS TEXT
      component2   component3.

    TRY.
        any_method( ).

      CATCH cx_any_exception  cx_other_exception
        cx_third_exception INTO DATA(lx_exception) ##NO_HANDLER.

      CATCH cx_fourth_exception cx_fifth_exception   cx_sixth_exception cx_seventh_exception cx_eighth_exception  cx_ninth_exception INTO lx_exception.
        " handle exceptions
    ENDTRY.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD align_clear_free_sort_catch.
    CLEAR: mv_any_value,
           mv_other_value,
           ls_any_structure-any_component,
           ls_any_structure-other_component,
           mt_any_table,
           mt_other_table,
           mt_third_table.

    CLEAR: mv_any_value,
           mv_other_value WITH lv_initial_value IN CHARACTER MODE,
           mv_third_value WITH NULL,
           mv_fourth_value WITH lv_initial_value IN BYTE MODE.

    FREE: mt_any_table,
          mts_other_table,
          mth_third_table.

    SORT mt_any_table STABLE BY comp1
                                comp2
                                comp3
                                comp4.

    SORT: mt_other_table BY comp1
                            comp2 DESCENDING
                            comp3
                            comp4 AS TEXT,
          mt_third_table BY component1 AS TEXT
                            component2
                            component3.

    TRY.
        any_method( ).

      CATCH cx_any_exception
            cx_other_exception
            cx_third_exception INTO DATA(lx_exception) ##NO_HANDLER.

      CATCH cx_fourth_exception
            cx_fifth_exception
            cx_sixth_exception
            cx_seventh_exception
            cx_eighth_exception
            cx_ninth_exception INTO lx_exception.
        " handle exceptions
    ENDTRY.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignClearFreeAndSortRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignClearFreeAndSortTest.java)

