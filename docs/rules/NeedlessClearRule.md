[<-- previous rule](ChainRule.md) | [overview](../rules.md) | [next rule -->](LocalDeclarationOrderRule.md)

# Remove needless CLEAR

Removes needless CLEAR of local variables from method start and method end.

## Options

* Action for CLEAR at method start: \[delete\]
* Action for CLEAR at method end: \[add TODO comment\]
* \[X\] Keep CLEAR for structure that is assigned to directly afterwards

## Examples


```ABAP

  METHOD remove_needless_clear.
    DATA lv_any   TYPE i.
    DATA lv_other TYPE string.
    DATA lv_third TYPE i VALUE 3.
    DATA ls_struc TYPE ty_s_struc.
    DATA lt_table TYPE ty_tt_any.
    DATA lr_ref   TYPE REF TO data.

    " while exporting parameters should be cleared, local variables automatically
    " get their initial value and therefore do not need CLEAR
    CLEAR lv_any.
    CLEAR: ev_result,
           lv_other,
           et_table,
           lv_third,
           lt_table.

    " a common case where you may want to keep CLEAR even at method start is this:
    CLEAR ls_struc.
    ls_struc-id   = 1.
    ls_struc-name = 'abc'.
    APPEND ls_struc TO et_table.

    CLEAR ls_struc.
    ls_struc-id   = 2.
    ls_struc-name = 'def'.
    APPEND ls_struc TO et_table.

    lr_ref = REF #( mt_any_table ).
    CLEAR lr_ref->*.

    " no need to clear local variables at method end, either; however, you may only want to
    " add a TODO comment here, because you may just be in the process of writing the method
    CLEAR: lv_other, lv_any.
    CLEAR: lv_third, lr_ref.
    CLEAR: lt_table,
           ev_table.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD remove_needless_clear.
    DATA lv_any   TYPE i.
    DATA lv_other TYPE string.
    DATA lv_third TYPE i VALUE 3.
    DATA ls_struc TYPE ty_s_struc.
    DATA lt_table TYPE ty_tt_any.
    DATA lr_ref   TYPE REF TO data.

    " while exporting parameters should be cleared, local variables automatically
    " get their initial value and therefore do not need CLEAR
    CLEAR: ev_result,
           et_table,
           lv_third.

    " a common case where you may want to keep CLEAR even at method start is this:
    CLEAR ls_struc.
    ls_struc-id   = 1.
    ls_struc-name = 'abc'.
    APPEND ls_struc TO et_table.

    CLEAR ls_struc.
    ls_struc-id   = 2.
    ls_struc-name = 'def'.
    APPEND ls_struc TO et_table.

    lr_ref = REF #( mt_any_table ).
    CLEAR lr_ref->*.

    " no need to clear local variables at method end, either; however, you may only want to
    " add a TODO comment here, because you may just be in the process of writing the method
    " TODO: remove needless CLEAR (ABAP cleaner)
    CLEAR: lv_other, lv_any.
    " TODO: remove needless CLEAR (ABAP cleaner)
    CLEAR: lv_third, lr_ref.
    " TODO: remove needless CLEAR (ABAP cleaner)
    CLEAR: lt_table,
           ev_table.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/NeedlessClearRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/NeedlessClearTest.java)

