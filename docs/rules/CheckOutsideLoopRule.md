[<-- previous rule](ExportingKeywordRule.md) | [overview](../rules.md) | [next rule -->](CheckInLoopRule.md)

# Convert CHECK outside loop to IF NOT ... RETURN

Converts CHECK that is found outside of loops \(LOOP, DO, WHILE\) to IF NOT ... RETURN.

## References

* [Clean ABAP Styleguide: CHECK vs. RETURN](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#check-vs-return)
* [Clean ABAP Styleguide: Avoid CHECK in other positions](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-check-in-other-positions)
* [Clean Code Checks: CHECK Statement Position](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/check-statement-position.md)

## Options

* Keep CHECK statement: \[after declarations\]
* Negate logical expressions with NOT \( ... \): \[if multiple inner negations \(IS NOT, <>, ...\) can be avoided\]
* \[X\] Convert abap\_false <-> abap\_true \(assuming abap\_undefined is never used\)
* \[X\] Allow CHECK after ASSERT, BREAK-POINT and LOG-POINT

## Examples


```ABAP

  METHOD convert_check_outside_loop.
    " CHECKs at earliest possible position
    CHECK its_table IS NOT INITIAL.

    CHECK is_item_buffer-item_id     IS NOT INITIAL
      AND is_item_buffer-first_flag   = abap_false
      AND is_item_buffer-second_flag  = abap_false
      AND is_item_buffer-last_flag    = abap_true.

    DATA: lv_any_value     TYPE i,
          lv_another_value TYPE string.

    FIELD-SYMBOLS <ls_struc> LIKE LINE OF its_table.

    " CHECKs only preceded by declarations
    CHECK ( c IS NOT SUPPLIED OR b IS INITIAL ) AND ( d IS SUPPLIED OR b IS NOT INITIAL ).

    CLEAR ev_success.

    " CHECKs only preceded by declarations and CLEAR
    CHECK a = abap_false AND b > 3 OR a = abap_true AND b <= 10.

    lv_value = 1.

    " CHECKs inside the method
    CHECK line_exists( its_table[ 0 ] ) 
       OR lines( its_table ) > 2  AND line_exists( its_table[ 1 ] ).
  ENDMETHOD.


  METHOD check_after_checkpoints.
    " various checkpoints
    BREAK-POINT.
    LOG-POINT ID any_id.
    ASSERT iv_value > 0.

    CHECK its_table IS NOT INITIAL.

    DATA lv_any_value TYPE i.

    CLEAR ev_success.
    CHECK its_table IS NOT INITIAL.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD convert_check_outside_loop.
    " CHECKs at earliest possible position
    CHECK its_table IS NOT INITIAL.

    CHECK is_item_buffer-item_id     IS NOT INITIAL
      AND is_item_buffer-first_flag   = abap_false
      AND is_item_buffer-second_flag  = abap_false
      AND is_item_buffer-last_flag    = abap_true.

    DATA: lv_any_value     TYPE i,
          lv_another_value TYPE string.

    FIELD-SYMBOLS <ls_struc> LIKE LINE OF its_table.

    " CHECKs only preceded by declarations
    CHECK ( c IS NOT SUPPLIED OR b IS INITIAL ) AND ( d IS SUPPLIED OR b IS NOT INITIAL ).

    CLEAR ev_success.

    " CHECKs only preceded by declarations and CLEAR
    IF ( a = abap_true OR b <= 3 ) AND ( a = abap_false OR b > 10 ).
      RETURN.
    ENDIF.

    lv_value = 1.

    " CHECKs inside the method
    IF NOT (    line_exists( its_table[ 0 ] )
             OR lines( its_table ) > 2 AND line_exists( its_table[ 1 ] ) ).
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD check_after_checkpoints.
    " various checkpoints
    BREAK-POINT.
    LOG-POINT ID any_id.
    ASSERT iv_value > 0.

    CHECK its_table IS NOT INITIAL.

    DATA lv_any_value TYPE i.

    CLEAR ev_success.
    IF its_table IS INITIAL.
      RETURN.
    ENDIF.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/CheckOutsideLoopRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/CheckOutsideLoopTest.java)

