[<-- previous rule](CheckOutsideLoopRule.md) | [overview](../rules.md) | [next rule -->](IfBlockAtLoopEndRule.md)

# Convert CHECK in loop to IF NOT ... CONTINUE

Converts CHECK inside a loop to IF NOT ... CONTINUE \(this applies to LOOP, DO and WHILE\)

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Avoid CHECK in other positions](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-check-in-other-positions)
* [Clean Code Checks: CHECK in LOOP](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/check-in-loop.md)

## Options

* Keep CHECK statement in LOOP: \[never\]
* Negate logical expressions with NOT \( ... \): \[if multiple inner negations \(IS NOT, <>, ...\) can be avoided\]
* \[X\] Convert abap\_false <-> abap\_true \(assuming abap\_undefined is never used\)
* \[X\] Unchain CHECK: chains in loops \(required for processing them with this rule\)

## Examples


```ABAP

  METHOD convert_check_in_loop.
    " outside the scope of this rule:
    CHECK its_input_table IS NOT INITIAL.

    DO 5 TIMES.
      CHECK its_table IS NOT INITIAL.

      LOOP AT its_table INTO DATA(ls_row).
        " the following CHECKs are considered to be at loop start (despite this comment)
        CHECK ls_row-min_id <> 0.
        CHECK ls_row-processed = abap_false.

        " chains can only be processed if they are first unchained
        CHECK: ls_row-max_id > 0,
               ls_row-flag IS NOT INITIAL.

        WHILE lv_id < ls_row-max_id.
          lv_id += 1.

          " these CHECKs are obviously NOT at loop start: 
          CHECK ( c IS NOT SUPPLIED OR b IS INITIAL ) AND ( d IS SUPPLIED OR b IS NOT INITIAL ).

          CHECK line_exists( its_table[ 0 ] ) AND its_table[ 0 ]-processed = abap_true
             OR lines( its_table ) > 2  AND line_exists( its_table[ 1 ] ).

          " do something very important
        ENDWHILE.

      ENDLOOP.

    ENDDO.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD convert_check_in_loop.
    " outside the scope of this rule:
    CHECK its_input_table IS NOT INITIAL.

    DO 5 TIMES.
      IF its_table IS INITIAL.
        CONTINUE.
      ENDIF.

      LOOP AT its_table INTO DATA(ls_row).
        " the following CHECKs are considered to be at loop start (despite this comment)
        IF ls_row-min_id = 0.
          CONTINUE.
        ENDIF.
        IF ls_row-processed = abap_true.
          CONTINUE.
        ENDIF.

        " chains can only be processed if they are first unchained
        IF ls_row-max_id <= 0.
          CONTINUE.
        ENDIF.
        IF ls_row-flag IS INITIAL.
          CONTINUE.
        ENDIF.

        WHILE lv_id < ls_row-max_id.
          lv_id += 1.

          " these CHECKs are obviously NOT at loop start: 
          IF ( c IS SUPPLIED AND b IS NOT INITIAL ) OR ( d IS NOT SUPPLIED AND b IS INITIAL ).
            CONTINUE.
          ENDIF.

          IF NOT (    line_exists( its_table[ 0 ] ) AND its_table[ 0 ]-processed = abap_true
                   OR lines( its_table ) > 2 AND line_exists( its_table[ 1 ] ) ).
            CONTINUE.
          ENDIF.

          " do something very important
        ENDWHILE.

      ENDLOOP.

    ENDDO.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/CheckInLoopRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/CheckInLoopTest.java)

