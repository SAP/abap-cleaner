[<-- previous rule](CheckInLoopRule.md) | [overview](../rules.md) | [next rule -->](IfBlockAtMethodEndRule.md)

# Replace long IF blocks at loop end

Replaces long IF blocks at loop end with IF NOT ... CONTINUE to decrease nesting depth, prefering early exit to overdoing structured programming.

## References

* [Clean ABAP Styleguide: Keep the nesting depth low](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#keep-the-nesting-depth-low)

## Options

* Replace IF blocks with at least \[10\] lines
* and a share of at least \[50\] % of all lines
* \[X\] Keep IF blocks that contain exceptional logic \(RAISE, MESSAGE, RETURN, EXIT\)
* Negate logical expressions with NOT \( ... \): \[if multiple inner negations \(IS NOT, <>, ...\) can be avoided\]
* \[X\] Convert abap\_false <-> abap\_true \(assuming abap\_undefined is never used\)
* \[X\] Add empty line after ENDIF

## Examples


```ABAP

  METHOD replace_if_block_at_loop_end.
    DATA lv_count TYPE i.
    DATA lv_value TYPE i.
    
    WHILE lv_count > 0.
      lv_count -= 1.
      IF lv_count MOD 2 = 0.
        LOOP AT its_table INTO DATA(ls_row).
          " if the row was already processed, there is nothing to do. 
          IF ls_row-processed = abap_false.
            IF ls_row-another_component = abap_true.
              " if the row was not yet processed, there's so much to do!
              lv_value = 1.
              lv_value = 2.
              lv_value = 3.
              lv_value = 4.
              lv_value = 5.
              lv_value = 6.
              lv_value = 7.
              lv_value = 8.
              lv_value = 9.
              ls_row-processed = abap_true.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.


  METHOD replace_if_block_at_loop_end_2.
    DATA lv_count TYPE i.
    DATA lv_value TYPE i.
    
    WHILE lv_count > 0.
      lv_count -= 1.
      IF lv_count MOD 2 = 0.
        LOOP AT its_table INTO DATA(ls_row).
          " if the row was already processed, there is nothing to do. 
          IF ls_row-processed = abap_false.
            " if the row was not yet processed, there's so much to do!
            lv_value = 1.
            lv_value = 2.
            lv_value = 3.
            ls_row-processed = abap_true.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD replace_if_block_at_loop_end.
    DATA lv_count TYPE i.
    DATA lv_value TYPE i.

    WHILE lv_count > 0.
      lv_count -= 1.
      IF lv_count MOD 2 <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT its_table INTO DATA(ls_row).
        " if the row was already processed, there is nothing to do. 
        IF ls_row-processed = abap_true.
          CONTINUE.
        ENDIF.

        IF ls_row-another_component = abap_false.
          CONTINUE.
        ENDIF.

        " if the row was not yet processed, there's so much to do!
        lv_value = 1.
        lv_value = 2.
        lv_value = 3.
        lv_value = 4.
        lv_value = 5.
        lv_value = 6.
        lv_value = 7.
        lv_value = 8.
        lv_value = 9.
        ls_row-processed = abap_true.
      ENDLOOP.
    ENDWHILE.
  ENDMETHOD.


  METHOD replace_if_block_at_loop_end_2.
    DATA lv_count TYPE i.
    DATA lv_value TYPE i.

    WHILE lv_count > 0.
      lv_count -= 1.
      IF lv_count MOD 2 <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT its_table INTO DATA(ls_row).
        " if the row was already processed, there is nothing to do. 
        IF ls_row-processed = abap_false.
          " if the row was not yet processed, there's so much to do!
          lv_value = 1.
          lv_value = 2.
          lv_value = 3.
          ls_row-processed = abap_true.
        ENDIF.
      ENDLOOP.
    ENDWHILE.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/IfBlockAtLoopEndRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/IfBlockAtLoopEndTest.java)

