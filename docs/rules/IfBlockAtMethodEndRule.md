[<-- previous rule](IfBlockAtLoopEndRule.md) | [overview](../rules.md) | [next rule -->](CallMethodRule.md)

# Replace long IF blocks at method end

Replaces long IF blocks at method end with IF NOT ... RETURN to decrease nesting depth, prefering early exit to overdoing structured programming.

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

  METHOD replace_if_block_at_method_end.
    DATA lv_value TYPE i.

    " if the item was already processed, there is nothing to do. 
    IF mv_item_processed = abap_false.
      " if the item was not yet processed, there's so much to do!
      lv_value = 1.
      lv_value = 2.
      lv_value = 3.
      lv_value = 4.
      lv_value = 5.
      lv_value = 6.
      lv_value = 7.
      lv_value = 8.
      lv_value = 9.
      mv_item_processed = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD replace_if_block_at_meth_end_2.
    DATA lv_value TYPE i.

    " if the item was already processed, there is nothing to do. 
    IF mv_item_processed = abap_false.
      " if the item was not yet processed, there's so much to do!
      lv_value = 1.
      lv_value = 2.
      lv_value = 3.
      mv_item_processed = abap_true.
    ENDIF.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD replace_if_block_at_method_end.
    DATA lv_value TYPE i.

    " if the item was already processed, there is nothing to do. 
    IF mv_item_processed = abap_true.
      RETURN.
    ENDIF.

    " if the item was not yet processed, there's so much to do!
    lv_value = 1.
    lv_value = 2.
    lv_value = 3.
    lv_value = 4.
    lv_value = 5.
    lv_value = 6.
    lv_value = 7.
    lv_value = 8.
    lv_value = 9.
    mv_item_processed = abap_true.
  ENDMETHOD.


  METHOD replace_if_block_at_meth_end_2.
    DATA lv_value TYPE i.

    " if the item was already processed, there is nothing to do. 
    IF mv_item_processed = abap_false.
      " if the item was not yet processed, there's so much to do!
      lv_value = 1.
      lv_value = 2.
      lv_value = 3.
      mv_item_processed = abap_true.
    ENDIF.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/IfBlockAtMethodEndRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/IfBlockAtMethodEndTest.java)

