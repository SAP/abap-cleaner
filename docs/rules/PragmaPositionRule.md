[<-- previous rule](PseudoCommentRule.md) | [overview](../rules.md) | [next rule -->](TypoRule.md)

# Move pragmas to correct position

Moves pragmas to the correct position at the end of the line, but before the period or comma. Pragmas at line start are correctly positioned but can optionally be moved, too.

## References

* [ABAP Keyword Documentation: Pragmas](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenpragma.htm)

## Options

* \[ \] Move \(correctly positioned\) pragmas from line start to line end

## Examples


```ABAP

  METHOD move_pragmas_to_correct_pos.
    " pragmas at line end must be placed before the comma or period;
    " they are also correct at line start, but you may prefer them at line end
    CONSTANTS lc_key TYPE ty_key VALUE 'abc'. ##NO_TEXT
    ##NO_TEXT CONSTANTS lc_other_key TYPE ty_key VALUE 'def'.

    DATA: a ##NEEDED TYPE string, " comment
          b TYPE string. ##NEEDED

    " a pragma before the chain colon is effective for all parts of the chain;
    " however, it cannot be moved if the line continues behind the colon
    ##NEEDED DATA:
          c TYPE string,
          d TYPE string.
    ##NEEDED DATA: e TYPE string,
          f TYPE string.

    MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest. ##ENH_OK

    " unlike pseudo comments, ##NEEDED should not be placed inside the empty block:
    DO 5 TIMES.
      ##NEEDED
    ENDDO.

    " the same is true for ##NO_HANDLER, which belongs to the CATCH statement:
    TRY.
      GET BADI lo_any_badi.
    CATCH cx_badi_not_implemented. " comment
      ##NO_HANDLER
    ENDTRY.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD move_pragmas_to_correct_pos.
    " pragmas at line end must be placed before the comma or period;
    " they are also correct at line start, but you may prefer them at line end
    CONSTANTS lc_key TYPE ty_key VALUE 'abc' ##NO_TEXT.
    ##NO_TEXT CONSTANTS lc_other_key TYPE ty_key VALUE 'def'.

    DATA: a TYPE string ##NEEDED, " comment
          b TYPE string ##NEEDED.

    " a pragma before the chain colon is effective for all parts of the chain;
    " however, it cannot be moved if the line continues behind the colon
    ##NEEDED DATA:
          c TYPE string,
          d TYPE string.
    ##NEEDED DATA: e TYPE string,
          f TYPE string.

    MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest ##ENH_OK.

    " unlike pseudo comments, ##NEEDED should not be placed inside the empty block:
    DO 5 TIMES ##NEEDED.
    ENDDO.

    " the same is true for ##NO_HANDLER, which belongs to the CATCH statement:
    TRY.
      GET BADI lo_any_badi.
    CATCH cx_badi_not_implemented ##NO_HANDLER. " comment
    ENDTRY.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/PragmaPositionRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/PragmaPositionTest.java)

