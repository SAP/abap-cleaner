[<-- previous rule](ExportingKeywordRule.md) | [overview](../rules.md) | [next rule -->](CheckOutsideLoopRule.md)

# Replace EXIT outside loop with RETURN

Replaces EXIT outside loop with RETURN to exit the processing block.

## References

* [ABAP Keyword Documentation: Only use RETURN to exit procedures](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenexit_procedure_guidl.htm)

## Options

* \[X\] Apply to procedures \(METHOD, FUNCTION, FORM\)
* \[X\] Apply to event blocks \(INITIALIZATION, START-OF-SELECTION etc.\)

## Examples


```ABAP

REPORT any_report.

CLASS any_class IMPLEMENTATION.
  METHOD any_method.
    DO 5 TIMES.
      IF sy-index = 4.
        " this continues after ENDDO
        EXIT.
      ENDIF.
    ENDDO.

    CASE iv_action.
      WHEN 'exit'.
        EXIT.
      WHEN OTHERS.
        do_something( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  IF a = b.
    EXIT.
  ENDIF.

START-OF-SELECTION.
  PERFORM any_form.
  EXIT.
  PERFORM unreachable_code.

FORM any_form.
  LOOP AT gt_any_table ASSIGNING <ls_any> WHERE comp1 > 10.
    " this continues after ENDLOOP
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    EXIT.
  ENDIF.
ENDFORM.
```

Resulting code:

```ABAP

REPORT any_report.

CLASS any_class IMPLEMENTATION.
  METHOD any_method.
    DO 5 TIMES.
      IF sy-index = 4.
        " this continues after ENDDO
        EXIT.
      ENDIF.
    ENDDO.

    CASE iv_action.
      WHEN 'exit'.
        RETURN.
      WHEN OTHERS.
        do_something( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  IF a = b.
    RETURN.
  ENDIF.

START-OF-SELECTION.
  PERFORM any_form.
  RETURN.
  PERFORM unreachable_code.

FORM any_form.
  LOOP AT gt_any_table ASSIGNING <ls_any> WHERE comp1 > 10.
    " this continues after ENDLOOP
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    RETURN.
  ENDIF.
ENDFORM.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/ExitOutsideLoopRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/ExitOutsideLoopTest.java)

