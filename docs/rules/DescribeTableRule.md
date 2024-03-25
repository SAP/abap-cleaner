[<-- previous rule](CondenseRule.md) | [overview](../rules.md) | [next rule -->](ReadTableRule.md)

# Replace DESCRIBE TABLE ... LINES with lines\( \)

Replaces DESCRIBE TABLE ... LINES with the built-in function lines\( \).

Statements cannot be replaced if they use other additions \(KIND ... or OCCURS ...\) or SY-TFILL / SY-TLENG are evaluated afterwards.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Prefer functional to procedural language constructs](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-functional-to-procedural-language-constructs)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD describe_table_lines.
    " the following cases can be replaced with the built-in function lines( ):
    DESCRIBE TABLE its_any_table LINES ev_any_line_count.
    DESCRIBE TABLE its_any_table LINES DATA(lv_any_line_count).
    DESCRIBE TABLE its_other_table LINES FINAL(lv_other_line_count).

    " DESCRIBE TABLE with other additions cannot be replaced
    DESCRIBE TABLE it_third_table KIND DATA(lv_kind) LINES lv_any_line_count.
    DESCRIBE TABLE it_third_table OCCURS FINAL(lv_init_mem_requirement).

    " DESCRIBE TABLE with any subsequent evaluation of SY-TLENG or SY-TFILL cannot be replaced
    DESCRIBE TABLE it_third_table LINES lv_any_line_count.
    DATA(lv_line_count) = sy-tfill.
    DATA(lv_line_length_in_bytes) = sy-tleng.

    " here, SY-TLENG is also evaluated in the program flow after DESCRIBE TABLE
    " (but only for the second one, so the first one can be changed)
    DO 3 TIMES.
      IF sy-index = 3.
        RETURN sy-tleng.
      ENDIF.
      DESCRIBE TABLE it_third_table LINES lv_any_line_count.
      DESCRIBE TABLE it_fourth_table LINES lv_line_count.
    ENDDO.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD describe_table_lines.
    " the following cases can be replaced with the built-in function lines( ):
    ev_any_line_count = lines( its_any_table ).
    DATA(lv_any_line_count) = lines( its_any_table ).
    FINAL(lv_other_line_count) = lines( its_other_table ).

    " DESCRIBE TABLE with other additions cannot be replaced
    DESCRIBE TABLE it_third_table KIND DATA(lv_kind) LINES lv_any_line_count.
    DESCRIBE TABLE it_third_table OCCURS FINAL(lv_init_mem_requirement).

    " DESCRIBE TABLE with any subsequent evaluation of SY-TLENG or SY-TFILL cannot be replaced
    DESCRIBE TABLE it_third_table LINES lv_any_line_count.
    DATA(lv_line_count) = sy-tfill.
    DATA(lv_line_length_in_bytes) = sy-tleng.

    " here, SY-TLENG is also evaluated in the program flow after DESCRIBE TABLE
    " (but only for the second one, so the first one can be changed)
    DO 3 TIMES.
      IF sy-index = 3.
        RETURN sy-tleng.
      ENDIF.
      lv_any_line_count = lines( it_third_table ).
      DESCRIBE TABLE it_fourth_table LINES lv_line_count.
    ENDDO.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/DescribeTableRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/DescribeTableTest.java)

