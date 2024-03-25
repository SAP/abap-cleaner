[<-- previous rule](TranslateRule.md) | [overview](../rules.md) | [next rule -->](DescribeTableRule.md)

# Replace CONDENSE with string function

Replaces the CONDENSE statement with the string processing function condense\( \).

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Prefer functional to procedural language constructs](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-functional-to-procedural-language-constructs)
* [ABAP Keyword Documentation: CONDENSE](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcondense.htm)
* [ABAP Keyword Documentation: string\_func - condense](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencondense_functions.htm)

## Options

* \[ \] Explicitly specify parameter val = ... even if no other parameters are used
* \[ \] Explicitly specify parameter del = \` \`, except for NO-GAPS
* \[X\] Explicitly specify parameter from = \` \` for NO-GAPS
* \[ \] Keep parameters on one line \(see rule 'Align parameters and components', option 'Keep other one-liners'\)

## Examples


```ABAP

  METHOD replace_condense.
    CONSTANTS lc_abc_with_gaps TYPE string VALUE `  a   b   c  `.

    DATA lv_text_a   TYPE char30 VALUE lc_abc_with_gaps.
    DATA lv_text_b   TYPE char30 VALUE lc_abc_with_gaps.
    DATA lv_text_c   TYPE char30 VALUE lc_abc_with_gaps.
    DATA lv_string_a TYPE string VALUE lc_abc_with_gaps.
    DATA lv_string_b TYPE string VALUE lc_abc_with_gaps.

    " condense first text field to 'a b c', second one to 'abc'
    CONDENSE lv_text_a.
    CONDENSE lv_text_b NO-GAPS.

    " condense first string to 'a b c', second one to 'abc'
    CONDENSE lv_string_a.
    CONDENSE lv_string_b NO-GAPS.

    " condense text field with offset 5 and length 7 to `  a  b c`
    " (specifying offset and length in write positions is possible for text fields, but not for strings)
    CONDENSE lv_text_c+5(7).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD replace_condense.
    CONSTANTS lc_abc_with_gaps TYPE string VALUE `  a   b   c  `.

    DATA lv_text_a   TYPE char30 VALUE lc_abc_with_gaps.
    DATA lv_text_b   TYPE char30 VALUE lc_abc_with_gaps.
    DATA lv_text_c   TYPE char30 VALUE lc_abc_with_gaps.
    DATA lv_string_a TYPE string VALUE lc_abc_with_gaps.
    DATA lv_string_b TYPE string VALUE lc_abc_with_gaps.

    " condense first text field to 'a b c', second one to 'abc'
    lv_text_a = condense( lv_text_a ).
    lv_text_b = condense( val  = lv_text_b
                          from = ` `
                          to   = `` ).

    " condense first string to 'a b c', second one to 'abc'
    lv_string_a = condense( lv_string_a ).
    lv_string_b = condense( val  = lv_string_b
                            from = ` `
                            to   = `` ).

    " condense text field with offset 5 and length 7 to `  a  b c`
    " (specifying offset and length in write positions is possible for text fields, but not for strings)
    lv_text_c+5(7) = condense( lv_text_c+5(7) ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/CondenseRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/CondenseTest.java)

