[<-- previous rule](RaiseTypeRule.md) | [overview](../rules.md) | [next rule -->](MoveToRule.md)

# Replace obsolete ADD ... TO etc. with \+= etc.

Replaces obsolete ADD TO, SUBTRACT FROM, MULTIPLY BY and DIVIDE BY statements with the corresponding calculation assignment operators \+=, -=, \*=, and /=.

This rule requires a NetWeaver version >= 7.54. For older syntax, the statements can be replaced with a = a \+ ... etc. \(see options\)

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Prefer functional to procedural language constructs](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-functional-to-procedural-language-constructs)
* [Clean ABAP Styleguide: Avoid obsolete language elements](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-obsolete-language-elements)
* [ABAP Keyword Documentation: Use the operator format](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencalc_expresssion_guidl.htm)
* [ABAP Keyword Documentation: Obsolete Calculation Statements: ADD](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapadd.htm)
* [ABAP Keyword Documentation: Obsolete Calculation Statements: SUBTRACT, MULTIPLY, DIVIDE](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapsubtract_multiply_divide.htm)

## Options

* If cleanup is restricted to NetWeaver < 7.54 syntax, \[replace with 'a = a \+ ...' etc.\]
* \[X\] Unchain ADD:, SUBTRACT: etc. chains \(required for processing them with this rule\)

## Examples


```ABAP

  METHOD replace_obsolete_add_to_etc.
    ADD 1 TO ls_struc-component.
    ADD iv_value TO lv_length.

    SUBTRACT lo_typedesc->length FROM lv_length.
    SUBTRACT is_struc-component FROM lv_length.

    SUBTRACT 1 FROM ls_any_structure-item_key.
    SUBTRACT lo_typedesc->length FROM lv_length.

    MULTIPLY iv_value BY 2.
    MULTIPLY lv_value BY ls_any_structure-component.

    DIVIDE lv_value BY lo_instance->attribute.
    DIVIDE lv_value BY lo_instance->ms_structure-component.

    " chains can only be processed if they are first unchained
    ADD 10 TO: lv_value, lv_other.

    SUBTRACT: 1 FROM lv_value, 2 FROM lv_other.

    MULTIPLY iv_value BY: 2, 3, 5.

    DIVIDE iv_value: BY lv_any, BY lv_other.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD replace_obsolete_add_to_etc.
    ls_struc-component += 1.
    lv_length += iv_value.

    lv_length -= lo_typedesc->length.
    lv_length -= is_struc-component.

    ls_any_structure-item_key -= 1.
    lv_length -= lo_typedesc->length.

    iv_value *= 2.
    lv_value *= ls_any_structure-component.

    lv_value /= lo_instance->attribute.
    lv_value /= lo_instance->ms_structure-component.

    " chains can only be processed if they are first unchained
    lv_value += 10.
    lv_other += 10.

    lv_value -= 1.
    lv_other -= 2.

    iv_value *= 2.
    iv_value *= 3.
    iv_value *= 5.

    iv_value /= lv_any.
    iv_value /= lv_other.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/AddToEtcRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/AddToEtcTest.java)

