[<-- previous rule](RaiseTypeRule.md) | [overview](../rules.md) | [next rule -->](MoveToRule.md)

# Replace obsolete ADD ... TO etc. with \+= etc.

Replaces obsolete ADD TO, SUBTRACT FROM, MULTIPLY BY and DIVIDE BY statements with the corresponding calculation assignment operators \+=, -=, \*=, and /= for NetWeaver versions >= 7.54. For older syntax, the statements can be replaced with a = a \+ ... etc. \(see options\)

## References

* [Clean ABAP Styleguide: Prefer functional to procedural language constructs](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-functional-to-procedural-language-constructs)
* [Clean ABAP Styleguide: Avoid obsolete language elements](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-obsolete-language-elements)
* [ABAP Keyword Documentation: Use the operator format](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencalc_expresssion_guidl.htm)
* [ABAP Keyword Documentation: Obsolete Calculation Statements: ADD](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapadd.htm)
* [ABAP Keyword Documentation: Obsolete Calculation Statements: SUBTRACT, MULTIPLY, DIVIDE](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapsubtract_multiply_divide.htm)

## Options

* If cleanup is restricted to NetWeaver < 7.54 syntax, \[replace with 'a = a \+ ...' etc.\]

## Examples


```ABAP

  METHOD replace_obsolete_add_to_etc.
    ADD 1 TO ls_struc-component.
    ADD lts_table[ num  = 5
                   name = 'abc']-length TO lv_length.

    SUBTRACT lo_typedesc->length FROM lv_length.
    SUBTRACT class_name( )=>get_tool( )->get_value( iv_param  = 5
                                                    iv_param2 = 'abc' ) FROM lv_length.

    SUBTRACT 1 FROM ls_any_structure-item_key.
    SUBTRACT lo_typedesc->length FROM lv_length.
    SUBTRACT lts_table[ num  = 5
                        name = 'abc']-length FROM lv_length.

    MULTIPLY iv_value BY 2.
    MULTIPLY lv_value BY lts_table[ num  = 5
                                    name = 'abc']-component.

    DIVIDE lv_value BY lo_struc-component.
    DIVIDE lv_value BY class_name( )=>get_tool( )->get_value( iv_param  = 5
                                                              iv_param2 = 'abc' ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD replace_obsolete_add_to_etc.
    ls_struc-component += 1.
    lv_length += lts_table[ num  = 5
                            name = 'abc']-length.

    lv_length -= lo_typedesc->length.
    lv_length -= class_name( )=>get_tool( )->get_value( iv_param  = 5
                                                        iv_param2 = 'abc' ).

    ls_any_structure-item_key -= 1.
    lv_length -= lo_typedesc->length.
    lv_length -= lts_table[ num  = 5
                            name = 'abc']-length.

    iv_value *= 2.
    lv_value *= lts_table[ num  = 5
                           name = 'abc']-component.

    lv_value /= lo_struc-component.
    lv_value /= class_name( )=>get_tool( )->get_value( iv_param  = 5
                                                       iv_param2 = 'abc' ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/AddToEtcRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/AddToEtcTest.java)

