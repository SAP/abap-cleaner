[<-- previous rule](ReceivingKeywordRule.md) | [overview](../rules.md) | [next rule -->](CheckOutsideLoopRule.md)

# Omit optional EXPORTING

Removes the optional EXPORTING keyword from method calls that do not use any other keyword \(IMPORTING, CHANGING, RECEIVING, or EXCEPTIONS\).

## References

* [Clean ABAP Styleguide: Omit the optional keyword EXPORTING](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#omit-the-optional-keyword-exporting)
* [Clean Code Checks: Omit Optional EXPORTING](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/omit-optional-exporting.md)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD omit_optional_exporting.
    lo_any_object->any_method(
      EXPORTING iv_par1 = iv_par1
                iv_par2 = iv_par2 ).

    lv_value = get_value( EXPORTING iv_value1 = lv_value
                                    iv_value2 = 'abc'
                                    iv_value3 = get_inner_value( EXPORTING a = 5
                                                                           b = 7 ) ).

    " calls with other keywords such as IMPORTING and RECEIVING must NOT be changed:
    calculate( EXPORTING iv_value  = lv_value
               IMPORTING ev_result = lv_result ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD omit_optional_exporting.
    lo_any_object->any_method(
        iv_par1 = iv_par1
        iv_par2 = iv_par2 ).

    lv_value = get_value( iv_value1 = lv_value
                          iv_value2 = 'abc'
                          iv_value3 = get_inner_value( a = 5
                                                       b = 7 ) ).

    " calls with other keywords such as IMPORTING and RECEIVING must NOT be changed:
    calculate( EXPORTING iv_value  = lv_value
               IMPORTING ev_result = lv_result ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/ExportingKeywordRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/ExportingKeywordTest.java)

