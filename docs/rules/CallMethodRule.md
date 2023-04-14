[<-- previous rule](IfBlockAtMethodEndRule.md) | [overview](../rules.md) | [next rule -->](CreateObjectRule.md)

# Replace CALL METHOD with functional call

Replaces obsolete CALL METHOD statements with functional calls, adding parentheses, if missing. Leaves CALL METHOD if dynamic typing is used for the method name.

## References

* [Clean ABAP Styleguide: Prefer functional to procedural calls](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-functional-to-procedural-calls)
* [Clean ABAP Styleguide: Avoid obsolete language elements](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-obsolete-language-elements)
* [Clean Code Checks: CALL METHOD Usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/call-method-usage.md)
* [ABAP Keyword Documentation: Formulate static method calls without CALL METHOD](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenmethod_call_guidl.htm)
* [ABAP Keyword Documentation: Obsolete Calls: CALL METHOD, Static](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapcall_method_static.htm)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD replace_call_method.
    CALL METHOD lo_any_instance->any_method
      EXPORTING iv_param = iv_param
      IMPORTING ev_param = ev_param
      CHANGING  cv_param = cv_param.

    " obsolete EXPORTING will be removed:
    CALL METHOD any_method_name
      EXPORTING iv_name   = iv_name
                iv_value  = iv_value.

    " RECEIVING may be omitted, depending on Omit RECEIVING rule:
    CALL METHOD other_method_name
      EXPORTING iv_name   = iv_name
                iv_value  = iv_value
      RECEIVING rv_result = DATA(lv_result).

    " dynamic method calls cannot be replaced
    CALL METHOD mo_any_instance->(iv_dynamic_method_name)
      EXPORTING iv_par = iv_par. 

    CALL METHOD (iv_dynamic_class_name)=>(iv_dynamic_method_name)
      EXPORTING iv_par = iv_par. 
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD replace_call_method.
    lo_any_instance->any_method(
      EXPORTING iv_param = iv_param
      IMPORTING ev_param = ev_param
      CHANGING  cv_param = cv_param ).

    " obsolete EXPORTING will be removed:
    any_method_name(
        iv_name   = iv_name
        iv_value  = iv_value ).

    " RECEIVING may be omitted, depending on Omit RECEIVING rule:
    DATA(lv_result) = other_method_name(
                          iv_name   = iv_name
                          iv_value  = iv_value ).

    " dynamic method calls cannot be replaced
    CALL METHOD mo_any_instance->(iv_dynamic_method_name)
      EXPORTING iv_par = iv_par.

    CALL METHOD (iv_dynamic_class_name)=>(iv_dynamic_method_name)
      EXPORTING iv_par = iv_par.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/CallMethodRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/CallMethodTest.java)

