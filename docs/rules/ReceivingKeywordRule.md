[<-- previous rule](SelfReferenceMeRule.md) | [overview](../rules.md) | [next rule -->](ExportingKeywordRule.md)

# Omit RECEIVING

Transforms method calls that use the RECEIVING keyword into functional style.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Omit RECEIVING](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#omit-receiving)
* [Clean Code Checks: RECEIVING Statement Usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/receiving-usage.md)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD omit_receiving.
    any_method(
      EXPORTING iv_param  = lv_param
      IMPORTING ev_param  = ev_param
      CHANGING  cv_param  = cv_param
      RECEIVING rv_result = lv_result ).

    cl_any_class=>get( )->any_method(
      IMPORTING
        ev_param  = ev_param
      CHANGING
        cv_param  = cv_param
      RECEIVING
        rv_result = DATA(lv_result2) ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD omit_receiving.
    lv_result = any_method(
                  EXPORTING iv_param  = lv_param
                  IMPORTING ev_param  = ev_param
                  CHANGING  cv_param  = cv_param ).

    DATA(lv_result2) = cl_any_class=>get( )->any_method(
                         IMPORTING
                           ev_param  = ev_param
                         CHANGING
                           cv_param  = cv_param ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/ReceivingKeywordRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/ReceivingKeywordTest.java)

