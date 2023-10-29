[<-- previous rule](TypoRule.md) | [overview](../rules.md) | [next rule -->](CalculationAssignmentRule.md)

# Resolve equals sign chain into several commands

Resolves assignments to multiple variables with assignment operator chaining \(a = b = c = 1.\) into several commands.

Since different \(implicit\) conversion rules may apply to the assignments, the rightmost term can not be repeated, even if it is a literal.

## References

* [Clean ABAP Styleguide: Don't chain assignments](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-chain-assignments)
* [Clean Code Checks: Equals Sign Chaining](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/equals-sign-chaining.md)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD equals_sign_chaining.
    " assign a number literal
    a = b = 42. " comment

    " assign a string literal
    c = d = e = 'abc'.

    " assign a simple variable
    f = g = h = iv_value.

    " assign a complex expression
    i = j = k = l = m = complex_expression( iv_param_1 = 5
                                            iv_param_2 = 'abc' 
                                            iv_param_3 = VALUE #( iv_param_4 = 42  
                                                                  iv_param_5 = iv_value ) ). 

    " depending on variable types, assignments may invoke a sequence of implicit
    " conversion rules, therefore this can NOT be resolved into lv_numc8 = lv_float
    " and lv_datum = lv_float, but rather, all conversion steps must be kept:
    lv_datum = lv_numc8 = lv_float.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD equals_sign_chaining.
    " assign a number literal
    b = 42. " comment
    a = b.

    " assign a string literal
    e = 'abc'.
    d = e.
    c = d.

    " assign a simple variable
    h = iv_value.
    g = h.
    f = g.

    " assign a complex expression
    m = complex_expression( iv_param_1 = 5
                            iv_param_2 = 'abc'
                            iv_param_3 = VALUE #( iv_param_4 = 42
                                                  iv_param_5 = iv_value ) ).
    l = m.
    k = l.
    j = k.
    i = j.

    " depending on variable types, assignments may invoke a sequence of implicit
    " conversion rules, therefore this can NOT be resolved into lv_numc8 = lv_float
    " and lv_datum = lv_float, but rather, all conversion steps must be kept:
    lv_numc8 = lv_float.
    lv_datum = lv_numc8.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/EqualsSignChainRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/EqualsSignChainTest.java)

