[<-- previous rule](TypoRule.md) | [overview](../rules.md) | [next rule -->](CalculationAssignmentRule.md)

# Resolve equals sign chain into several commands

Resolves assignments to multiple variables with assignment operator chaining \(a = b = c = 1.\) into several commands.

## References

* [Clean ABAP Styleguide: Don't chain assignments](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-chain-assignments)
* [Clean Code Checks: Equals Sign Chaining](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/equals-sign-chaining.md)

## Options

* \[X\] Repeat integer literals
* \[X\] Repeat string literals
* \[X\] Repeat simple identifiers

## Examples


```ABAP

  METHOD equals_sign_chaining.
    " assign a number literal
    a = b = 42. " plain and simple

    " assign a string literal
    c = d = e = 'abc'. " still quite simple

    " assign a simple variable
    f = g = h = iv_value.

    " assign a complex expression that should not be repeated
    i = j = k = l = m = complex_expression( iv_param_1 = 5
                                            iv_param_2 = 'abc' 
                                            iv_param_3 = VALUE #( iv_param_4 = 42  
                                                                  iv_param_5 = iv_value ) ). 
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD equals_sign_chaining.
    " assign a number literal
    b = 42. " plain and simple
    a = 42.

    " assign a string literal
    e = 'abc'. " still quite simple
    d = 'abc'.
    c = 'abc'.

    " assign a simple variable
    h = iv_value.
    g = iv_value.
    f = iv_value.

    " assign a complex expression that should not be repeated
    m = complex_expression( iv_param_1 = 5
                            iv_param_2 = 'abc'
                            iv_param_3 = VALUE #( iv_param_4 = 42
                                                  iv_param_5 = iv_value ) ).
    l = m.
    k = l.
    j = k.
    i = j.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/EqualsSignChainRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/EqualsSignChainTest.java)

