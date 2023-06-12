[<-- previous rule](EqualsSignChainRule.md) | [overview](../rules.md) | [next rule -->](ComparisonOperatorRule.md)

# Prefer calculation assignment operators \(\+=, -= etc.\)

Transforms assignments like i = i \+ 1 to use calculation assignment operators \(\+=, -= etc.\).

This rule requires a NetWeaver version >= 7.54.

## Options

* \[ \] Convert 'a = - a' to 'a \*= -1'
* \[X\] Convert 'a = - a \* b' and 'a = - a / b'
* \[X\] Convert 'a = b \+ a' and 'a = b \* a'

## Examples


```ABAP

  METHOD prefer_calculation_assign_ops.
    " simple cases
    lv_value = lv_value + 1.
    lv_value = lv_value - 1.
    lv_value = lv_value * lv_factor.
    lv_value = lv_value / lv_divisor.

    lv_value = - lv_value.
    lv_value = - lv_value * 10.
    lv_value = - lv_value / lv_divisor.

    lv_value = 2 + lv_value.
    lv_value = - 10 * lv_value.
    lv_value = lv_factor * lv_value.

    " more complex cases
    ls_struc-component = ls_struc-component + 1.
    <ls_field>-component = <ls_field>-component - 1.
    <ls_field>-component = - <ls_field>-component.
    lv_value = lv_value * ( -1 ).  " parentheses must be removed, because *= ( -1 ) would be a syntax error
    lv_value = lv_value + get_value( EXPORTING iv_value1 = lv_value 
                                               iv_value2 = 'abc'    
                                               iv_value3 = VALUE #( a = 5
                                                                    b = 7 ) ). 
    lv_date+4(2) = lv_date+4(2) - 1.

    " cases that must NOT be changed
    lv_value = iv_value + 1.     " identifiers do not match!
    lv_value = lv_value * 5 + 3. " due to operator priority this is NOT the same as lv_value *= 5 + 3
    lv_value = lv_value / 2 + 1. " due to operator priority this is NOT the same as lv_value /= 2 + 1
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD prefer_calculation_assign_ops.
    " simple cases
    lv_value += 1.
    lv_value -= 1.
    lv_value *= lv_factor.
    lv_value /= lv_divisor.

    lv_value = - lv_value.
    lv_value *= -10.
    lv_value /= - lv_divisor.

    lv_value += 2.
    lv_value *= -10.
    lv_value *= lv_factor.

    " more complex cases
    ls_struc-component += 1.
    <ls_field>-component -= 1.
    <ls_field>-component = - <ls_field>-component.
    lv_value *= -1.  " parentheses must be removed, because *= ( -1 ) would be a syntax error
    lv_value += get_value( EXPORTING iv_value1 = lv_value
                                     iv_value2 = 'abc'
                                     iv_value3 = VALUE #( a = 5
                                                          b = 7 ) ).
    lv_date+4(2) -= 1.

    " cases that must NOT be changed
    lv_value = iv_value + 1.     " identifiers do not match!
    lv_value = lv_value * 5 + 3. " due to operator priority this is NOT the same as lv_value *= 5 + 3
    lv_value = lv_value / 2 + 1. " due to operator priority this is NOT the same as lv_value /= 2 + 1
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/CalculationAssignmentRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/CalculationAssignmentTest.java)

