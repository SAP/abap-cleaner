[<-- previous rule](CalculationAssignmentRule.md) | [overview](../rules.md) | [next rule -->](NotIsRule.md)

# Prefer =, <>, <= etc. to EQ, NE, LE etc.

Replaces keywords \(LT, LE, EQ, NE, GT, GE\) with symbolic comparison operators \(<, <=, =, >=, >, <>\).

## References

* [ABAP Keyword Documentation: Use consistent spelling](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlogexp_any_operand.htm)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD prefer_symbolic_comparison_ops.
    IF a EQ b OR c NE d.
      IF a LT c AND b GT d
                AND b LT e.
        IF a LE d AND c GE b.
          " do something
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD prefer_symbolic_comparison_ops.
    IF a = b OR c <> d.
      IF a < c AND b > d
               AND b < e.
        IF a <= d AND c >= b.
          " do something
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/ComparisonOperatorRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/ComparisonOperatorTest.java)

