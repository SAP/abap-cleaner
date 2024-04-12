[<-- previous rule](CalculationAssignmentRule.md) | [overview](../rules.md) | [next rule -->](NotIsRule.md)

# Use consistent set of comparison operators

Replaces textual comparison operators \(LT, LE, EQ, NE, GE, GT\) with symbolic comparison operators \(<, <=, =, <>, >=, >\) or vice versa.

## References

* [ABAP Keyword Documentation: Use consistent spelling](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlogexp_any_operand.htm)
* [ABAP Keyword Documentation: Obsolete Relational Operators](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenobsolete_logexp_op.htm)

## Options

* Preferred set of comparison operators: \[symbolic \(<, <=, =, <>, >=, >\)\]
* \[X\] Replace regular comparison operators with preferred variant
* \[X\] Replace obsolete comparison operators \(><  =>  =<\) with preferred variant

## Examples


```ABAP

CLASS any_class IMPLEMENTATION.
  METHOD use_consistent_comparison_ops.
    IF a EQ b OR c NE d.
      IF a < c AND b > d
               AND b < e.
        IF a LE d AND c GE b.
          result = xsdbool( a <= d OR a GE b ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

FORM any_form.
  " these obsolete variants are only possible outside of the object-oriented context:
  IF a >< b AND b => c OR c =< d.
    RETURN.
  ENDIF.
ENDFORM.
```

Resulting code:

```ABAP

CLASS any_class IMPLEMENTATION.
  METHOD use_consistent_comparison_ops.
    IF a = b OR c <> d.
      IF a < c AND b > d
               AND b < e.
        IF a <= d AND c >= b.
          result = xsdbool( a <= d OR a >= b ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

FORM any_form.
  " these obsolete variants are only possible outside of the object-oriented context:
  IF a <> b AND b >= c OR c <= d.
    RETURN.
  ENDIF.
ENDFORM.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/ComparisonOperatorRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/ComparisonOperatorTest.java)

