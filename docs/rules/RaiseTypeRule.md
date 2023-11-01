[<-- previous rule](CreateObjectRule.md) | [overview](../rules.md) | [next rule -->](AddToEtcRule.md)

# Replace RAISE ... TYPE with RAISE ... NEW

Replaces the TYPE section of RAISE EXCEPTION TYPE ... and RAISE SHORTDUMP TYPE ... with a NEW constructor call.

This rule requires a NetWeaver version >= 7.52.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Prefer RAISE EXCEPTION NEW to RAISE EXCEPTION TYPE](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-raise-exception-new-to-raise-exception-type)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD replace_raise_type_with_new.
    RAISE EXCEPTION TYPE cx_any_exception.

    RAISE RESUMABLE EXCEPTION TYPE cx_any_exception.

    RAISE EXCEPTION TYPE cx_other_exception
      EXPORTING
        any_param   = any_value
        other_param = other_value.

    RAISE SHORTDUMP TYPE cx_demo_t100
      EXPORTING
        textid = cx_demo_t100=>demo
        text1  = 'this'
        text2  = 'is'
        text3  = 'an'
        text4  = 'example'.

    " NEW is not possible with [USING] MESSAGE
    RAISE EXCEPTION TYPE cx_any_exception USING MESSAGE.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD replace_raise_type_with_new.
    RAISE EXCEPTION NEW cx_any_exception( ).

    RAISE RESUMABLE EXCEPTION NEW cx_any_exception( ).

    RAISE EXCEPTION NEW cx_other_exception(
        any_param   = any_value
        other_param = other_value ).

    RAISE SHORTDUMP NEW cx_demo_t100(
        textid = cx_demo_t100=>demo
        text1  = 'this'
        text2  = 'is'
        text3  = 'an'
        text4  = 'example' ).

    " NEW is not possible with [USING] MESSAGE
    RAISE EXCEPTION TYPE cx_any_exception USING MESSAGE.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/RaiseTypeRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/RaiseTypeTest.java)

