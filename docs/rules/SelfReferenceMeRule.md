[<-- previous rule](ValueStatementRule.md) | [overview](../rules.md) | [next rule -->](ReceivingKeywordRule.md)

# Remove the self-reference me->

Removes the self-reference me->.

## References

* [Clean ABAP Styleguide: Omit the self-reference me when calling an instance attribute or method](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#omit-the-self-reference-me-when-calling-an-instance-attribute-or-method)
* [Clean Code Checks: Self-Reference](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/self-reference.md)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

CLASS any_class DEFINITION.
  PUBLIC SECTION.
    METHODS remove_self_reference_me
      EXPORTING ev_result  TYPE x
                ev_count   TYPE y
                ets_result TYPE z.
    METHODS signature_out_of_sight REDEFINITION.
  PRIVATE SECTION.
    " DATA ...
ENDCLASS.

CLASS any_class IMPLEMENTATION.
  METHOD remove_self_reference_me.
    me->any_method( a = 5
                    b = 'abc' ).

    ev_result = me->mv_value.

    ev_count += me->get_count( ).

    ets_result = VALUE #( ( line_id = 1  price = me->mv_price_1 )
                          ( line_id = 2  price = me->create_price( iv_currency = me->get_currency( )
                                                                   iv_amount   = me->mv_amount ) ) ).
  ENDMETHOD.

  METHOD signature_out_of_sight.
    " 'amount' and 'price' could be IMPORTING parameters, so me-> must NOT be removed
    me->amount = me->price + 1.
  ENDMETHOD.
ENDCLASS.
```

Resulting code:

```ABAP

CLASS any_class DEFINITION.
  PUBLIC SECTION.
    METHODS remove_self_reference_me
      EXPORTING ev_result  TYPE x
                ev_count   TYPE y
                ets_result TYPE z.
    METHODS signature_out_of_sight REDEFINITION.
  PRIVATE SECTION.
    " DATA ...
ENDCLASS.

CLASS any_class IMPLEMENTATION.
  METHOD remove_self_reference_me.
    any_method( a = 5
                b = 'abc' ).

    ev_result = mv_value.

    ev_count += get_count( ).

    ets_result = VALUE #( ( line_id = 1  price = mv_price_1 )
                          ( line_id = 2  price = create_price( iv_currency = get_currency( )
                                                               iv_amount   = mv_amount ) ) ).
  ENDMETHOD.

  METHOD signature_out_of_sight.
    " 'amount' and 'price' could be IMPORTING parameters, so me-> must NOT be removed
    me->amount = me->price + 1.
  ENDMETHOD.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/SelfReferenceMeRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/SelfReferenceMeTest.java)

