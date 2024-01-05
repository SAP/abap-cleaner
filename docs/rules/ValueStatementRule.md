[<-- previous rule](EmptyCommandRule.md) | [overview](../rules.md) | [next rule -->](SelfReferenceMeRule.md)

# Shorten VALUE statements

Shortens VALUE statements for internal tables by factoring out assignments that are identical in all lines.

## Options

* \[X\] Move assignments with integer literals
* \[X\] Move assignments with '...' numeric literals
* \[X\] Move assignments with string literals
* \[X\] Move assignments with identifiers / constants
* \[X\] Move assignments with complex expressions
* \[ \] Move assignments with method calls \(WARNING: could change behavior if methods with side effects are called\!\)

## Examples


```ABAP

  METHOD shorten_value_statements.
    lts_data = VALUE #( ( a = 2  b = 3  c = 6 )
                        ( a = 2  b = 4  c = 8 ) 
                        ( a = 2  b = 5  c = 10 )  ).

    lts_data_exp = VALUE #( ( item_key       = '20220010000101'
                              item_type      = lt_item[ 1 ]->item_data-item_type
                              contract_id    = lt_item[ 1 ]->get_contract_id( )
                              contrac_type   = 'ABCD'
                              category       = 'AB'
                              flag           = if_any_interface=>co_any_flag
                              is_defined     = 'Y'
                              gjahr          = 2022
                              poper          = '001'
                              amount         = 6000
                              currency       = if_any_interface=>co_any_currency
                              account        = '12345'
                              guid           = '1')
                            ( item_key       = '20220010000101'
                              item_type      = lt_item[ 1 ]->item_data-item_type
                              contract_id    = lt_item[ 1 ]->get_contract_id( )
                              contract_type  = 'ABCD'
                              category       = 'CD'
                              flag           = if_any_interface=>co_other_flag
                              is_optional    = abap_true
                              is_defined     = 'Y'
                              gjahr          = 2022
                              poper          = '001'
                              amount         = -6000
                              currency       = if_any_interface=>co_any_currency
                              account        = '67890'
                              guid           = '2' ) ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD shorten_value_statements.
    lts_data = VALUE #( a = 2
                        ( b = 3  c = 6 )
                        ( b = 4  c = 8 )
                        ( b = 5  c = 10 )  ).

    lts_data_exp = VALUE #( item_key       = '20220010000101'
                            item_type      = lt_item[ 1 ]->item_data-item_type
                            is_defined     = 'Y'
                            gjahr          = 2022
                            poper          = '001'
                            currency       = if_any_interface=>co_any_currency
                            ( contract_id    = lt_item[ 1 ]->get_contract_id( )
                              contrac_type   = 'ABCD'
                              category       = 'AB'
                              flag           = if_any_interface=>co_any_flag
                              amount         = 6000
                              account        = '12345'
                              guid           = '1')
                            ( contract_id    = lt_item[ 1 ]->get_contract_id( )
                              contract_type  = 'ABCD'
                              category       = 'CD'
                              flag           = if_any_interface=>co_other_flag
                              is_optional    = abap_true
                              amount         = -6000
                              account        = '67890'
                              guid           = '2' ) ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/ValueStatementRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/ValueStatementTest.java)

