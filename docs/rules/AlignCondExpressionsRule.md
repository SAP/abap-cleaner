[<-- previous rule](AlignLogicalExpressionsRule.md) | [overview](../rules.md) | [next rule -->](AlignFormDeclarationRule.md)

# Align conditional expressions

Aligns conditional expressions with the constructor operators COND \#\( WHEN ... THEN ... ELSE ... \) and SWTICH \#\( operand WHEN ... THEN ... ELSE ... \).

## Options

* Maximum line length for one-liners and tabular cases: \[120\] 
* One-liners: \[keep as is\]
* Simple cases \(1x WHEN\): \[put WHEN/THEN/ELSE below each other\]
* Tabular cases \(multiple WHEN\): \[put WHEN... THEN ... on one line if possible\]
* \[X\] Tabular cases: Align ELSE value with THEN values
* \[X\] Complex cases: Put THEN at the end of WHEN line
* \[ \] Complex cases: Continue after ELSE
* Complex cases: Indentation of values: \[below WHEN \+ 2 spaces\]

## Examples


```ABAP

  METHOD align_cond_expressions.
    ev_value = COND #( WHEN iv_value IS SUPPLIED   THEN   iv_value   ELSE   gc_default_value ).

    ev_sign = COND #( WHEN iv_negative = abap_true
                        THEN -1
                          ELSE 1 ).

    ev_number = COND #( WHEN io_object->get_sub_object( )->get_number( ) <= if_object_boundaries=>co_maximum_number THEN
                          io_object->get_sub_object( )->get_number( )
                        ELSE
                           if_object_boundaries=>co_maximum_number ).

    ev_any_non_zero_amount = COND #( WHEN ls_amounts-betrw IS NOT INITIAL THEN ls_amounts-betrw
                                      WHEN ls_amounts-betrh IS NOT INITIAL THEN ls_amounts-betrh
                                   WHEN ls_amounts-betr2 IS NOT INITIAL THEN ls_amounts-betr2
                                         WHEN ls_amounts-betr3 IS NOT INITIAL THEN ls_amounts-betr3 ).

    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0
                                      THEN   ls_amounts-waers
                                    WHEN ls_amounts-betrh <> 0
                                       THEN  ls_amounts-hwaer
                                    WHEN ls_amounts-betr2 <> 0
                                      THEN   ls_amounts-hwae2
                                     WHEN ls_amounts-betr3 <> 0
                                        THEN ls_amounts-hwae3
                                    ELSE    gc_default_currency_code ).

    display_time_info( COND #( LET t = '120000' IN
                             WHEN sy-timlo < t
                               THEN |{ sy-timlo TIME = ISO } AM|
                              WHEN sy-timlo > t AND sy-timlo < '240000'
                                THEN |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|
                               WHEN sy-timlo = t
                                 THEN |High Noon|
                                ELSE THROW cx_cant_be( ) ) ).

    ev_num = SWITCH #( ev_num WHEN 999 THEN 0 ELSE ( ev_num + 1 ) ).

    out->write( SWITCH string( sy-index
                               WHEN 1 THEN 'one'
                               WHEN 2 THEN 'two'
                               WHEN 3 THEN 'three' ELSE THROW cx_overflow( ) ) ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD align_cond_expressions.
    ev_value = COND #( WHEN iv_value IS SUPPLIED THEN iv_value ELSE gc_default_value ).

    ev_sign = COND #( WHEN iv_negative = abap_true
                      THEN -1
                      ELSE 1 ).

    ev_number = COND #( WHEN io_object->get_sub_object( )->get_number( ) <= if_object_boundaries=>co_maximum_number
                        THEN io_object->get_sub_object( )->get_number( )
                        ELSE if_object_boundaries=>co_maximum_number ).

    ev_any_non_zero_amount = COND #( WHEN ls_amounts-betrw IS NOT INITIAL THEN ls_amounts-betrw
                                     WHEN ls_amounts-betrh IS NOT INITIAL THEN ls_amounts-betrh
                                     WHEN ls_amounts-betr2 IS NOT INITIAL THEN ls_amounts-betr2
                                     WHEN ls_amounts-betr3 IS NOT INITIAL THEN ls_amounts-betr3 ).

    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0 THEN ls_amounts-waers
                                    WHEN ls_amounts-betrh <> 0 THEN ls_amounts-hwaer
                                    WHEN ls_amounts-betr2 <> 0 THEN ls_amounts-hwae2
                                    WHEN ls_amounts-betr3 <> 0 THEN ls_amounts-hwae3
                                    ELSE                            gc_default_currency_code ).

    display_time_info( COND #( LET t = '120000' IN
                               WHEN sy-timlo < t THEN
                                 |{ sy-timlo TIME = ISO } AM|
                               WHEN sy-timlo > t AND sy-timlo < '240000' THEN
                                 |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|
                               WHEN sy-timlo = t THEN
                                 |High Noon|
                               ELSE
                                 THROW cx_cant_be( ) ) ).

    ev_num = SWITCH #( ev_num WHEN 999 THEN 0 ELSE ( ev_num + 1 ) ).

    out->write( SWITCH string( sy-index
                               WHEN 1 THEN 'one'
                               WHEN 2 THEN 'two'
                               WHEN 3 THEN 'three'
                               ELSE        THROW cx_overflow( ) ) ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignCondExpressionsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignCondExpressionsTest.java)

