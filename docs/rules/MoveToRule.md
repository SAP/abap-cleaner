[<-- previous rule](AddToEtcRule.md) | [overview](../rules.md) | [next rule -->](TranslateRule.md)

# Replace obsolete MOVE ... TO with =

Replaces obsolete MOVE ... TO and MOVE ... ?TO statements with the more general assignment operators = and ?=.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Prefer functional to procedural language constructs](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-functional-to-procedural-language-constructs)
* [Clean ABAP Styleguide: Avoid obsolete language elements](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-obsolete-language-elements)
* [ABAP Keyword Documentation: Obsolete Assignments: MOVE](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapmove_obs.htm)

## Options

* \[X\] Unchain MOVE: chains \(required for processing them with this rule\)

## Examples


```ABAP

  METHOD replace_obsolete_move_to.
    MOVE 1 TO ev_result.
    MOVE 'ab' TO es_result-text.
    MOVE '12' TO ev_start+4(2).

    MOVE get_next_date( iv_year   = iv_year
                        iv_period = iv_period ) TO ev_date.

    MOVE lo_source ?TO lo_dest.

    MOVE EXACT source TO dest.

    MOVE:
      " some comment
      1 TO ev_value,
      '2023' TO ev_start(4),

      " another comment
      EXACT iv_data TO ev_data,
      io_instance ?TO eo_instance.

    " with the MOVE chain, get_next_value( ) is also called 3 times,
    " but without the chain, that's much clearer:
    MOVE get_next_value( ) TO: ev_any, ev_other, ev_third.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD replace_obsolete_move_to.
    ev_result = 1.
    es_result-text = 'ab'.
    ev_start+4(2) = '12'.

    ev_date = get_next_date( iv_year   = iv_year
                             iv_period = iv_period ).

    lo_dest ?= lo_source.

    dest = EXACT #( source ).

    " some comment
    ev_value = 1.
    ev_start(4) = '2023'.

    " another comment
    ev_data = EXACT #( iv_data ).
    eo_instance ?= io_instance.

    " with the MOVE chain, get_next_value( ) is also called 3 times,
    " but without the chain, that's much clearer:
    ev_any = get_next_value( ).
    ev_other = get_next_value( ).
    ev_third = get_next_value( ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/MoveToRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/MoveToTest.java)

