[<-- previous rule](LogicalOperatorPositionRule.md) | [overview](../rules.md) | [next rule -->](ValueStatementRule.md)

# Remove empty commands

Removes commands that only consist of . or : , . but keeps comments which these commands may contain.

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD remove_empty_commands.
    " although this method contains syntactically correct ABAP code,
    " we hope you'll never see anything like this in real life!

    DATA iv_primary TYPE i...

    iv_primary = 2.
    . . .
    iv_primary = 3.

    . iv_primary = 5.
    .. iv_primary = 7..
    :,. iv_primary = 11.

    ::::. iv_primary = 13. ::::.
    .:.:,.:,.iv_primary = 17.:.:,.:,.
    ... iv_primary = 19... iv_primary = 23... iv_primary = 29...

    : " comment 1
*   comment 2
    , " comment 3
    . " comment 4
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD remove_empty_commands.
    " although this method contains syntactically correct ABAP code,
    " we hope you'll never see anything like this in real life!

    DATA iv_primary TYPE i.

    iv_primary = 2.
    iv_primary = 3.

    iv_primary = 5.
    iv_primary = 7.
    iv_primary = 11.

    iv_primary = 13.
    iv_primary = 17.
    iv_primary = 19. iv_primary = 23. iv_primary = 29.

    " comment 1
*   comment 2
    " comment 3
    " comment 4
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/EmptyCommandRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/EmptyCommandTest.java)

