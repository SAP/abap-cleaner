[<-- previous rule](NeedlessParenthesesRule.md) | [overview](../rules.md) | [next rule -->](ValueStatementRule.md)

# Remove empty commands

Removes commands that only consist of . or : , . but keeps comments which these commands may contain.

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD remove_empty_commands.
    " although this method contains syntactically correct ABAP code,
    " we hope you'll never see anything like this in real life!

    DATA lv_primary TYPE i...

    lv_primary = 2.
    . . .
    lv_primary = 3.

    . lv_primary = 5.
    .. lv_primary = 7..
    :,. lv_primary = 11.

    ::::. lv_primary = 13. ::::.
    .:.:,.:,.lv_primary = 17.:.:,.:,.
    ... lv_primary = 19... lv_primary = 23... lv_primary = 29...

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

    DATA lv_primary TYPE i.

    lv_primary = 2.
    lv_primary = 3.

    lv_primary = 5.
    lv_primary = 7.
    lv_primary = 11.

    lv_primary = 13.
    lv_primary = 17.
    lv_primary = 19. lv_primary = 23. lv_primary = 29.

    " comment 1
*   comment 2
    " comment 3
    " comment 4
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/EmptyCommandRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/EmptyCommandTest.java)

