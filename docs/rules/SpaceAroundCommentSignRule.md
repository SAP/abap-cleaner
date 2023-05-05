[<-- previous rule](SpaceBeforePeriodRule.md) | [overview](../rules.md) | [next rule -->](NeedlessSpacesRule.md)

# Put spaces around " comment sign

Ensures that there is at least one space before and after the " comment sign.

## Options

* \[X\] Separate code and " comment with a space
* \[X\] Start " comment with space

## Examples


```ABAP

  METHOD space_around_comment_sign.
    "Comment signs
    "are NOT the same as "quotation marks",
    "so it looks much better
    "to put a space between the " and the text.

    CLEAR ev_result.  "the same is true at line end

    lv_value = 0."comment

    ls_pair = VALUE #(" initial comment
                       a = '3.1415'" pi
                       b = '1.4142'" sqrt(2)
                      )."final comment
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD space_around_comment_sign.
    " Comment signs
    " are NOT the same as "quotation marks",
    " so it looks much better
    " to put a space between the " and the text.

    CLEAR ev_result.  " the same is true at line end

    lv_value = 0. " comment

    ls_pair = VALUE #( " initial comment
                       a = '3.1415' " pi
                       b = '1.4142' " sqrt(2)
                      ). " final comment
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/spaces/SpaceAroundCommentSignRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/spaces/SpaceAroundCommentSignTest.java)

