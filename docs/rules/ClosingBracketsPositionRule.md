[<-- previous rule](SpaceAroundTextLiteralRule.md) | [overview](../rules.md) | [next rule -->](SpaceBeforePeriodRule.md)

# Close brackets at line end

Closes brackets at line end by moving one or many \) \] . to the previous line.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Close brackets at line end](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#close-brackets-at-line-end)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD close_brackets_at_line_end.
    ev_result = VALUE #( ( a = 1
                           b = 2
                         )
                         ( a = 2
                           b = 4 " comment
                         )
                       ).

    any_method( iv_name   = 'ABC'
                iv_amount = get_random( iv_min = lts_min[ id  = lv_item_id
                                                          var = lv_var
                                                        ]
                                        iv_max = 100
                                      )
              ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD close_brackets_at_line_end.
    ev_result = VALUE #( ( a = 1
                           b = 2 )
                         ( a = 2
                           b = 4 ) ). " comment

    any_method( iv_name   = 'ABC'
                iv_amount = get_random( iv_min = lts_min[ id  = lv_item_id
                                                          var = lv_var ]
                                        iv_max = 100 ) ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/spaces/ClosingBracketsPositionRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/spaces/ClosingBracketsPositionTest.java)

