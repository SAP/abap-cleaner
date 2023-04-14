[<-- previous rule](EmptyLinesInClassDefinitionRule.md) | [overview](../rules.md) | [next rule -->](ClosingBracketsPositionRule.md)

# Remove multiple spaces in empty parentheses

Removes multiple spaces from empty parentheses \(e.g. in method call chains\).

## References

* [Clean ABAP Styleguide: Condense your code](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#condense-your-code)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD single_space_in_empty_parens.
    ev_result = class_name(  )=>get_tool(  )->get_value(      ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD single_space_in_empty_parens.
    ev_result = class_name( )=>get_tool( )->get_value( ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/spaces/SpacesInEmptyBracketsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/spaces/SpacesInEmptyBracketsTest.java)

