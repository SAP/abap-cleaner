[<-- previous rule](DdlPositionAssociationRule.md) | [overview](../rules.md) | [next rule -->](DdlPositionClausesRule.md)

# Break before select list braces

Standardizes line breaks and indentation of braces around select lists.

## Options

* Break before opening brace \{ of select list: \[Always\]
* Indent if breaking: \[0\] 
* Break before closing brace \} of select list: \[Always\]
* Indent if breaking: \[0\] 
* Break before FROM \(in syntax without braces\): \[Always\]
* Indent if breaking: \[2\] 

## Examples


```ASDDLS

define view entity C_AnyEntity
  as select from I_AnyEntity as AnyAlias

  {
  key AnyAlias.AnyKeyField,
      AnyAlias.AnyNonKeyField
      }

union all 
  select from I_OtherEntity As OtherAlias {
  key OtherAlias.AnyKeyField,
      OtherAlias.AnyNonKeyField
  }

except
  select from I_ThirdEntity As ThirdAlias
  {
  key ThirdAlias.AnyKeyField,
      ThirdAlias.AnyNonKeyField }
```

Resulting code:

```ASDDLS

define view entity C_AnyEntity
  as select from I_AnyEntity as AnyAlias

{
  key AnyAlias.AnyKeyField,
      AnyAlias.AnyNonKeyField
}

union all
  select from I_OtherEntity As OtherAlias
{
  key OtherAlias.AnyKeyField,
      OtherAlias.AnyNonKeyField
}

except
  select from I_ThirdEntity As ThirdAlias
{
  key ThirdAlias.AnyKeyField,
      ThirdAlias.AnyNonKeyField
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/position/DdlPositionBracesRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/position/DdlPositionBracesTest.java)

