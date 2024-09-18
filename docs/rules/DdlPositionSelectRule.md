[<-- previous rule](DdlPositionDefineRule.md) | [overview](../rules.md) | [next rule -->](DdlPositionJoinRule.md)

# Break before AS SELECT etc.

Standardizes line breaks and indentation before entity name, WITH PARAMETERS, \[AS\] SELECT, AS PROJECTION ON and data source.

## Options

* Break before AS SELECT FROM: \[Always\]
* Indent if breaking: \[2\] 
* Break before SELECT FROM after UNION etc. \[Always\]
* Indent if breaking: \[2\] 
* Break before AS PROJECTION ON: \[Always\]
* Indent if breaking: \[2\] 
* Break before data source: \[Never\]
* Indent if breaking: \[4\] 

## Examples


```ASDDLS

define view entity C_AnyEntity as select
from I_AnyEntity as AnyAlias

    left outer to one join I_OtherEntity as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField

{
  key AnyAlias.AnyKeyField,
      AnyAlias.AnyNonKeyField
}

union
  all
    select
      from
        I_ThirdEntity As ThirdAlias

{
  key ThirdAlias.AnyKeyField,
      ThirdAlias.AnyNonKeyField
}

except select
from I_FourthEntity As FourthAlias

{
  key FourthAlias.AnyKeyField,
      FourthAlias.AnyNonKeyField
}
```

Resulting code:

```ASDDLS

define view entity C_AnyEntity
  as select from I_AnyEntity as AnyAlias

    left outer to one join I_OtherEntity as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField

{
  key AnyAlias.AnyKeyField,
      AnyAlias.AnyNonKeyField
}

union all
  select from I_ThirdEntity As ThirdAlias

{
  key ThirdAlias.AnyKeyField,
      ThirdAlias.AnyNonKeyField
}

except
  select from I_FourthEntity As FourthAlias

{
  key FourthAlias.AnyKeyField,
      FourthAlias.AnyNonKeyField
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/position/DdlPositionSelectRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/position/DdlPositionSelectTest.java)

