[<-- previous rule](DdlPositionSelectRule.md) | [overview](../rules.md) | [next rule -->](DdlPositionAssociationRule.md)

# Break before JOINs

Standardizes line breaks and indentation of JOINs.

## Options

* Break before keywords: \[Always\]
* Indent if breaking: \[4\] 
* Break before data source: \[Never\]
* Indent if breaking: \[4\] 
* Break before condition: \[If view contains multi-line condition\]
* Indent if breaking: \[6\] 

## Examples


```ASDDLS

define view entity C_AnyEntity
  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField

  left outer to one
  join I_ThirdEntity as ThirdAlias
  on  AnyAlias.IdField      = ThirdAlias.IdField
  and OtherAlias.SubIdField = ThirdAlias.SubIdField

  association [0..1] to I_FourthEntity as _Fourth
    on  $projection.IdField = _Fourth.IdField
    and _Fourth.CondField   = 'X'

{
  key AnyAlias.IdField,
  key ThirdAlias.SubIdField,

      OtherAlias.AnyNonKeyField,
      ThirdAlias.OtherNonKeyField,

      _Fourth
}
```

Resulting code:

```ASDDLS

define view entity C_AnyEntity
  as select from I_AnyEntity as AnyAlias

    left outer to one join I_OtherEntity as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField

    left outer to one join I_ThirdEntity as ThirdAlias
      on  AnyAlias.IdField      = ThirdAlias.IdField
      and OtherAlias.SubIdField = ThirdAlias.SubIdField

  association [0..1] to I_FourthEntity as _Fourth
    on  $projection.IdField = _Fourth.IdField
    and _Fourth.CondField   = 'X'

{
  key AnyAlias.IdField,
  key ThirdAlias.SubIdField,

      OtherAlias.AnyNonKeyField,
      ThirdAlias.OtherNonKeyField,

      _Fourth
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/position/DdlPositionJoinRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/position/DdlPositionJoinTest.java)

