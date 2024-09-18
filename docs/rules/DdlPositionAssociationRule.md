[<-- previous rule](DdlPositionJoinRule.md) | [overview](../rules.md) | [next rule -->](DdlPositionBracesRule.md)

# Break before ASSOCIATIONs

Standardizes line breaks and indentation of ASSOCIATIONs.

## Options

* Break before keywords: \[Always\]
* Indent if breaking: \[2\] 
* Break before data source: \[Never\]
* Indent if breaking: \[2\] 
* Break before condition: \[If view contains multi-line condition\]
* Indent if breaking: \[4\] 
* Break before default filter: \[Always\]
* Indent if breaking: \[4\] 

## Examples


```ASDDLS

define view entity C_AnyEntity
  as select from I_AnyEntity as AnyAlias

    left outer to one join I_OtherEntity as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField

  association [0..1] to I_FourthEntity as _Fourth on  $projection.IdField = _Fourth.IdField
                                                  and _Fourth.CondField   = 'X'

  association of many
  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField
                                  and $projection.SubIdField = _Fifth.SubIdField

{
  key AnyAlias.IdField,
  key OtherAlias.SubIdField,

      OtherAlias.AnyNonKeyField,
      OtherAlias.OtherNonKeyField,

      _Fourth,
      _Fifth
}
```

Resulting code:

```ASDDLS

define view entity C_AnyEntity
  as select from I_AnyEntity as AnyAlias

    left outer to one join I_OtherEntity as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField

  association [0..1] to I_FourthEntity as _Fourth
    on  $projection.IdField = _Fourth.IdField
    and _Fourth.CondField   = 'X'

  association of many to many I_FifthEntity as _Fifth
    on  $projection.IdField    = _Fifth.IdField
    and $projection.SubIdField = _Fifth.SubIdField

{
  key AnyAlias.IdField,
  key OtherAlias.SubIdField,

      OtherAlias.AnyNonKeyField,
      OtherAlias.OtherNonKeyField,

      _Fourth,
      _Fifth
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/position/DdlPositionAssociationRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/position/DdlPositionAssociationTest.java)

