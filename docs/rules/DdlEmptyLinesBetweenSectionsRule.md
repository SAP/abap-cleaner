[<-- previous rule](DdlAlignSelectListRule.md) | [overview](../rules.md) | [next rule -->](DdlEmptyLinesWithinSectionsRule.md)

# Standardize empty lines between sections

Standardizes empty lines between the sections for entity annotations, parameters, joins, associations, etc.

## Options

* Empty line between entity annotations and DEFINE etc.: \[always at least one\]
* Empty line between parameters and AS SELECT FROM: \[always at least one\]
* Empty line between SELECT FROM and JOINs: \[always exactly one\]
* Empty line between JOINs and ASSOCIATIONs: \[always at least one\]
* Empty line before select list start '\{': \[always at least one\]
* Empty line after select list start '\{': \[never\]
* Empty line before select list end '\}': \[never\]
* Empty line after select list end '\}': \[always at least one\]
* \[X\] Remove empty lines at document end
* Maximum number of consecutive empty lines: \[2\] 

## Examples


```ASDDLS

@EndUserText.label: 'Any Description'
define view entity C_AnyEntity
  with parameters
    P_AnyParam:   any_parameter_type,
    P_OtherParam: other_type
  as select from I_AnyEntity as AnyAlias
    inner join I_OtherEntity as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField
    left outer to one join I_ThirdEntity as ThirdAlias
      on  AnyAlias.IdField    = ThirdAlias.IdField
      and AnyAlias.SubIdField = ThirdAlias.SubIdField
  // comment on association
  association [0..*] to I_FourthEntity as _FourthAlias
    on AnyAlias.IdField = _FourthAlias.IdField
{
  key AnyAlias.AnyKeyField,
  key AnyAlias.OtherKeyField,
      @Annotation.subAnno: 'value'
      OtherAlias.AnyNonKeyField
}
where AnyNonKeyField > 10

union all
  select from I_AnyEntity2 as AnyAlias

    inner join I_OtherEntity2 as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField


  // comment on association
  association [0..*] to I_ThirdEntity2 as _ThirdAlias
    on  AnyAlias.IdField    = _ThirdAlias.IdField
    and AnyAlias.SubIdField = _ThirdAlias.SubIdField

  association [0..*] to I_FourthEntity2 as _FourthAlias
    on AnyAlias.IdField = _FourthAlias.IdField



{


  key AnyAlias.AnyKeyField,
  key AnyAlias.OtherKeyField,



      OtherAlias.AnyNonKeyField


}


where AnyNonKeyField > 10



```

Resulting code:

```ASDDLS

@EndUserText.label: 'Any Description'

define view entity C_AnyEntity
  with parameters
    P_AnyParam:   any_parameter_type,
    P_OtherParam: other_type

  as select from I_AnyEntity as AnyAlias

    inner join I_OtherEntity as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField
    left outer to one join I_ThirdEntity as ThirdAlias
      on  AnyAlias.IdField    = ThirdAlias.IdField
      and AnyAlias.SubIdField = ThirdAlias.SubIdField

  // comment on association
  association [0..*] to I_FourthEntity as _FourthAlias
    on AnyAlias.IdField = _FourthAlias.IdField

{
  key AnyAlias.AnyKeyField,
  key AnyAlias.OtherKeyField,
      @Annotation.subAnno: 'value'
      OtherAlias.AnyNonKeyField
}

where AnyNonKeyField > 10

union all
  select from I_AnyEntity2 as AnyAlias

    inner join I_OtherEntity2 as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField


  // comment on association
  association [0..*] to I_ThirdEntity2 as _ThirdAlias
    on  AnyAlias.IdField    = _ThirdAlias.IdField
    and AnyAlias.SubIdField = _ThirdAlias.SubIdField

  association [0..*] to I_FourthEntity2 as _FourthAlias
    on AnyAlias.IdField = _FourthAlias.IdField


{
  key AnyAlias.AnyKeyField,
  key AnyAlias.OtherKeyField,


      OtherAlias.AnyNonKeyField
}


where AnyNonKeyField > 10
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/emptylines/DdlEmptyLinesBetweenSectionsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/emptylines/DdlEmptyLinesBetweenSectionsTest.java)

