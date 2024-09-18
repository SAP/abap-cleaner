[<-- previous rule](DdlEmptyLinesBetweenSectionsRule.md) | [overview](../rules.md) | next rule -->

# Standardize empty lines within sections

Standardizes empty lines within the sections for entity parameters, joins, associations and the select list.

## Options

* Add empty lines around entity parameters that span \[2\] lines or more
* Add empty lines around JOINs that span \[2\] lines or more
* Add empty lines around ASSOCIATIONs that span \[2\] lines or more
* Add empty lines around select list elements that span \[2\] lines or more
* Add empty lines around WHERE etc. clauses that span \[2\] lines or more
* \[X\] Add empty line below KEY fields in select list
* \[X\] Add empty line above associations in select list
* Remove empty lines between single-line parameters: \[if all are detached one-liners\]
* Remove empty lines between single-line JOINs: \[if all are detached one-liners\]
* Remove empty lines between single-line ASSOCIATIONs: \[if all are detached one-liners\]
* Remove empty lines between single-line select list elements: \[if all are detached one-liners\]
* Remove empty lines between single-line WHERE etc. clauses: \[never\]
* Warning: With this setting, intentional visual grouping might get lost.

## Examples


```ASDDLS

@EndUserText.label: 'Any Description'
define view entity C_AnyEntity
  with parameters
    P_AnyParam:   any_parameter_type,
    @Annotation.subAnno: 'value'
    P_OtherParam: other_type

  as select from I_AnyEntity as AnyAlias
    inner join I_OtherEntity2      as OtherAlias on AnyAlias.IdField = OtherAlias.IdField
    left outer join I_ThirdEntity2 as ThirdAlias on  AnyAlias.IdField      = ThirdAlias.IdField
                                                 and AnyAlias.SubIdField   = ThirdAlias.SubIdField
                                                 and AnyAlias.ThirdIdField = ThirdAlias.ThirdIdField

  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField

  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField

  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField

{
  key AnyAlias.AnyKeyField,
  key AnyAlias.OtherKeyField,
      sum(OtherAlias.AnyNonKeyField),
      @Annotation.subAnno: 'value'
      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField
      @<Annotation.otherSubAnno: 'this annotation refers to the previous element!',
      max(OtherAlias.ThirdNonKeyField)  as ThirdNonKeyField,
      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,
      max(ThirdAlias.FifthNonKeyField)  as FifthNonKeyField,
      _FourthAlias,
      _FifthAlias,
      _SixthAlias
}
where  AnyNonKeyField  > 10
  and OtherNonKeyField = 'A'
group by AnyAlias.AnyKeyField
         AnyAlias.OtherKeyField
having sum(OtherAilas.AnyNonKeyField)     > 100
   and avg(OtherAlias.ThirdNonKeyField)   < 42
   and max(ThirdAlias.FourthNonKeyField) >= 'Z'

union all
  select from I_AnyEntity2         as AnyAlias

    inner join I_OtherEntity as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField
    left outer to one join I_ThirdEntity as ThirdAlias
      on  AnyAlias.IdField    = ThirdAlias.IdField
      and AnyAlias.SubIdField = ThirdAlias.SubIdField

  association [0..*] to I_FourthEntity as _FourthAlias
    on AnyAlias.IdField = _FourthAlias.IdField
  // comment on association
  association [0..*] to I_FifthEntity as _FifthAlias
    on AnyAlias.IdField = _FifthAlias.IdField

{
  key AnyAlias.AnyKeyField,

  key AnyAlias.OtherKeyField,

      OtherAlias.AnyNonKeyField
}
```

Resulting code:

```ASDDLS

@EndUserText.label: 'Any Description'
define view entity C_AnyEntity
  with parameters
    P_AnyParam:   any_parameter_type,

    @Annotation.subAnno: 'value'
    P_OtherParam: other_type

  as select from I_AnyEntity as AnyAlias
    inner join I_OtherEntity2      as OtherAlias on AnyAlias.IdField = OtherAlias.IdField

    left outer join I_ThirdEntity2 as ThirdAlias on  AnyAlias.IdField      = ThirdAlias.IdField
                                                 and AnyAlias.SubIdField   = ThirdAlias.SubIdField
                                                 and AnyAlias.ThirdIdField = ThirdAlias.ThirdIdField

  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField
  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField
  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField

{
  key AnyAlias.AnyKeyField,
  key AnyAlias.OtherKeyField,

      sum(OtherAlias.AnyNonKeyField),

      @Annotation.subAnno: 'value'
      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField
      @<Annotation.otherSubAnno: 'this annotation refers to the previous element!',

      max(OtherAlias.ThirdNonKeyField)  as ThirdNonKeyField,
      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,
      max(ThirdAlias.FifthNonKeyField)  as FifthNonKeyField,

      _FourthAlias,
      _FifthAlias,
      _SixthAlias
}
where  AnyNonKeyField  > 10
  and OtherNonKeyField = 'A'

group by AnyAlias.AnyKeyField
         AnyAlias.OtherKeyField

having sum(OtherAilas.AnyNonKeyField)     > 100
   and avg(OtherAlias.ThirdNonKeyField)   < 42
   and max(ThirdAlias.FourthNonKeyField) >= 'Z'

union all
  select from I_AnyEntity2         as AnyAlias

    inner join I_OtherEntity as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField

    left outer to one join I_ThirdEntity as ThirdAlias
      on  AnyAlias.IdField    = ThirdAlias.IdField
      and AnyAlias.SubIdField = ThirdAlias.SubIdField

  association [0..*] to I_FourthEntity as _FourthAlias
    on AnyAlias.IdField = _FourthAlias.IdField

  // comment on association
  association [0..*] to I_FifthEntity as _FifthAlias
    on AnyAlias.IdField = _FifthAlias.IdField

{
  key AnyAlias.AnyKeyField,
  key AnyAlias.OtherKeyField,

      OtherAlias.AnyNonKeyField
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/emptylines/DdlEmptyLinesWithinSectionsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/emptylines/DdlEmptyLinesWithinSectionsTest.java)

