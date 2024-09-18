[<-- previous rule](DdlAlignFieldListsRule.md) | [overview](../rules.md) | [next rule -->](DdlAlignSelectListRule.md)

# Align JOINs and ASSOCIATIONs

Aligns the data sources, aliases and ON conditions of SELECT FROM, JOINs and ASSOCIATIONs.

Whether to break ON conditions to the next line is configured in 'Break before JOINs'.

## Options

* \[X\] Align data sources
* \[X\] Align aliases
* \[X\] Align ON conditions that continue on the same line
* \[ \] Align ASSOCIATIONs together with JOINs
* \[ \] Consider all lines of multi-line parameter assignments

## Examples


```ASDDLS

// Depending on configuration in 'Break before JOINs',
// ON conditions were either moved to the next line ...
define view I_AnyView
  as select from I_AnySource as AnyAlias

    left outer join I_OtherSource as OtherAlias
      on AnyAlias.AnyField = OtherAlias.AnyField

    inner join I_ThirdSourceWithLongName as ThirdAliasWithLongName
      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField
      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField

    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'
                                           P_OtherParam : 42 ) as FourthAlias
      on AnyAlias.AnyField = FourthAlias.OtherField

  association [0..*] to I_FifthSource as _FifthAlias
     on _FifthAlias.AnyField = AnyAlias.AnyField

  association [0..1] to I_SixthSourceWithLongName as _SixthAlias
     on  _SixthAlias.AnyField   = AnyAlias.AnyField
     and _SixthAlias.OtherField = AnyAlias.OtherField
     and _SixthAlias.ThirdField = ThirdAlias.ThirdField

{
  key AnyAlias.AnyField,
      OtherAlias.OtherField,
      ThirdAlias.ThirdField,
      FourthAlias.FourthField
}

// ... or the ON conditions continue after the alias:
union all
  select from I_AnySource2 as AnyAlias

    left outer join I_OtherSource2 as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField

    inner join I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField
                                                                    and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField

    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'
                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField

  association [0..*] to I_FifthSource2 as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField

  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField
                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField
                                                                  and _SixthAlias.ThirdField = ThirdAlias.ThirdField

{
  key AnyAlias.AnyField,
      OtherAlias.OtherField,
      ThirdAlias.ThirdField,
      FourthAlias.FourthField
}
```

Resulting code:

```ASDDLS

// Depending on configuration in 'Break before JOINs',
// ON conditions were either moved to the next line ...
define view I_AnyView
  as select from           I_AnySource                         as AnyAlias

    left outer join        I_OtherSource                       as OtherAlias
      on AnyAlias.AnyField = OtherAlias.AnyField

    inner join             I_ThirdSourceWithLongName           as ThirdAliasWithLongName
      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField
      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField

    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'
                                           P_OtherParam : 42 ) as FourthAlias
      on AnyAlias.AnyField = FourthAlias.OtherField

  association [0..*] to I_FifthSource             as _FifthAlias
     on _FifthAlias.AnyField = AnyAlias.AnyField

  association [0..1] to I_SixthSourceWithLongName as _SixthAlias
     on  _SixthAlias.AnyField   = AnyAlias.AnyField
     and _SixthAlias.OtherField = AnyAlias.OtherField
     and _SixthAlias.ThirdField = ThirdAlias.ThirdField

{
  key AnyAlias.AnyField,
      OtherAlias.OtherField,
      ThirdAlias.ThirdField,
      FourthAlias.FourthField
}

// ... or the ON conditions continue after the alias:
union all
  select from              I_AnySource2                         as AnyAlias

    left outer join        I_OtherSource2                       as OtherAlias             on AnyAlias.AnyField = OtherAlias.AnyField

    inner join             I_ThirdSourceWithLongName2           as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField
                                                                                          and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField

    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'
                                            P_OtherParam : 42 ) as FourthAlias            on AnyAlias.AnyField = FourthAlias.OtherField

  association [0..*] to I_FifthSource2             as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField

  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField
                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField
                                                                  and _SixthAlias.ThirdField = ThirdAlias.ThirdField

{
  key AnyAlias.AnyField,
      OtherAlias.OtherField,
      ThirdAlias.ThirdField,
      FourthAlias.FourthField
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignDataSourcesRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignDataSourcesTest.java)

