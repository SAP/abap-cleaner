[<-- previous rule](DdlTypoRule.md) | [overview](../rules.md) | [next rule -->](DdlAlignSourceParametersRule.md)

# Align view parameters

Aligns the parameter definitions of a view.

Parameter indent is configured in 'Break before DEFINE etc.'. Space around colons \(if colons are not aligned\) is configured in 'Standardize spaces around colon, comma etc.'.

## Options

* \[X\] Align colons in own column
* \[X\] Align types in own column

## Examples


```ASDDLS

@EndUserText.label: 'Any Description'

define view entity C_AnyEntity
  with parameters
    @AnalyticsDetails.query.variableSequence: 30
    P_AnyParam: any_parameter_type,

    // comment on OtherParam
    @AnalyticsDetails.query.variableSequence: 50
    P_OtherParam :           other_type,

    @AnalyticsDetails.query.variableSequence: 60
    @Consumption.defaultValue: 'NN'
    P_ThirdParameter   : third_type,

    @Consumption.hidden: true
    P_FourthParam:fourth_type

  as select from I_AnyEntity as AnyAlias

    left outer to one join I_OtherEntity as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField

{
  key AnyAlias.AnyKeyField,
      OtherAlias.AnyNonKeyField
}
```

Resulting code:

```ASDDLS

@EndUserText.label: 'Any Description'

define view entity C_AnyEntity
  with parameters
    @AnalyticsDetails.query.variableSequence: 30
    P_AnyParam       : any_parameter_type,

    // comment on OtherParam
    @AnalyticsDetails.query.variableSequence: 50
    P_OtherParam     : other_type,

    @AnalyticsDetails.query.variableSequence: 60
    @Consumption.defaultValue: 'NN'
    P_ThirdParameter : third_type,

    @Consumption.hidden: true
    P_FourthParam    : fourth_type

  as select from I_AnyEntity as AnyAlias

    left outer to one join I_OtherEntity as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField

{
  key AnyAlias.AnyKeyField,
      OtherAlias.AnyNonKeyField
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignEntityParametersRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignEntityParametersTest.java)

