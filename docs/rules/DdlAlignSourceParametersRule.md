[<-- previous rule](DdlAlignEntityParametersRule.md) | [overview](../rules.md) | [next rule -->](DdlAlignFunctionParametersRule.md)

# Align source parameters

Aligns the parameters supplied to a data source.

Space inside parentheses is configured in 'Standardize spaces around brackets'. Space around colons \(if colons are not aligned\) is configured in 'Standardize spaces around colon, comma etc.'.

## Options

* Position of parameters \[below view name \+ 2\]
* \[X\] Align colons in own column
* \[X\] Align actual parameters in own column
* Position of AS <alias> \[continue after \)\]

## Examples


```ASDDLS

define view entity I_AnyEntity
  with parameters
    P_AnyParam       : any_parameter_type,
    P_OtherParam     : other_type,
    P_ThirdParameter : third_type,
    P_FourthParam    : fourth_type

  as select from I_OtherEntity
  (
   P_AnyParam : $parameters.P_AnyParam,
     P_OtherParam    :   $parameters.P_OtherParam,
    // comment
 P_ThirdParameter        :
    $parameters.P_ThirdParameter,
  P_FourthParam
  : $parameters.P_FourthParam 
 )
   as OtherAlias

    inner join I_ThirdEntity
          (P_AnyParam  :  $parameters.P_AnyParam,
//        P_OtherParam : $parameters.P_OtherParam,
//        P_ThirdParam:$parameters.P_ThirdParameter,
          P_FourthParameter: $parameters.P_FourthParam)
          as ThirdAlias
      on OtherAlias.AnyKeyField = ThirdAlias.AnyKeyField
{
  key OtherAlias.AnyKeyField,
      ThirdAlias.AnyNonKeyField
}
union all
  select from I_FourthEntity( P_AnyParam   :
         $parameters.P_AnyParam, P_OtherParam        :
         $parameters.P_OtherParam, P_ThirdParameter :
         $parameters.P_ThirdParameter, P_FourthParam:
         $parameters.P_FourthParam  ) as FourthAlias

{
  key FourthAlias.AnyKeyField,
      FourthAlias.AnyNonKeyField
}
```

Resulting code:

```ASDDLS

define view entity I_AnyEntity
  with parameters
    P_AnyParam       : any_parameter_type,
    P_OtherParam     : other_type,
    P_ThirdParameter : third_type,
    P_FourthParam    : fourth_type

  as select from I_OtherEntity(
                   P_AnyParam       : $parameters.P_AnyParam,
                   P_OtherParam     : $parameters.P_OtherParam,
                   // comment
                   P_ThirdParameter : $parameters.P_ThirdParameter,
                   P_FourthParam    : $parameters.P_FourthParam) as OtherAlias

    inner join I_ThirdEntity(
                 P_AnyParam        : $parameters.P_AnyParam,
//                 P_OtherParam      : $parameters.P_OtherParam,
//                 P_ThirdParam      : $parameters.P_ThirdParameter,
                 P_FourthParameter : $parameters.P_FourthParam) as ThirdAlias
      on OtherAlias.AnyKeyField = ThirdAlias.AnyKeyField
{
  key OtherAlias.AnyKeyField,
      ThirdAlias.AnyNonKeyField
}
union all
  select from I_FourthEntity(
                P_AnyParam       : $parameters.P_AnyParam,
                P_OtherParam     : $parameters.P_OtherParam,
                P_ThirdParameter : $parameters.P_ThirdParameter,
                P_FourthParam    : $parameters.P_FourthParam) as FourthAlias

{
  key FourthAlias.AnyKeyField,
      FourthAlias.AnyNonKeyField
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignSourceParametersRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignSourceParametersTest.java)

