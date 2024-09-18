[<-- previous rule](DdlAnnotationNestingRule.md) | [overview](../rules.md) | [next rule -->](DdlPositionSelectRule.md)

# Break before DEFINE etc.

Standardizes line breaks and indentation before DEFINE / EXTEND etc. keywords, entity name and WITH PARAMETERS.

## Options

* Break before DEFINE etc. keywords: \[Always\]
* Indent if breaking: \[0\] 
* Break before entity name: \[Never\]
* Indent if breaking: \[2\] 
* Break before WITH PARAMETERS: \[Always\]
* Indent if breaking: \[2\] 
* Indent of parameters: \[4\] 

## Examples


```ASDDLS

@EndUserText.label: 'Any Description' define
   view
 entity
C_AnyEntity with
parameters
// comment
P_AnyParam   : AnyType,
  P_OtherParam : OtherType,
    P_ThirdParam : ThirdType

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
    // comment
    P_AnyParam   : AnyType,
    P_OtherParam : OtherType,
    P_ThirdParam : ThirdType

  as select from I_AnyEntity as AnyAlias

    left outer to one join I_OtherEntity as OtherAlias
      on AnyAlias.IdField = OtherAlias.IdField

{
  key AnyAlias.AnyKeyField,
      OtherAlias.AnyNonKeyField
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/position/DdlPositionDefineRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/position/DdlPositionDefineTest.java)

