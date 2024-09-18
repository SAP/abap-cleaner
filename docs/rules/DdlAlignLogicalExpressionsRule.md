[<-- previous rule](DdlAlignFunctionParametersRule.md) | [overview](../rules.md) | [next rule -->](DdlAlignFieldListsRule.md)

# Align logical expressions in views

Aligns logical expressions in ON / WHERE / HAVING conditions and path expressions.

Space inside parentheses is configured in 'Standardize spaces around brackets'.

## Options

* \[X\] Align ON condition in JOINs
* \[X\] Align ON conditions in ASSOCIATIONs
* \[X\] Align WHEN condition in complex CASE distinctions
* \[X\] Align path expressions
* \[X\] Align WHERE clause
* \[X\] Align HAVING clause
* Align AND / OR with ON \[left-align\]
* Align AND / OR with FILTER \[right-align\]
* Align AND / OR with WHEN \[right-align\]
* Align AND / OR with WHERE \[right-align\]
* Align AND / OR with HAVING \[right-align\]
* \[X\] Right-align comparison operators / IS
* Do not align if more than \[20\] inner spaces would be required

## Examples


```ASDDLS

define view entity I_AnyEntity
  as select from I_AnySource as AnyAlias

    inner join I_OtherSource as OtherAlias
      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField
         and ( OtherAlias.AnyType = 'X' or
      OtherAlias.AnyType = 'Y' )

  association [1..*] to I_ThirdSource  as _ThirdSource
    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField and
    OtherAlias.OtherKeyField = _ThirdSource.OtherKeyField
    and OtherAlias.NumericField >= _ThirdSource.NumericField

{
  key AnyAlias.AnyKeyField,
  key OtherAlias.OtherKeyField,
      OtherAlias.AnyType,

      case when AnyAlias.Category = 'A' then 'category A'
           when AnyAlias.Category = 'B' or
      AnyAlias.Category = 'C' then 'category B or C'
      end as CategoryText,

      sum(OtherAlias.NumericField) as SumField,

      max(_ThirdSource[1:SubCategory = 'X' or
      SubCategory = 'Y'
      or SubCategory = 'Z'].NumericField) as MaxNumericField

}
where
OtherAlias.OtherKeyField > 'NN' and
OtherAlias.NumericField > 100
and ( AnyAlias.Category = 'A' or
AnyAlias.Category = 'B'
or AnyAlias.Category = 'C')

group by AnyAlias.AnyKeyField,
         OtherAlias.OtherKeyField,
         OtherAlias.AnyType,
         AnyAlias.Category

having AnyAlias.Category = 'A' and
  avg(OtherAlias.NumericField) >= 200 or
    AnyAlias.Category = 'B'
  and sum(OtherAlias.NumericField) >= 1000 and
sum(OtherAlias.NumericField) < 5000
```

Resulting code:

```ASDDLS

define view entity I_AnyEntity
  as select from I_AnySource as AnyAlias

    inner join I_OtherSource as OtherAlias
      on  OtherAlias.AnyKeyField = AnyAlias.AnyKeyField
      and (   OtherAlias.AnyType = 'X'
           or OtherAlias.AnyType = 'Y')

  association [1..*] to I_ThirdSource  as _ThirdSource
    on  AnyAlias.AnyKeyField      = _ThirdSource.AnyKeyField
    and OtherAlias.OtherKeyField  = _ThirdSource.OtherKeyField
    and OtherAlias.NumericField  >= _ThirdSource.NumericField

{
  key AnyAlias.AnyKeyField,
  key OtherAlias.OtherKeyField,
      OtherAlias.AnyType,

      case when AnyAlias.Category = 'A' then 'category A'
           when AnyAlias.Category = 'B'
             or AnyAlias.Category = 'C' then 'category B or C'
      end as CategoryText,

      sum(OtherAlias.NumericField) as SumField,

      max(_ThirdSource[1:    SubCategory = 'X'
                          or SubCategory = 'Y'
                          or SubCategory = 'Z'].NumericField) as MaxNumericField

}
where OtherAlias.OtherKeyField > 'NN'
  and OtherAlias.NumericField  > 100
  and (   AnyAlias.Category = 'A'
       or AnyAlias.Category = 'B'
       or AnyAlias.Category = 'C')

group by AnyAlias.AnyKeyField,
         OtherAlias.OtherKeyField,
         OtherAlias.AnyType,
         AnyAlias.Category

having     AnyAlias.Category             = 'A'
       and avg(OtherAlias.NumericField) >= 200
    or     AnyAlias.Category             = 'B'
       and sum(OtherAlias.NumericField) >= 1000
       and sum(OtherAlias.NumericField)  < 5000
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignLogicalExpressionsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignLogicalExpressionsTest.java)

