[<-- previous rule](DdlPositionBracesRule.md) | [overview](../rules.md) | [next rule -->](DdlSpacesAroundSignsRule.md)

# Break before WHERE clause etc.

Standardizes line breaks and indentation of keywords that start the clauses WHERE, GROUP BY, HAVING, as well as EXCEPT, INTERSECT and UNION.

## Options

* Break before WHERE / GROUP BY / HAVING: \[Always\]
* Indent if breaking: \[0\] 
* Break before EXCEPT / INTERSECT / UNION: \[Always\]
* Indent if breaking: \[0\] 
* The position of SELECT FROM after a UNION etc. can be changed with the rule 'Break before AS SELECT etc.'

## Examples


```ASDDLS

define view entity C_AnyEntity
  as select from I_AnyEntity as AnyAlias

{
  key AnyAlias.AnyKeyField          as AnyKey,
      sum(AnyAlias.AnyNumericField) as AnySum,
      AnyAlias.UnitField            as Unit
}

where AnyAlias.AnyConditionField = 'X'
  and AnyAlias.AnyCategory       = 'A' group
by AnyAlias.AnyKeyField,
   AnyAlias.AnyNumericField,
   AnyAlias.UnitField having AnyAlias.AnyNumericField > 100

union all
  select from I_OtherEntity as OtherAlias

{
  key OtherAlias.OtherKeyField          as AnyKey,
      sum(OtherAlias.OtherNumericField) as AnySum,
      OtherAlias.OtherUnitField         as Unit
} where OtherAlias.OtherCategory = 'A'
     or OtherAlias.OtherCategory = 'B'

group by OtherAlias.OtherKeyField,
         OtherAlias.OtherNumericField,
         OtherAlias.OtherUnitField

```

Resulting code:

```ASDDLS

define view entity C_AnyEntity
  as select from I_AnyEntity as AnyAlias

{
  key AnyAlias.AnyKeyField          as AnyKey,
      sum(AnyAlias.AnyNumericField) as AnySum,
      AnyAlias.UnitField            as Unit
}

where AnyAlias.AnyConditionField = 'X'
  and AnyAlias.AnyCategory       = 'A'
group by AnyAlias.AnyKeyField,
         AnyAlias.AnyNumericField,
         AnyAlias.UnitField
having AnyAlias.AnyNumericField > 100

union all
  select from I_OtherEntity as OtherAlias

{
  key OtherAlias.OtherKeyField          as AnyKey,
      sum(OtherAlias.OtherNumericField) as AnySum,
      OtherAlias.OtherUnitField         as Unit
}
where OtherAlias.OtherCategory = 'A'
   or OtherAlias.OtherCategory = 'B'

group by OtherAlias.OtherKeyField,
         OtherAlias.OtherNumericField,
         OtherAlias.OtherUnitField
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/position/DdlPositionClausesRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/position/DdlPositionClausesTest.java)

