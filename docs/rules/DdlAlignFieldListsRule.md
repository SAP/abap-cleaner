[<-- previous rule](DdlAlignLogicalExpressionsRule.md) | [overview](../rules.md) | [next rule -->](DdlAlignDataSourcesRule.md)

# Align name list and GROUP BY list

Aligns name lists of DDIC-based views and GROUP BY field lists.

## Options

* Maximum line length \[120\] 
* Name list position: \[below line start \+ 2\]
* Name lists layout: \[multi-line\]
* GROUP BY list position: \[continue after GROUP BY\]
* GROUP BY lists with complex fields: \[multi-line\]
* GROUP BY lists with simple fields only: \[multi-line\]
* \[X\] Consider 'Alias.Field' as complex

## Examples


```ASDDLS

define view I_AnyDDicBasedView
 (
  AnyFieldAlias,
OtherFieldAlias,
 ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias
    )

  as select from  I_AnySource as AnyAlias

    left outer join I_OtherSource as OtherAlias
      on AnyAlias.AnyField = OtherAlias.AnyField

  association [0..*] to I_ThirdSource as _ThirdAlias
     on _ThirdAlias.OtherField = AnyAlias.OtherField

{
  key AnyAlias.AnyField,
      OtherAlias.OtherField,
//      OtherAlias.CommentedOutField,
      OtherAlias.ThirdField,
      _ThirdAlias.FourthFieldWithLongName,
      _ThirdAlias._FourthAlias.FifthField,
}
group by
       AnyAlias.AnyField,
     OtherAlias.OtherField,
// OtherAlias.CommentedOutField,
 OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,
      _ThirdAlias._FourthAlias.FifthField
```

Resulting code:

```ASDDLS

define view I_AnyDDicBasedView
(
  AnyFieldAlias,
  OtherFieldAlias,
  ThirdFieldAlias,
  FourthFieldAliasWithLongName,
  FifthFieldAlias
)

  as select from  I_AnySource as AnyAlias

    left outer join I_OtherSource as OtherAlias
      on AnyAlias.AnyField = OtherAlias.AnyField

  association [0..*] to I_ThirdSource as _ThirdAlias
     on _ThirdAlias.OtherField = AnyAlias.OtherField

{
  key AnyAlias.AnyField,
      OtherAlias.OtherField,
//      OtherAlias.CommentedOutField,
      OtherAlias.ThirdField,
      _ThirdAlias.FourthFieldWithLongName,
      _ThirdAlias._FourthAlias.FifthField,
}
group by AnyAlias.AnyField,
         OtherAlias.OtherField,
//         OtherAlias.CommentedOutField,
         OtherAlias.ThirdField,
         _ThirdAlias.FourthFieldWithLongName,
         _ThirdAlias._FourthAlias.FifthField
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignFieldListsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignFieldListsTest.java)

