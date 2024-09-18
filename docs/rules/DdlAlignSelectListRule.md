[<-- previous rule](DdlAlignDataSourcesRule.md) | [overview](../rules.md) | [next rule -->](DdlEmptyLinesBetweenSectionsRule.md)

# Align select list

Aligns key and non-key elements in the select list and their aliases.

## Options

* \[X\] Align keyword KEY / VIRTUAL
* \[X\] Align aliases
* \[X\] Consider simple elements without alias \(e.g. 'AnySource.AnyField'\) for alias position
* \[ \] Consider complex elements without alias \(but with calculations or 2\+ dots\) for alias position
* \[ \] Consider all lines of multi-line elements for alias position
* Maximum start position of aligned aliases: \[120\]  - Aliases that would start beyond this position are moved to the next line.
* \[X\] Align textual comments from line start with elements
* \[X\] Align commented-out code with active code

## Examples


```ASDDLS

define view I_AnyView
  as select from I_AnySource as AnyAlias

    left outer join I_OtherSource as OtherAlias
      on AnyAlias.AnyField = OtherAlias.AnyField

  association [0..*] to I_ThirdSource as _ThirdAlias
     on _ThirdAlias.AnyField = AnyAlias.AnyField

{
   @ObjectModel.text.element: ['ThirdFieldAlias']
  key AnyAlias.AnyField as AnyFieldAlias,
 key   AnyAlias.OtherField as OtherFieldAlias,

      // non-key fields
   OtherAlias.ThirdField as ThirdFieldAlias,
    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,
       OtherAlias.FifthField
         as FifthFieldAlias,
  // complex element with multiple dots:
         _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,

// calculated fields
// A textual comment like this one should not be at line start, but should be aligned with the elements.
// Only commented-out code should have comment signs at line start, so Ctrl+> can easily uncomment it.
 cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,

//   // Commented-out select list elements are also being aligned, as shown below. For this to work,
//     // - their comment signs must be at the very start of the line (as Ctrl+< would produce them), and
//    // - the commented-out element must not span multiple comment lines:
// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,
//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,
//  OtherAlias.CommentedOutFieldWithoutAlias,

    // associations
 _ThirdAlias
}
union all
  select from I_AnySource2 as AnyAlias

    left outer join I_OtherSource2 as OtherAlias
      on AnyAlias.AnyField = OtherAlias.AnyField

  association [0..*] to I_ThirdSource2 as _ThirdAliasWithVeryLongName
     on _ThirdAliasWithVeryLongName.AnyField = AnyAlias.AnyField

{
   // key fields
  key AnyAlias.AnyField     as AnyFieldAlias,
 key   AnyAlias.OtherField as OtherFieldAlias,

      // non-key fields
   OtherAlias.ThirdField as ThirdFieldAlias,
    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,
       OtherAlias.FifthField        as FifthFieldAlias,
         _ThirdAliasWithVeryLongName.FieldNameThatGetsNoAlias,
         _ThirdAliasWithVeryLongName._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,

// calculated fields
 cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthFieldWithLongName
      + OtherAlias.SixthField as any_type) as CalculatedField,

// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,
//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,
//  OtherAlias.CommentedOutFieldWithoutAlias,

    // associations
 _ThirdAliasWithVeryLongName
}
```

Resulting code:

```ASDDLS

define view I_AnyView
  as select from I_AnySource as AnyAlias

    left outer join I_OtherSource as OtherAlias
      on AnyAlias.AnyField = OtherAlias.AnyField

  association [0..*] to I_ThirdSource as _ThirdAlias
     on _ThirdAlias.AnyField = AnyAlias.AnyField

{
      @ObjectModel.text.element: ['ThirdFieldAlias']
  key AnyAlias.AnyField                                                                                    as AnyFieldAlias,
  key AnyAlias.OtherField                                                                                  as OtherFieldAlias,

      // non-key fields
      OtherAlias.ThirdField                                                                                as ThirdFieldAlias,
      OtherAlias.FourthFieldWithLongName                                                                   as FourthFieldAlias,
      OtherAlias.FifthField                                                                                as FifthFieldAlias,
      // complex element with multiple dots:
      _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,

      // calculated fields
      // A textual comment like this one should not be at line start, but should be aligned with the elements.
      // Only commented-out code should have comment signs at line start, so Ctrl+> can easily uncomment it.
      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,

//      // Commented-out select list elements are also being aligned, as shown below. For this to work,
//      // - their comment signs must be at the very start of the line (as Ctrl+< would produce them), and
//      // - the commented-out element must not span multiple comment lines:
//  key OtherAlias.CommentedOutField                                                                         as CommentedOutFieldAlias,
//      OtherAlias.OtherCommentedOutField                                                                    as OtherCommentedOutFieldAlias,
//      OtherAlias.CommentedOutFieldWithoutAlias,

      // associations
      _ThirdAlias
}
union all
  select from I_AnySource2 as AnyAlias

    left outer join I_OtherSource2 as OtherAlias
      on AnyAlias.AnyField = OtherAlias.AnyField

  association [0..*] to I_ThirdSource2 as _ThirdAliasWithVeryLongName
     on _ThirdAliasWithVeryLongName.AnyField = AnyAlias.AnyField

{
      // key fields
  key AnyAlias.AnyField                                     as AnyFieldAlias,
  key AnyAlias.OtherField                                   as OtherFieldAlias,

      // non-key fields
      OtherAlias.ThirdField                                 as ThirdFieldAlias,
      OtherAlias.FourthFieldWithLongName                    as FourthFieldAlias,
      OtherAlias.FifthField                                 as FifthFieldAlias,
      _ThirdAliasWithVeryLongName.FieldNameThatGetsNoAlias,
      _ThirdAliasWithVeryLongName._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,

      // calculated fields
      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthFieldWithLongName
           + OtherAlias.SixthField as any_type)             as CalculatedField,

//  key OtherAlias.CommentedOutField                          as CommentedOutFieldAlias,
//      OtherAlias.OtherCommentedOutField                     as OtherCommentedOutFieldAlias,
//      OtherAlias.CommentedOutFieldWithoutAlias,

      // associations
      _ThirdAliasWithVeryLongName
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignSelectListRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignSelectListTest.java)

