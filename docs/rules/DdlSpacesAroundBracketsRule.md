[<-- previous rule](DdlSpacesAroundSignsRule.md) | [overview](../rules.md) | [next rule -->](DdlCamelCaseNameRule.md)

# Standardize spaces around brackets

Standardizes spaces around brackets \[...\] and parentheses \(...\).

Spaces in annotations are handled in 'Standardize annotation layout'.

## Options

* Spaces around brackets for cardinality: \[Always\]
* Spaces inside brackets for cardinality: \[Never\]
* Spaces before brackets for path expressions: \[Never\]
* Spaces inside brackets for path expressions: \[Never\]
* Spaces before parentheses for functions: \[Never\]
* Spaces inside parentheses for functions: \[Never\]
* Spaces before parentheses for ABAP types: \[Never\]
* Spaces inside parentheses for ABAP types: \[Never\]
* Spaces inside parentheses in arithmetics: \[Never\]

## Examples


```ASDDLS

// spaces in annotations are handled in 'Standardize annotation layout':
@Annotation: { anySubAnno: 'value', otherSubAnno: 'value' }

define view C_AnyView
  as select from I_AnyView

  // associations with cardinality
  association[1..* ] to I_OtherView as _OtherAlias
    on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField

  association [ *]to I_ThirdView as _ThirdAlias
    on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField

{
  key AnyKeyField,

      // path expressions
      _OtherAlias [1:AnyValue > 0]._AnyAssoc[ *: OtherValue = 42 ].AnyField as AnyField,
      _ThirdAlias[ inner where i = '2']._Text [ 1: Language = $session.system_language].AnyName as AnyName,

      // built-in functions
      concat ( AnyText, concat( '_', OtherText)) as AnyTextField,
      division(AnyArg *10, OtherArg, 2) as ThirdValue,
      round( AnyValue / 100, 5 ) as RoundedValue,

      // casts and ABAP types
      cast (  AnyAmount as abap.curr( 23,2)) as AnyAmount,
      cast(AnyQuantity as abap.quan (18, 6 ) ) as AnyQuantity,

      // arithmetic expressions
      ( AnyValue + OtherValue) as AnyValueField,
      3 * (-2 * AnyValue - 4 * (OtherValue - 1) ) as OtherValueField
}
```

Resulting code:

```ASDDLS

// spaces in annotations are handled in 'Standardize annotation layout':
@Annotation: { anySubAnno: 'value', otherSubAnno: 'value' }

define view C_AnyView
  as select from I_AnyView

  // associations with cardinality
  association [1..*] to I_OtherView as _OtherAlias
    on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField

  association [*] to I_ThirdView as _ThirdAlias
    on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField

{
  key AnyKeyField,

      // path expressions
      _OtherAlias[1:AnyValue > 0]._AnyAssoc[*: OtherValue = 42].AnyField as AnyField,
      _ThirdAlias[inner where i = '2']._Text[1: Language = $session.system_language].AnyName as AnyName,

      // built-in functions
      concat(AnyText, concat('_', OtherText)) as AnyTextField,
      division(AnyArg *10, OtherArg, 2) as ThirdValue,
      round(AnyValue / 100, 5) as RoundedValue,

      // casts and ABAP types
      cast(AnyAmount as abap.curr(23,2)) as AnyAmount,
      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity,

      // arithmetic expressions
      (AnyValue + OtherValue) as AnyValueField,
      3 * (-2 * AnyValue - 4 * (OtherValue - 1)) as OtherValueField
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/spaces/DdlSpacesAroundBracketsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/spaces/DdlSpacesAroundBracketsTest.java)

