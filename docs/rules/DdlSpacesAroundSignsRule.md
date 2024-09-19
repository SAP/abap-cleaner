[<-- previous rule](DdlPositionClausesRule.md) | [overview](../rules.md) | [next rule -->](DdlSpacesAroundBracketsRule.md)

# Standardize spaces around colon, comma etc.

Standardizes spaces around comment signs, colons, commas and arithmetic operators.

Spaces in annotations are handled in 'Standardize annotation layout'.

## Options

* Space before comment sign: \[Always\]
* Space after comment sign: \[Always\]
* Space before colon: \[Keep as is\]
* Space after colon: \[Always\]
* Space before comma: \[Never\]
* Space after comma: \[Always\]
* Space after comma in ABAP type: \[Never\]
* Spaces around arithmetic operators: \[Always\]

## Examples


```ASDDLS

// colons and commas in annotations are not changed by this rule
// but handled in 'Standardize annotation layout':
@Annotation: { anySubAnno: 'value', otherSubAnno: 'value' }

define view C_AnyView
  with parameters
    P_AnyParam  : any_type    ,
    P_OtherParam:other_type

  --colons that start parameter names :P_Any must not be detached
  as select from I_AnyView(
    P_AnyParam  ::P_AnyParam ,
    P_OtherParam: :P_OtherParam ) as AnyAlias

  --no spaces can be put around the * that specifies cardinality
  association [1..*] to I_OtherView as _OtherAlias
    on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField

{
  key AnyKeyField   ,    //any comment
  key OtherKeyField ,-- other comment

      //arithmetic expressions
      AnyValue*2 +OtherValue* 4- ThirdValue as AnyValueField,
      3*(-2*AnyValue- 4*OtherValue) as OtherValueField,

      -- built-in functions
      concat(AnyText,concat('_' ,OtherText)) as AnyTextField,
      division(AnyArg *10,OtherArg,2) as ThirdValue,
      round(AnyValue* 100, 5) as RoundedValue,

      //you may format commas in ABAP types in a different way,
      // because these are not built-in functions
      cast(AnyAmount   as abap.curr(23 ,2)) as AnyAmount  ,
      cast(OtherAmount as abap.curr(23,2)) as OtherAmount ,
      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity,
}
```

Resulting code:

```ASDDLS

// colons and commas in annotations are not changed by this rule
// but handled in 'Standardize annotation layout':
@Annotation: { anySubAnno: 'value', otherSubAnno: 'value' }

define view C_AnyView
  with parameters
    P_AnyParam  : any_type,
    P_OtherParam: other_type

  -- colons that start parameter names :P_Any must not be detached
  as select from I_AnyView(
    P_AnyParam  : :P_AnyParam,
    P_OtherParam: :P_OtherParam ) as AnyAlias

  -- no spaces can be put around the * that specifies cardinality
  association [1..*] to I_OtherView as _OtherAlias
    on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField

{
  key AnyKeyField,    // any comment
  key OtherKeyField, -- other comment

      // arithmetic expressions
      AnyValue * 2 + OtherValue * 4 - ThirdValue as AnyValueField,
      3 * (-2 * AnyValue - 4 * OtherValue) as OtherValueField,

      -- built-in functions
      concat(AnyText, concat('_', OtherText)) as AnyTextField,
      division(AnyArg * 10, OtherArg, 2) as ThirdValue,
      round(AnyValue * 100, 5) as RoundedValue,

      // you may format commas in ABAP types in a different way,
      // because these are not built-in functions
      cast(AnyAmount   as abap.curr(23,2)) as AnyAmount,
      cast(OtherAmount as abap.curr(23,2)) as OtherAmount,
      cast(AnyQuantity as abap.quan(18,6)) as AnyQuantity,
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/spaces/DdlSpacesAroundSignsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/spaces/DdlSpacesAroundSignsTest.java)

