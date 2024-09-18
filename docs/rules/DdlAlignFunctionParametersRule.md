[<-- previous rule](DdlAlignSourceParametersRule.md) | [overview](../rules.md) | [next rule -->](DdlAlignLogicalExpressionsRule.md)

# Align function parameters after =>

Aligns the parameters of special built-in functions for currency and unit conversion, and of analytical scalar functions.

Space inside parentheses is configured in 'Standardize spaces around brackets'.

## Options

* Position of parameters \[continue after \(\]
* \[X\] Align => in own column
* \[X\] Align actual parameters in own column

## Examples


```ASDDLS

define view entity I_AnyEntity
  with parameters
    P_DisplayCurrency     : vdm_v_display_currency,
    P_ExchangeRateType    : kurst,
    P_ExchangeRateDate    : vdm_v_exchange_rate_date,
    P_DisplayUnit         : meins

  as select from I_AnySource as AnyAlias

    inner join I_OtherSource as OtherAlias
      on AnyAlias.AnyKeyField = OtherAlias.AnyKeyField

  association to I_ThirdSource  as _PreviousPeriod
    on _PreviousPeriod.FiscalYearPeriod = fiscal_calendar_shift(
                                                               fiscal_variant => $projection.FiscalYearVariant,
                                                               base => $projection.FiscalYearPeriod,
                                                               base_level => CALENDAR_FISCAL_LEVEL.#period, shift => abap.int2'-1'
                                                                )

{
  key AnyAlias.AnyKeyField,
  key AnyAlias.FiscalYearVariant,
  key AnyAlias.FiscalYearPeriod,

      // special built-in functions
      currency_conversion(
       amount => AnyAlias.Amount,
        source_currency =>AnyAlias.Currency,
     target_currency =>$parameters.P_DisplayCurrency,
      exchange_rate_type=> $parameters.P_ExchangeRateType,
        exchange_rate_date=>
          $parameters.P_ExchangeRateDate
//    round   =>'X',
// error_handling => 'SET_TO_NULL'
    ) as AmountInDisplayCurrency,

      unit_conversion
       (quantity => AnyAlias.Quantity, source_unit =>
        AnyAlias.QuantityUnit, target_unit=> $parameters.P_DisplayUnit)

      // analytical scalar functions
      power(base=> power(base=> OtherAlias.AnyNumericValue,
      exponent => 3), exponent => 2) as ValueCubeSquare,

      ratio_of( portion  =>
(  OtherAlias.TotalAmount
 - OtherAlias.TotalAmount * OtherAlias.Discount) * OtherAlias.Tax,
total =>OtherAlias.TotalAmount) as AmountRatio,

     _PreviousPeriod
}
```

Resulting code:

```ASDDLS

define view entity I_AnyEntity
  with parameters
    P_DisplayCurrency     : vdm_v_display_currency,
    P_ExchangeRateType    : kurst,
    P_ExchangeRateDate    : vdm_v_exchange_rate_date,
    P_DisplayUnit         : meins

  as select from I_AnySource as AnyAlias

    inner join I_OtherSource as OtherAlias
      on AnyAlias.AnyKeyField = OtherAlias.AnyKeyField

  association to I_ThirdSource  as _PreviousPeriod
    on _PreviousPeriod.FiscalYearPeriod = fiscal_calendar_shift(fiscal_variant => $projection.FiscalYearVariant,
                                                                base           => $projection.FiscalYearPeriod,
                                                                base_level     => CALENDAR_FISCAL_LEVEL.#period,
                                                                shift          => abap.int2'-1')

{
  key AnyAlias.AnyKeyField,
  key AnyAlias.FiscalYearVariant,
  key AnyAlias.FiscalYearPeriod,

      // special built-in functions
      currency_conversion(amount             => AnyAlias.Amount,
                          source_currency    => AnyAlias.Currency,
                          target_currency    => $parameters.P_DisplayCurrency,
                          exchange_rate_type => $parameters.P_ExchangeRateType,
                          exchange_rate_date => $parameters.P_ExchangeRateDate
//                          round              => 'X',
//                          error_handling     => 'SET_TO_NULL'
                         ) as AmountInDisplayCurrency,

      unit_conversion(quantity    => AnyAlias.Quantity,
                      source_unit => AnyAlias.QuantityUnit,
                      target_unit => $parameters.P_DisplayUnit)

      // analytical scalar functions
      power(base     => power(base     => OtherAlias.AnyNumericValue,
                              exponent => 3),
            exponent => 2) as ValueCubeSquare,

      ratio_of(portion => (  OtherAlias.TotalAmount
                           - OtherAlias.TotalAmount * OtherAlias.Discount) * OtherAlias.Tax,
               total   => OtherAlias.TotalAmount) as AmountRatio,

     _PreviousPeriod
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignFunctionParametersRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/alignment/DdlAlignFunctionParametersTest.java)

