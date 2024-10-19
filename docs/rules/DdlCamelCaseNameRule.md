[<-- previous rule](DdlSpacesAroundBracketsRule.md) | [overview](../rules.md) | [next rule -->](DdlTypoRule.md)

# Use CamelCase for known entity and field names

Fixes upper and lower case of known entity names and field names, and of aliases named after known entity names.

This rule only changes view and field names where they are defined; usages are automatically adjusted upon save. Custom view and field names from rule 'Use CamelCase for known CDS names' are reused.

## Options

* \[X\] Fix known entity names to CamelCase
* \[X\] Fix aliases named after known entity names to CamelCase
* \[X\] Fix known field names to CamelCase
* \[X\] Fix references in annotations to CamelCase
* \[X\] Only use approved names
* \[X\] Only change field names if all field names in this entity are known

## Examples


```ASDDLS

define view entity i_Companycode
  as select from t001 as d

  association[0..1] to I_Country as _country
    on $projection.Country = _country.Country

  association[0..1] to I_ChartOfAccounts as _chartofaccounts
    on $projection.ChartOfAccounts = _chartofaccounts.ChartOfAccounts

{
      @ObjectModel.text.element: ['companycodename']
  key d.bukrs    as companycode,

      d.butxt    as companyCodename,

      d.ort01    as CityName,

      @ObjectModel.foreignKey.association: '_country'
      d.land1    as COUNTRY,

      @ObjectModel.foreignKey.association: '_CHARTOFACCOUNTS'
      d.ktopl    as chArtOfAccOUnts,

      d.periv    as FiscalyeaRVariant,

      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',
                                                    element: 'CONTROLLINGAREA' }}]
      d.kokrs    as controllingarea,

      d.mwska    as nontaxabletransactiontaxcode,
      d.xvatdate as TAXRPTGDATEISACTIVE,
      d.xskfn    as cashdiscountbaseamtisnetamt,

      // Once the definitions of aliases etc. are corrected to _ChartOfAccounts etc. above,
      // any usage of these names will automatically be adjusted once the view is saved.
      // Therefore, it would not help to fix names that are defined in a different place,
      // because that would be undone when saving the view (except for annotation values).
      _country,
      _chartofaccounts
}
```

Resulting code:

```ASDDLS

define view entity I_CompanyCode
  as select from t001 as d

  association[0..1] to I_Country as _Country
    on $projection.Country = _country.Country

  association[0..1] to I_ChartOfAccounts as _ChartOfAccounts
    on $projection.ChartOfAccounts = _chartofaccounts.ChartOfAccounts

{
      @ObjectModel.text.element: ['CompanyCodeName']
  key d.bukrs    as CompanyCode,

      d.butxt    as CompanyCodeName,

      d.ort01    as CityName,

      @ObjectModel.foreignKey.association: '_Country'
      d.land1    as Country,

      @ObjectModel.foreignKey.association: '_ChartOfAccounts'
      d.ktopl    as ChartOfAccounts,

      d.periv    as FiscalYearVariant,

      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',
                                                    element: 'ControllingArea' }}]
      d.kokrs    as ControllingArea,

      d.mwska    as NonTaxableTransactionTaxCode,
      d.xvatdate as TaxRptgDateIsActive,
      d.xskfn    as CashDiscountBaseAmtIsNetAmt,

      // Once the definitions of aliases etc. are corrected to _ChartOfAccounts etc. above,
      // any usage of these names will automatically be adjusted once the view is saved.
      // Therefore, it would not help to fix names that are defined in a different place,
      // because that would be undone when saving the view (except for annotation values).
      _country,
      _chartofaccounts
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/spaces/DdlCamelCaseNameRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/spaces/DdlCamelCaseNameTest.java)

