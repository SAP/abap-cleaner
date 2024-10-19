[<-- previous rule](UpperAndLowerCaseRule.md) | [overview](../rules.md) | [next rule -->](CamelCaseInCdsTestRule.md)

# Use CamelCase for known CDS names

Changes known VDM CDS view names and field names to CamelCase.

This rule does not distinguish between structures from ABAP or CDS View context \(see examples below\). Please fine-tune as needed.

## References

* [ABAP Keyword Documentation: Use uppercase for keywords and lowercase for operands](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlower_upper_case_guidl.htm)

## Options

* \[X\] Change known view names to CamelCase
* \[X\] Change known field names to CamelCase
* \[X\] Consider commented-out lines in VALUE or NEW constructors \(expected format: '\*  component = ...'\)
* To decide whether to change a known field name to CamelCase, ABAP cleaner analyzes its context, e.g. all field names in a VALUE constructor, a SELECT statement, a table key etc.
* Consider known field names a 'sure match' if they are at least \[11\] chars long and contain an upper case letter after a lower case one
* \[ \] Only consider approved names a 'sure match'
* If a context contains a 'sure match' and all field names are known: \[change all known field names in the context\]
* If a context contains a 'sure match' but also an unknown field name: \[do not change any field names in the context\]
* Correct existing 'CameLcasE' name \[if it differs from an approved name\]
* Custom view and field names can be maintained in text files inside the \(local or synchronized\) folder of this profile, simply using the line format CamelCaseName<ENTER>
* Custom view names file in profile folder: \[CustomViewNames.txt\]
* Custom field names file in profile folder: \[CustomFieldNames.txt\]

## Examples


```ABAP

  METHOD camel_case_names.
    DATA lt_company TYPE STANDARD TABLE OF i_companycode.
    DATA lv_fiscal_year_variant TYPE i_companycode-fiscalyearvariant.

    " Fields from CDS Views can be hard to read if shown in all-lower case.
    " The longer the field name, the more its readability benefits from CamelCase:
    lt_company = VALUE I_COMPANYCODE( ( companycode                  = '1234'
                                        companycodename              = 'Company Name'
                                        cityname                     = 'Berlin'
*                                        chartofaccounts              = 'ABCD'
*                                        FiscalyeaRVariant            = 'K4'
                                        nontaxabletransactiontaxcode = 'AB'
                                        taxrptgdateisactive          = abap_true
                                        cashdiscountbaseamtisnetamt  = abap_false ) ).

    ASSIGN lt_company[ companycodename = 'Company Name' ] TO FIELD-SYMBOL(<ls_any>).
    LOOP AT lt_company ASSIGNING FIELD-SYMBOL(<ls_company>) WHERE cityname = 'Berlin'.
      any_method( iv_cash_discount_is_net = <ls_company>-cashdiscountbaseamtisnetamt
                  iv_fiscal_year_variant  = <ls_company>-fiscalyearvariant ).
    ENDLOOP.

    " IMPORTANT: this rule does NOT distinguish between structures from ABAP or VDM CDS View context.
    " Most components of the following structure consist of two words, but they do not use snake_case,
    " so they might be replaced with corresponding CamelCase field names from CDS context, too:
    ls_vdm_style = VALUE #( refid        = '1'
                            owner        = 'anyone'
                            cityname     = 'Hamburg'
                            lognumber    = '12345'
                            dataprovider = 'XYZ' ).

    " by contrast, proper ABAP style would be snake_case; such component names are not replaced,
    " because they differ from the naming style of VDM CDS View fields:
    ls_abap_style = VALUE #( ref_id         = '1'
                             owner          = 'anyone'
                             city_name      = 'Hamburg'
                             log_number     = '12345'
                             data_provider  = 'XYZ' ).

    " 'False positives' without underscores (such as lognumber above) are typically short.
    " Unintentional replacement can be prevented by setting a minimum length for a 'sure match' in the
    " options above, since in CDS View context, field names are usually long: Among CDS field names,
    "   99% have 11+ characters
    "   95% have 15+ characters
    "   82% have 20+ characters
    "   61% have 24+ characters

    " for the following fields, CamelCase variants are known, but none of them is officially approved:
    SORT lt_web_info BY website
                        webaddress
                        visitedPlacename.

    " Shorter field names as well as field names with just an initial upper case character like 'Currency'
    " or 'Language' can be changed if all field names in the context (e.g. in the whole VALUE constructor)
    " are known field names and at least one 'sure match' was identified:
    lt_company = VALUE #( ( companycode     = '1234'
                            currency        = 'EUR'
                            language        = 'E'
                            ChartofACcounts = 'ABCD' ) ).
    " without this restriction, the field 'currency' and 'language' might be changed in ABAP structures, too:
    ls_abap_struc = VALUE #( obj_nr     = '1'
                             currency   = 'EUR'
                             language   = 'E'
                             field_name = 'any' ).

    " you can configure how much should be changed if one of the fields is unknown:
    ls_unknown_struc = VALUE #( refid              = '1'
                                currency           = 'EUR'
                                unknown_field_name = 42
                                fiscalyearvariant  = 'K4' ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD camel_case_names.
    DATA lt_company TYPE STANDARD TABLE OF I_CompanyCode.
    DATA lv_fiscal_year_variant TYPE I_CompanyCode-FiscalYearVariant.

    " Fields from CDS Views can be hard to read if shown in all-lower case.
    " The longer the field name, the more its readability benefits from CamelCase:
    lt_company = VALUE I_CompanyCode( ( CompanyCode                  = '1234'
                                        CompanyCodeName              = 'Company Name'
                                        CityName                     = 'Berlin'
*                                        ChartOfAccounts              = 'ABCD'
*                                        FiscalYearVariant            = 'K4'
                                        NonTaxableTransactionTaxCode = 'AB'
                                        TaxRptgDateIsActive          = abap_true
                                        CashDiscountBaseAmtIsNetAmt  = abap_false ) ).

    ASSIGN lt_company[ CompanyCodeName = 'Company Name' ] TO FIELD-SYMBOL(<ls_any>).
    LOOP AT lt_company ASSIGNING FIELD-SYMBOL(<ls_company>) WHERE cityname = 'Berlin'.
      any_method( iv_cash_discount_is_net = <ls_company>-CashDiscountBaseAmtIsNetAmt
                  iv_fiscal_year_variant  = <ls_company>-FiscalYearVariant ).
    ENDLOOP.

    " IMPORTANT: this rule does NOT distinguish between structures from ABAP or VDM CDS View context.
    " Most components of the following structure consist of two words, but they do not use snake_case,
    " so they might be replaced with corresponding CamelCase field names from CDS context, too:
    ls_vdm_style = VALUE #( RefID        = '1'
                            Owner        = 'anyone'
                            CityName     = 'Hamburg'
                            LogNumber    = '12345'
                            DataProvider = 'XYZ' ).

    " by contrast, proper ABAP style would be snake_case; such component names are not replaced,
    " because they differ from the naming style of VDM CDS View fields:
    ls_abap_style = VALUE #( ref_id         = '1'
                             owner          = 'anyone'
                             city_name      = 'Hamburg'
                             log_number     = '12345'
                             data_provider  = 'XYZ' ).

    " 'False positives' without underscores (such as lognumber above) are typically short.
    " Unintentional replacement can be prevented by setting a minimum length for a 'sure match' in the
    " options above, since in CDS View context, field names are usually long: Among CDS field names,
    "   99% have 11+ characters
    "   95% have 15+ characters
    "   82% have 20+ characters
    "   61% have 24+ characters

    " for the following fields, CamelCase variants are known, but none of them is officially approved:
    SORT lt_web_info BY Website
                        WebAddress
                        visitedPlacename.

    " Shorter field names as well as field names with just an initial upper case character like 'Currency'
    " or 'Language' can be changed if all field names in the context (e.g. in the whole VALUE constructor)
    " are known field names and at least one 'sure match' was identified:
    lt_company = VALUE #( ( CompanyCode     = '1234'
                            Currency        = 'EUR'
                            Language        = 'E'
                            ChartOfAccounts = 'ABCD' ) ).
    " without this restriction, the field 'currency' and 'language' might be changed in ABAP structures, too:
    ls_abap_struc = VALUE #( obj_nr     = '1'
                             currency   = 'EUR'
                             language   = 'E'
                             field_name = 'any' ).

    " you can configure how much should be changed if one of the fields is unknown:
    ls_unknown_struc = VALUE #( refid              = '1'
                                currency           = 'EUR'
                                unknown_field_name = 42
                                fiscalyearvariant  = 'K4' ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/prettyprinter/CamelCaseNameRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/prettyprinter/CamelCaseNameTest.java)

