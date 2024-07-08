[<-- previous rule](LogicalOperatorPositionRule.md) | [overview](../rules.md) | [next rule -->](NeedlessParenthesesRule.md)

# Use string templates to assemble text

Replaces the concatenation operator && with string templates |text \{ variable \} text|.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Use | to assemble text](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use--to-assemble-text)
* [Code Pal for ABAP: Text Assembly](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/text-assembly.md)

## Options

* Embed operands with |\{ ... \}|: \[if result is shorter or equal\]
* \[X\] Convert text literals in concatenations regardless of result length
* \[ \] Only convert text literals if line contains operands to embed
* \[X\] Ignore multi-line operands at line start or end
* \[X\] Keep string templates with control characters \\t \\n \\r separate

## Examples


```ABAP

  METHOD use_string_templates.
    " cases with multiple literals
    lv_from = left && ` join ` && right && ` on `.
    ls_sel_params-low = '*' && ls_sel_params-low && '*'.
    out->write( `Name:` && ` ` && iv_first_name && ` ` && iv_last_name ).
    cl_abap_unit_assert=>assert_fail( msg = 'Expected:' && ` ` && iv_number && ',' && ` ` && 'Actual:' && ` ` && lv_act ).
    " existing line breaks are kept:
    cl_abap_unit_assert=>assert_fail( msg = 'Expected:' && ` ` && iv_number && ',' && ` `
                                         && 'Actual:' && ` ` && lv_act ).
    " you might want to keep string templates with control characters separate:
    lv_text = |\r\n| && |first line|  && |\t| && |cell B1|
           && |\r\n| && |second line| && |\t| && |cell B2|.

    " cases with one literal
    lv_first_day_of_march = lv_year && '0301'.
    lv_formula = lv_var_1 && ` + ` && lv_var_2.
    lv_salutation = `Hello ` && lv_name.
    " the multi-line operand at the end can be embedded or ignored
    lv_text = |{ lv_count } | && COND #( WHEN lv_count = 1
                                         THEN 'element'
                                         ELSE 'elements').

    " cases without literal
    lv_date = lv_year && lv_month && lv_day.
    lv_fiscal_year_period = lv_fiscal_year && lv_period.

    " the && operator ignores trailing spaces in text field literals '...',
    " but keeps them in text string literals `...`, so the conversion reflects this:
    lv_result1 = ' a   ' && ' +   ' && ' b  ' && ' =     ' && ' 10' && '  '.
    lv_result2 = ` a   ` && ` +   ` && ` b  ` && ` =     ` && ` 10` && `  `.

    " escape chars are changed accordingly:
    lv_escape1 = 'To ''be''' && ` or ``not`` to be`.
    lv_escape2 = 'String templates must escape |' && ` as well as { and }.`.

    " you may want to use string templates only on lines that contain operands to embed:
    rv_example  = `3 + 5 = ` && `8` && `. `
               && `a + b = ` && c && `.`.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD use_string_templates.
    " cases with multiple literals
    lv_from = |{ left } join { right } on |.
    ls_sel_params-low = |*{ ls_sel_params-low }*|.
    out->write( |Name: { iv_first_name } { iv_last_name }| ).
    cl_abap_unit_assert=>assert_fail( msg = |Expected: { iv_number }, Actual: { lv_act }| ).
    " existing line breaks are kept:
    cl_abap_unit_assert=>assert_fail( msg = |Expected: { iv_number }, |
                                         && |Actual: { lv_act }| ).
    " you might want to keep string templates with control characters separate:
    lv_text = |\r\n| && |first line|  && |\t| && |cell B1|
           && |\r\n| && |second line| && |\t| && |cell B2|.

    " cases with one literal
    lv_first_day_of_march = |{ lv_year }0301|.
    lv_formula = |{ lv_var_1 } + { lv_var_2 }|.
    lv_salutation = |Hello { lv_name }|.
    " the multi-line operand at the end can be embedded or ignored
    lv_text = |{ lv_count } | && COND #( WHEN lv_count = 1
                                         THEN 'element'
                                         ELSE 'elements').

    " cases without literal
    lv_date = lv_year && lv_month && lv_day.
    lv_fiscal_year_period = lv_fiscal_year && lv_period.

    " the && operator ignores trailing spaces in text field literals '...',
    " but keeps them in text string literals `...`, so the conversion reflects this:
    lv_result1 = | a + b = 10|.
    lv_result2 = | a    +    b   =      10  |.

    " escape chars are changed accordingly:
    lv_escape1 = |To 'be' or `not` to be|.
    lv_escape2 = |String templates must escape \| as well as \{ and \}.|.

    " you may want to use string templates only on lines that contain operands to embed:
    rv_example  = |3 + 5 = 8. |
               && |a + b = { c }.|.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/StringTemplateRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/StringTemplateTest.java)

