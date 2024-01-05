[<-- previous rule](AlignFormDeclarationRule.md) | [overview](../rules.md) | next rule -->

# Align PERFORM parameters

Aligns the parameters of obsolete subroutine calls with PERFORM.

Alignment of PERFORM: chains is not supported.

## References

* [Clean ABAP Styleguide: Line-break multiple parameters](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#line-break-multiple-parameters)

## Options

* Maximum line length \[120\] 
* Continue behind PERFORM for up to \[3\] parameters
* \[ \] Always break after call if additions IN PROGRAM or IF FOUND are used
* \[ \] Align TABLES / USING / CHANGING with subroutine name
* \[X\] Continue after TABLES / USING / CHANGING
* Condense parameter lists starting from \[16\] parameters

## Examples


```ABAP

FORM any_form.
  PERFORM any_subroutine_with_a_long_name USING lv_any_value 42 'ABCDEFGHIJKLMN' gc_any_constant '3.14159265'.

  PERFORM other_subr
    TABLES lt_any
    USING lt_any_table[] lv_other_table[].

  PERFORM third_subr IN PROGRAM any_program TABLES lt_any_table lt_other_table
          USING lv_other_value.

  PERFORM (lv_subr_name) IN PROGRAM other_program
  TABLES lr_table_ref->*
    <lt_other_table>
    USING if_any_interface=>co_any_constant
      lo_any_instance->mv_any_attribute
      CHANGING <ls_any_structure>-any_component IF FOUND.

  PERFORM (lv_subr_name)  IN PROGRAM (lv_program_name) IF  FOUND
    USING 100  200   300 400   500 600 700 800 " comment
       'abcde' 'fghij' 'klmno' 'pqrst'  'uwvxy'
      gc_any_constant gc_other_constant
   gc_third_constant gc_fourth_constant
    CHANGING lv_any_value lv_other_value.

  PERFORM lv_subr_index OF form_one form_two form_three form_four
   form_five form_six form_seven.

  " obsolete syntax:
  PERFORM
    subr_name(prog_name)
    IF FOUND
    TABLES lt_any USING
    lv_any_value CHANGING lv_other_value.
ENDFORM.
```

Resulting code:

```ABAP

FORM any_form.
  PERFORM any_subroutine_with_a_long_name
    USING lv_any_value
          42
          'ABCDEFGHIJKLMN'
          gc_any_constant
          '3.14159265'.

  PERFORM other_subr TABLES lt_any
                     USING  lt_any_table[]
                            lv_other_table[].

  PERFORM third_subr IN PROGRAM any_program TABLES lt_any_table
                                                   lt_other_table
                                            USING  lv_other_value.

  PERFORM (lv_subr_name) IN PROGRAM other_program IF FOUND
    TABLES   lr_table_ref->*
             <lt_other_table>
    USING    if_any_interface=>co_any_constant
             lo_any_instance->mv_any_attribute
    CHANGING <ls_any_structure>-any_component.

  PERFORM (lv_subr_name) IN PROGRAM (lv_program_name) IF FOUND
    USING    100 200 300 400 500 600 700 800 " comment
             'abcde' 'fghij' 'klmno' 'pqrst' 'uwvxy' gc_any_constant gc_other_constant gc_third_constant
             gc_fourth_constant
    CHANGING lv_any_value lv_other_value.

  PERFORM lv_subr_index OF form_one
                           form_two
                           form_three
                           form_four
                           form_five
                           form_six
                           form_seven.

  " obsolete syntax:
  PERFORM subr_name(prog_name) IF FOUND TABLES   lt_any
                                        USING    lv_any_value
                                        CHANGING lv_other_value.
ENDFORM.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignPerformRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignPerformTest.java)

