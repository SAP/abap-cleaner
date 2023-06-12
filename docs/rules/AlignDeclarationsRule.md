[<-- previous rule](AlignAliasesForRule.md) | [overview](../rules.md) | [next rule -->](AlignAssignmentsRule.md)

# Align declarations

Aligns both chains and consecutive declaration lines of CONSTANTS, DATA, FIELD-SYMBOLS, and TYPES.

## References

* [<-> Clean ABAP Styleguide: Don't align type clauses](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-align-type-clauses)

## Options

* \[X\] Also execute on CLASS ... DEFINITION sections and INTERFACES
* Action for chains: \[align name, TYPE, LENGTH, VALUE etc. if filled\]
* Action for consecutive non-chains: \[align name, TYPE, LENGTH, VALUE etc. if filled\]
* \[X\] Align across empty lines
* \[X\] Align across comment lines
* Action for structures \(BEGIN OF ...\): \[align name, TYPE, LENGTH, VALUE etc. if filled\]
* Scope of nested structures: \[align outer structure independently \(like Pretty Printer\)\]
* Fill Ratio to justify own column \[20\] %
* \[X\] Condense inner spaces in non-aligned parts

## Examples


```ABAP

  METHOD align_declarations.
    CONSTANTS: lc_any_price TYPE ty_price VALUE 1200,
      lc_other_price TYPE ty_price VALUE 1000,
               lc_num_contract_change TYPE ty_sequence_number VALUE    1,
          lc_start_date TYPE ty_start_date VALUE '20220312'.

    FIELD-SYMBOLS: 
         <ls_data> TYPE ty_s_data,   " first comment line
      <ls_amount> LIKE LINE OF its_amount,
    <ls_contract> TYPE ty_s_contract, " second comment line
   <ls_param>  LIKE LINE OF mt_parameter.

    " if only a single declaration (or a very small ratio of them) has a VALUE, a comment, etc.,
    " no extra column is created for it
    DATA lth_any_table TYPE ty_th_hash_table_type. " only line with a comment
    DATA lo_contract TYPE REF TO  cl_contract  ##NEEDED.
    DATA ls_item_data   TYPE if_any_interface=>ty_s_item.
    DATA lv_was_saved   TYPE abap_bool  VALUE abap_false  ##NO_TEXT.

    " alignment across comments and empty lines (depending on configuration):
    CONSTANTS lc_pi TYPE p LENGTH 10 DECIMALS 10 VALUE '3.1415926536'.
    CONSTANTS lc_e TYPE p LENGTH 8 DECIMALS 10 VALUE '2.718281828'.
    " square roots
    CONSTANTS lc_sqrt_2 TYPE p LENGTH 8 DECIMALS 4 VALUE '1.4142'.
    CONSTANTS lc_sqrt_32 TYPE p LENGTH 8 DECIMALS 10 VALUE '5.6568542495'.

    CONSTANTS lc_ln_10 TYPE p  LENGTH 8 DECIMALS 4 VALUE '2.3026'.

    TYPES:
      BEGIN OF ty_s_outer,
      one TYPE i,
      two TYPE i,
      BEGIN OF ty_s_inner,
      alpha TYPE p LENGTH 5 DECIMALS 2,
      beta TYPE p LENGTH 10 DECIMALS 2,
      gamma TYPE i,
      END OF ty_s_inner,
      three  TYPE i,
      four   TYPE i,
      BEGIN OF ty_s_another_inner,
      long_component_name   TYPE i,
      very_long_component_name TYPE i,
      END OF ty_s_another_inner,
      seventeen TYPE i,
      eighteen TYPE i,
      END OF ty_s_outer,
      ty_tt_outer TYPE STANDARD TABLE OF ty_s_outer WITH DEFAULT KEY,

      BEGIN OF ty_s_outer_2,
      any_component TYPE i,
      BEGIN OF ty_s_inner,
      alpha  TYPE i,
      beta   TYPE i,
      END OF ty_s_inner,
      other_component TYPE i,
      END OF ty_s_outer_2.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD align_declarations.
    CONSTANTS: lc_any_price           TYPE ty_price           VALUE 1200,
               lc_other_price         TYPE ty_price           VALUE 1000,
               lc_num_contract_change TYPE ty_sequence_number VALUE 1,
               lc_start_date          TYPE ty_start_date      VALUE '20220312'.

    FIELD-SYMBOLS:
      <ls_data>     TYPE ty_s_data,            " first comment line
      <ls_amount>   LIKE LINE OF its_amount,
      <ls_contract> TYPE ty_s_contract,        " second comment line
      <ls_param>    LIKE LINE OF mt_parameter.

    " if only a single declaration (or a very small ratio of them) has a VALUE, a comment, etc.,
    " no extra column is created for it
    DATA lth_any_table TYPE ty_th_hash_table_type. " only line with a comment
    DATA lo_contract   TYPE REF TO cl_contract ##NEEDED.
    DATA ls_item_data  TYPE if_any_interface=>ty_s_item.
    DATA lv_was_saved  TYPE abap_bool VALUE abap_false  ##NO_TEXT.

    " alignment across comments and empty lines (depending on configuration):
    CONSTANTS lc_pi      TYPE p LENGTH 10 DECIMALS 10 VALUE '3.1415926536'.
    CONSTANTS lc_e       TYPE p LENGTH 8  DECIMALS 10 VALUE '2.718281828'.
    " square roots
    CONSTANTS lc_sqrt_2  TYPE p LENGTH 8  DECIMALS 4  VALUE '1.4142'.
    CONSTANTS lc_sqrt_32 TYPE p LENGTH 8  DECIMALS 10 VALUE '5.6568542495'.

    CONSTANTS lc_ln_10   TYPE p LENGTH 8  DECIMALS 4  VALUE '2.3026'.

    TYPES:
      BEGIN OF ty_s_outer,
        one       TYPE i,
        two       TYPE i,
        BEGIN OF ty_s_inner,
          alpha TYPE p LENGTH 5  DECIMALS 2,
          beta  TYPE p LENGTH 10 DECIMALS 2,
          gamma TYPE i,
        END OF ty_s_inner,
        three     TYPE i,
        four      TYPE i,
        BEGIN OF ty_s_another_inner,
          long_component_name      TYPE i,
          very_long_component_name TYPE i,
        END OF ty_s_another_inner,
        seventeen TYPE i,
        eighteen  TYPE i,
      END OF ty_s_outer,
      ty_tt_outer TYPE STANDARD TABLE OF ty_s_outer WITH DEFAULT KEY,

      BEGIN OF ty_s_outer_2,
        any_component   TYPE i,
        BEGIN OF ty_s_inner,
          alpha TYPE i,
          beta  TYPE i,
        END OF ty_s_inner,
        other_component TYPE i,
      END OF ty_s_outer_2.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignDeclarationsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignDeclarationsTest.java)

