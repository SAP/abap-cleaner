[<-- previous rule](NeedlessSpacesRule.md) | [overview](../rules.md) | [next rule -->](NeedlessClearRule.md)

# Unchain into multiple statements

Resolves a chain \(DATA:, FIELD-SYMBOLS:, ASSERT: etc.\) into multiple standalone statements. The chain is kept, however, for structure declarations with BEGIN OF ... END OF.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Do not chain up-front declarations](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#do-not-chain-up-front-declarations)
* [Code Pal for ABAP: Chain Declaration Usage](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/chain-declaration-usage.md)
* [ABAP Keyword Documentation: Only use chained statements where appropriate](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenchained_statements_guidl.htm)

## Options

* \[X\] Unchain declarations in interfaces
* \[X\] Unchain declarations in CLASS ... DEFINITION sections
* \[X\] Unchain declarations in methods etc.
* \[X\] After TYPES: BEGIN OF ... END OF, keep tables chained with their structure
* \[ \] Unchain simple commands \(chain after first keyword, e.g. ASSERT:, CHECK:, CLEAR:, FREE:\) except WRITE:
* \[X\] Unchain complex commands \(a \+= : 1,2,3 etc.\)

## Examples


```ABAP

INTERFACE if_unchaining.
  CONSTANTS: any_constant TYPE i VALUE 1,
             other_constant TYPE i VALUE 2.

  METHODS:
    any_method,
    other_method
      IMPORTING iv_any_parameter TYPE i.
ENDINTERFACE.

CLASS cl_unchaining DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: any_constant TYPE i VALUE 1,
               other_constant TYPE i VALUE 2.

    " BEGIN OF ... END OF blocks are kept as a chain, however other types around
    " them are unchained; tables using the structure can be kept in the chain:
    TYPES: 
      ty_any_type TYPE string,
      BEGIN of ty_s_any,
        a TYPE i,
        b TYPE string,
      END OF ty_s_any,
      ty_tt_any TYPE STANDARD TABLE OF ty_s_any WITH EMPTY KEY,
      ty_ts_any TYPE SORTED TABLE OF ty_s_any WITH UNIQUE KEY a,
      ty_other_type TYPE i,
      ty_third_type TYPE i.

    METHODS:
      setup,
      unchain.
ENDCLASS.

CLASS cl_unchaining IMPLEMENTATION.
  METHOD unchain.
    CONSTANTS: 
      lc_list_price_1200 TYPE ty_list_price VALUE 1200,
      lc_amount_1000 TYPE ty_amount VALUE 1000,
      lc_num_contract_change TYPE ty_sequence_number VALUE 1.

    " alignment within the declaration line is not changed; empty lines are kept
    DATA: lth_any_hash_table TYPE ty_th_hash_table, " comment
          lo_contract TYPE REF TO cl_contract  ##NEEDED,

          ls_structure TYPE if_any_interface=>ty_s_structure,
          mv_bool_variable TYPE abap_bool VALUE abap_false ##NO_TEXT.

    FIELD-SYMBOLS: " comment above the first identifier
      <ls_data>      TYPE ty_s_data, ##PRAGMA_IN_WRONG_POSITION
      <ls_amount>  LIKE LINE OF its_amount, " comment

      " comment line within the chain
      <ls_contract_data> TYPE ty_s_contract_data,
      <ls_parameter>   LIKE LINE OF mt_parameter.

    ASSERT: lv_count = 1,
            lts_table IS NOT INITIAL.

    CLEAR: ev_result_a, ev_result_b.

    " WRITE: chains make sense, as the output is on one line, too; they are therefore kept
    WRITE: / `text`, iv_value, `more text`, iv_other_value.

    lv_value += 2 ** : 1, 2, 3.

    add_value( lv_value ):,,.

    CALL METHOD any_method
      EXPORTING iv_value_a = 'text'
                iv_value_b = : 42, 84.
  ENDMETHOD.
ENDCLASS.
```

Resulting code:

```ABAP

INTERFACE if_unchaining.
  CONSTANTS any_constant TYPE i VALUE 1.
  CONSTANTS other_constant TYPE i VALUE 2.

  METHODS any_method.
  METHODS other_method
            IMPORTING iv_any_parameter TYPE i.
ENDINTERFACE.

CLASS cl_unchaining DEFINITION.
  PUBLIC SECTION.
    CONSTANTS any_constant TYPE i VALUE 1.
    CONSTANTS other_constant TYPE i VALUE 2.

    " BEGIN OF ... END OF blocks are kept as a chain, however other types around
    " them are unchained; tables using the structure can be kept in the chain:
    TYPES ty_any_type TYPE string.
    TYPES: BEGIN of ty_s_any,
             a TYPE i,
             b TYPE string,
           END OF ty_s_any,
           ty_tt_any TYPE STANDARD TABLE OF ty_s_any WITH EMPTY KEY,
           ty_ts_any TYPE SORTED TABLE OF ty_s_any WITH UNIQUE KEY a.
    TYPES ty_other_type TYPE i.
    TYPES ty_third_type TYPE i.

    METHODS setup.
    METHODS unchain.
ENDCLASS.

CLASS cl_unchaining IMPLEMENTATION.
  METHOD unchain.
    CONSTANTS lc_list_price_1200 TYPE ty_list_price VALUE 1200.
    CONSTANTS lc_amount_1000 TYPE ty_amount VALUE 1000.
    CONSTANTS lc_num_contract_change TYPE ty_sequence_number VALUE 1.

    " alignment within the declaration line is not changed; empty lines are kept
    DATA lth_any_hash_table TYPE ty_th_hash_table. " comment
    DATA lo_contract TYPE REF TO cl_contract  ##NEEDED.

    DATA ls_structure TYPE if_any_interface=>ty_s_structure.
    DATA mv_bool_variable TYPE abap_bool VALUE abap_false ##NO_TEXT.

    " comment above the first identifier
    FIELD-SYMBOLS <ls_data>      TYPE ty_s_data ##PRAGMA_IN_WRONG_POSITION.
    FIELD-SYMBOLS <ls_amount>  LIKE LINE OF its_amount. " comment

    " comment line within the chain
    FIELD-SYMBOLS <ls_contract_data> TYPE ty_s_contract_data.
    FIELD-SYMBOLS <ls_parameter>   LIKE LINE OF mt_parameter.

    ASSERT: lv_count = 1,
            lts_table IS NOT INITIAL.

    CLEAR: ev_result_a, ev_result_b.

    " WRITE: chains make sense, as the output is on one line, too; they are therefore kept
    WRITE: / `text`, iv_value, `more text`, iv_other_value.

    lv_value += 2 ** 1.
    lv_value += 2 ** 2.
    lv_value += 2 ** 3.

    add_value( lv_value ).
    add_value( lv_value ).
    add_value( lv_value ).

    CALL METHOD any_method
      EXPORTING iv_value_a = 'text'
                iv_value_b = 42.
    CALL METHOD any_method
      EXPORTING iv_value_a = 'text'
                iv_value_b = 84.
  ENDMETHOD.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/ChainRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/ChainTest.java)

