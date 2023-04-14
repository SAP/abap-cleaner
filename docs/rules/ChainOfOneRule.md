[<-- previous rule](UnusedVariablesRule.md) | [overview](../rules.md) | [next rule -->](ImplicitTypeRule.md)

# Simplify a chain with one element

Simplifies chains that consist of one element only by removing the : sign.

## Options

* \[X\] Execute on declarations in CLASS ... DEFINITION sections
* \[X\] Execute on declarations in methods etc.
* \[X\] Execute on non-declaration commands

## Examples


```ABAP

CLASS simplify_chain_of_one DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: any_constant TYPE i VALUE 42.
    METHODS:
      simplify_chain_of_one.
ENDCLASS.

CLASS simplify_chain_of_one IMPLEMENTATION.
  METHOD simplify_chain_of_one.
    CONSTANTS: lc_any_price TYPE ty_price VALUE 1200.

    DATA: " comment on declaration
      lo_item TYPE REF TO cl_item ##NEEDED.
    DATA: lth_hash_table TYPE ty_th_hash_table. 

    FIELD-SYMBOLS: <ls_field_symbol> LIKE LINE OF its_table.
    FIELD-SYMBOLS:
         <ls_data> TYPE ty_any_data.

    CHECK: its_table IS NOT INITIAL.
    CLEAR: ev_result.
    ev_result := 1.

    CALL METHOD : " comment on method call
      any_method( ).
  ENDMETHOD.
ENDCLASS.
```

Resulting code:

```ABAP

CLASS simplify_chain_of_one DEFINITION.
  PUBLIC SECTION.
    CONSTANTS any_constant TYPE i VALUE 42.
    METHODS simplify_chain_of_one.
ENDCLASS.

CLASS simplify_chain_of_one IMPLEMENTATION.
  METHOD simplify_chain_of_one.
    CONSTANTS lc_any_price TYPE ty_price VALUE 1200.

    " comment on declaration
    DATA lo_item TYPE REF TO cl_item ##NEEDED.
    DATA lth_hash_table TYPE ty_th_hash_table.

    FIELD-SYMBOLS <ls_field_symbol> LIKE LINE OF its_table.
    FIELD-SYMBOLS <ls_data> TYPE ty_any_data.

    CHECK its_table IS NOT INITIAL.
    CLEAR ev_result.
    ev_result = 1.

    " comment on method call
    CALL METHOD any_method( ).
  ENDMETHOD.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/ChainOfOneRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/ChainOfOneTest.java)

