[<-- previous rule](ImplicitTypeRule.md) | [overview](../rules.md) | [next rule -->](ClassDefinitionRule.md)

# Use FINAL for immutable variables

Replaces DATA\(\) inline declarations with FINAL\(\) if no other write access to the respective variable is found.

This rule requires a NetWeaver version >= 7.57. Note that this rule will skip methods in which macros are used.

## References

* [ABAP Keyword Documentation: FINAL, Inline Declaration for Immutable Variables](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfinal_inline.htm)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

CLASS any_class IMPLEMENTATION.
  METHOD use_final_for_immutable_vars.
    DATA(lo_utility) = cl_any_factory=>get( )->get_any_utility( ).
    DATA(lv_date) = lo_utility->get_any_date( ).

    SELECT carrid, connid, seatsocc
           FROM sflight
           WHERE fldate = @lv_date
           INTO TABLE @DATA(lt_flight).

    DATA(lv_sum) = 0.
    LOOP AT lt_flight INTO DATA(ls_flight) ##INTO_OK.
      lv_sum += ls_flight-seatsocc.
    ENDLOOP.

    GET TIME FIELD DATA(lv_time).
    rv_result = |{ lv_date } { lv_time }: { lv_sum }|.

    " if a variable is changed via field-symbol, FINAL would cause a runtime error:
    DATA(lt_connection) = get_connections( lt_flight ).
    LOOP AT lt_connection ASSIGNING FIELD-SYMBOL(<ls_connection>).
      <ls_connection>-used = abap_true.
    ENDLOOP.

    " similarly, FINAL is never introduced if a data reference is created somewhere:
    DATA(lt_any_table) = get_table( ).
    any_method( ir_struc = REF #( lt_any_table[ 1 ] ) ).

    " FINAL is not introduced if method calls are found inside of constructor expressions,
    " because some of these cases would create syntax errors with FINAL:
    DATA(ls_struc) = VALUE ty_s_any_struc( comp1 = get_any_value( )
                                           comp2 = 'literal' ).
  ENDMETHOD.
ENDCLASS.
```

Resulting code:

```ABAP

CLASS any_class IMPLEMENTATION.
  METHOD use_final_for_immutable_vars.
    FINAL(lo_utility) = cl_any_factory=>get( )->get_any_utility( ).
    FINAL(lv_date) = lo_utility->get_any_date( ).

    SELECT carrid, connid, seatsocc
           FROM sflight
           WHERE fldate = @lv_date
           INTO TABLE @FINAL(lt_flight).

    DATA(lv_sum) = 0.
    LOOP AT lt_flight INTO FINAL(ls_flight) ##INTO_OK.
      lv_sum += ls_flight-seatsocc.
    ENDLOOP.

    GET TIME FIELD FINAL(lv_time).
    rv_result = |{ lv_date } { lv_time }: { lv_sum }|.

    " if a variable is changed via field-symbol, FINAL would cause a runtime error:
    DATA(lt_connection) = get_connections( lt_flight ).
    LOOP AT lt_connection ASSIGNING FIELD-SYMBOL(<ls_connection>).
      <ls_connection>-used = abap_true.
    ENDLOOP.

    " similarly, FINAL is never introduced if a data reference is created somewhere:
    DATA(lt_any_table) = get_table( ).
    any_method( ir_struc = REF #( lt_any_table[ 1 ] ) ).

    " FINAL is not introduced if method calls are found inside of constructor expressions,
    " because some of these cases would create syntax errors with FINAL:
    DATA(ls_struc) = VALUE ty_s_any_struc( comp1 = get_any_value( )
                                           comp2 = 'literal' ).
  ENDMETHOD.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/FinalVariableRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/FinalVariableTest.java)

