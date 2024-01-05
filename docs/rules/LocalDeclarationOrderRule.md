[<-- previous rule](NeedlessClearRule.md) | [overview](../rules.md) | [next rule -->](UnusedParametersRule.md)

# Rearrange local declarations

Rearranges up-front declarations of TYPES, CONSTANTS, DATA, and FIELD-SYMBOLS. Chains are kept and only rearranged within, therefore this rule should be used in combination with the 'Unchain into multiple statements' rule. 

Moving declarations to their innermost block is discouraged, but may help to refactor long methods.

## References

* [Clean Code Checks: Scope of Variable](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/scope-of-variable.md)

## Options

* Rearrange TYPES \[move to method start, keeping current order\]
* Rearrange CONSTANTS \[move to method start, keeping current order\]
* Rearrange STATICS \[move to method start, order by first usage\]
* Rearrange DATA \[move to method start, order by first usage\]
* Rearrange FIELD-SYMBOLS \[move to method start, order by first usage\]
* \[X\] Create distinct blocks for STATICS, DATA, and FIELD-SYMBOLS
* \[X\] Put empty line between blocks of STATICS, DATA, and FIELD-SYMBOLS\)
* \[X\] Move attached comments
* \[X\] Rearrange declarations within chains
* \[ \] Consider usage in commented-out code

## Examples


```ABAP

  METHOD rearrange_local_declarations.
    TYPES: BEGIN OF ty_s_data,
             name  TYPE string,
             count TYPE i,
           END OF ty_s_data.
    DATA ls_data_avg TYPE ty_s_data.
    TYPES ty_ts_data TYPE SORTED TABLE OF ty_s_data WITH NON-UNIQUE KEY name.
    STATICS sv_call_count TYPE i.
    sv_call_count += 1.

    DATA lts_result TYPE ty_ts_data.
    CLEAR lts_result.
    DATA ls_data_max TYPE ty_s_data.
    CONSTANTS lc_name_average TYPE string VALUE `average`.
    DATA lts_data TYPE ty_ts_data.
    lts_data = CORRESPONDING #( get_data( ) ).

    " field-symbol for calculating the average count
    FIELD-SYMBOLS <ls_data_avg> TYPE ty_s_data.

*    UNASSIGN <ls_data_avg>.
    IF iv_get_maximum = abap_true.
      " field-symbol for calculating the maximum count
      FIELD-SYMBOLS <ls_data_max> TYPE ty_s_data.
      CONSTANTS lc_name_maximum TYPE string VALUE `maximum`.

      ls_data_max-name = lc_name_maximum.
      LOOP AT lts_data ASSIGNING <ls_data_max>.
        IF <ls_data_max>-count > ls_data_max-count.
          ls_data_max-count = <ls_data_max>-count.
        ENDIF.
      ENDLOOP.

      INSERT ls_data_max INTO TABLE lts_result.
    ENDIF.

    IF iv_get_average = abap_true.
      ls_data_avg-name = lc_name_average.
      LOOP AT lts_data ASSIGNING <ls_data_avg>.
        ls_data_avg-count += <ls_data_avg>-count.
      ENDLOOP.
      ls_data_avg-count /= lines( lts_data ).

      INSERT ls_data_avg INTO TABLE lts_result.
    ENDIF.

    CONSTANTS lc_name_call_count TYPE string VALUE `call count`.
    INSERT VALUE #( name  = lc_name_call_count
                    count = sv_call_count ) INTO TABLE lts_result.

    rts_result = CORRESPONDING #( lts_result ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD rearrange_local_declarations.
    TYPES: BEGIN OF ty_s_data,
             name  TYPE string,
             count TYPE i,
           END OF ty_s_data.
    TYPES ty_ts_data TYPE SORTED TABLE OF ty_s_data WITH NON-UNIQUE KEY name.

    CONSTANTS lc_name_average TYPE string VALUE `average`.
    CONSTANTS lc_name_maximum TYPE string VALUE `maximum`.
    CONSTANTS lc_name_call_count TYPE string VALUE `call count`.

    STATICS sv_call_count TYPE i.

    DATA lts_result TYPE ty_ts_data.
    DATA lts_data TYPE ty_ts_data.
    DATA ls_data_max TYPE ty_s_data.
    DATA ls_data_avg TYPE ty_s_data.

    " field-symbol for calculating the maximum count
    FIELD-SYMBOLS <ls_data_max> TYPE ty_s_data.
    " field-symbol for calculating the average count
    FIELD-SYMBOLS <ls_data_avg> TYPE ty_s_data.

    sv_call_count += 1.

    CLEAR lts_result.
    lts_data = CORRESPONDING #( get_data( ) ).

*    UNASSIGN <ls_data_avg>.
    IF iv_get_maximum = abap_true.
      ls_data_max-name = lc_name_maximum.
      LOOP AT lts_data ASSIGNING <ls_data_max>.
        IF <ls_data_max>-count > ls_data_max-count.
          ls_data_max-count = <ls_data_max>-count.
        ENDIF.
      ENDLOOP.

      INSERT ls_data_max INTO TABLE lts_result.
    ENDIF.

    IF iv_get_average = abap_true.
      ls_data_avg-name = lc_name_average.
      LOOP AT lts_data ASSIGNING <ls_data_avg>.
        ls_data_avg-count += <ls_data_avg>-count.
      ENDLOOP.
      ls_data_avg-count /= lines( lts_data ).

      INSERT ls_data_avg INTO TABLE lts_result.
    ENDIF.

    INSERT VALUE #( name  = lc_name_call_count
                    count = sv_call_count ) INTO TABLE lts_result.

    rts_result = CORRESPONDING #( lts_result ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/LocalDeclarationOrderRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/LocalDeclarationOrderTest.java)

