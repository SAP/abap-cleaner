[<-- previous rule](DescribeTableRule.md) | [overview](../rules.md) | [next rule -->](AssertEqualsBooleanRule.md)

# Replace READ TABLE with table expression

Replaces suitable cases of READ TABLE with table expressions, introducing ASSIGN, line\_exists\( \), or line\_index\( \).

Some variants cannot be replaced, esp. if 'FROM wa', 'INTO wa', 'REFERENCE INTO dref' or 'BINARY SEARCH' is used, if multiple SY- fields are evaluated afterwards, or the table is returned by a function or constructor expression.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Prefer functional to procedural language constructs](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-functional-to-procedural-language-constructs)
* [Code Pal for ABAP: Prefer LINE\_EXISTS or LINE\_INDEX to READ TABLE or LOOP AT](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/prefer-line-exists.md)

## Options

* \[X\] Replace with ASSIGN itab\[ ... \]
* \[X\] Replace with line\_exists\( itab\[ ... \] \)
* \[X\] Replace with line\_index\( itab\[ ... \] \)
* \[ \] Keep optional keyword COMPONENTS after KEY name

## Examples


```ABAP

  METHOD read_table_to_table_expression.
    READ TABLE its_any WITH TABLE KEY comp1 = iv_value1 ASSIGNING FIELD-SYMBOL(<ls_any>) ELSE UNASSIGN.
    CHECK sy-subrc <> 0.

    READ TABLE its_any WITH KEY comp2 = iv_value2 TRANSPORTING NO FIELDS. "#EC CI_SORTSEQ
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.

    " note how verbose READ TABLE is compared to a table expression!
    READ TABLE its_any WITH TABLE KEY keyname
                       COMPONENTS comp1 = iv_value1
                                  comp2 = iv_value2
                       TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE its_any WITH TABLE KEY seckey COMPONENTS comp1 = 1 comp2 = abap_true TRANSPORTING NO FIELDS.
    DATA(lv_line_index) = sy-tabix.

    " ----------------------------------------------------------------------------------------------
    " the following variants of READ TABLE can NOT be automatically replaced with table expressions:

    " BINARY SEARCH is not available with table expressions, so replacing this might lead to a full table scan
    READ TABLE it_any WITH KEY comp1 = iv_value1 BINARY SEARCH TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 0.

    " FROM is not available with table expressions
    READ TABLE its_any FROM is_any TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 0.

    " replacing INTO or REFERENCE INTO would require catching the exception CX_SY_TAB_LINE_NOT_FOUND: While
    " VALUE #( itab[ ... ] OPTIONAL ) and REF #( itab[ ... ] OPTIONAL ) work without exception handling, their
    " behavior differs from READ TABLE if the table line is NOT found: In this case, READ TABLE preserves the old
    " value of the variable, while the VALUE #( ) and REF #( ) constructors would initialize the variable
    READ TABLE its_any WITH KEY comp1 = 'abc' INTO DATA(ls_any).
    READ TABLE its_any WITH KEY comp1 = 'def' REFERENCE INTO lr_any.

    " tables retrieved via functional call or constructor expression are not supported by table expressions
    READ TABLE get_table( ) WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.
    lv_line_index = sy-tabix.

    " READ TABLE is kept if SY-SUBRC is checked against a value other than 0, used multiple times, or passed on
    READ TABLE its_any WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 4.
    any_method( iv_subrc = sy-subrc ).

    " this would be equivalent to cl_abap_unit_assert=>assert_true( xsdbool( line_exists( its_any[ ... ] ) ) )
    READ TABLE its_any WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).

    " comments in some places of the READ TABLE statement prevent the automated replacement
    READ TABLE its_any " very important comment which must never get lost
          WITH TABLE KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 0.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD read_table_to_table_expression.
    ASSIGN its_any[ comp1 = iv_value1 ] TO FIELD-SYMBOL(<ls_any>) ELSE UNASSIGN.
    CHECK sy-subrc <> 0.

    IF line_exists( its_any[ comp2 = iv_value2 ] ). "#EC CI_SORTSEQ
      RETURN.
    ENDIF.

    " note how verbose READ TABLE is compared to a table expression!
    IF NOT line_exists( its_any[ KEY keyname
                                 comp1 = iv_value1
                                 comp2 = iv_value2 ] ).
      RETURN.
    ENDIF.

    DATA(lv_line_index) = line_index( its_any[ KEY seckey
                                               comp1 = 1
                                               comp2 = abap_true ] ).

    " ----------------------------------------------------------------------------------------------
    " the following variants of READ TABLE can NOT be automatically replaced with table expressions:

    " BINARY SEARCH is not available with table expressions, so replacing this might lead to a full table scan
    READ TABLE it_any WITH KEY comp1 = iv_value1 BINARY SEARCH TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 0.

    " FROM is not available with table expressions
    READ TABLE its_any FROM is_any TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 0.

    " replacing INTO or REFERENCE INTO would require catching the exception CX_SY_TAB_LINE_NOT_FOUND: While
    " VALUE #( itab[ ... ] OPTIONAL ) and REF #( itab[ ... ] OPTIONAL ) work without exception handling, their
    " behavior differs from READ TABLE if the table line is NOT found: In this case, READ TABLE preserves the old
    " value of the variable, while the VALUE #( ) and REF #( ) constructors would initialize the variable
    READ TABLE its_any WITH KEY comp1 = 'abc' INTO DATA(ls_any).
    READ TABLE its_any WITH KEY comp1 = 'def' REFERENCE INTO lr_any.

    " tables retrieved via functional call or constructor expression are not supported by table expressions
    READ TABLE get_table( ) WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.
    lv_line_index = sy-tabix.

    " READ TABLE is kept if SY-SUBRC is checked against a value other than 0, used multiple times, or passed on
    READ TABLE its_any WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 4.
    any_method( iv_subrc = sy-subrc ).

    " this would be equivalent to cl_abap_unit_assert=>assert_true( xsdbool( line_exists( its_any[ ... ] ) ) )
    READ TABLE its_any WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).

    " comments in some places of the READ TABLE statement prevent the automated replacement
    READ TABLE its_any " very important comment which must never get lost
          WITH TABLE KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 0.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/ReadTableRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/ReadTableTest.java)

