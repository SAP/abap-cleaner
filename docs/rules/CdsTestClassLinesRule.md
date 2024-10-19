[<-- previous rule](EmptyLinesInClassDefinitionRule.md) | [overview](../rules.md) | [next rule -->](SpaceAroundTextLiteralRule.md)

# Standardize test classes for CDS views

Standardizes empty lines and removes repetitive ABAP Doc instructions from test classes for CDS views.

This rule only works on local test classes with a "\!@testing <CDS view name> annotation, as created by the ADT command 'New ABAP Test Class'.

## Options

* \[X\] Remove generated instructions in ABAP Doc
* Remove comment 'TODO: Provide the test data here' \[if empty VALUE constructor was changed\]
* \[X\] Insert empty line between multi-line VALUE and INSERT\_TEST\_DATA
* \[X\] Insert empty line above SELECT statement
* \[X\] Insert empty line below SELECT statement
* \[ \] Move PREPARE\_... method\(s\) above the first FOR TESTING method

## Examples


```ABAP

"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT
CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CLASS-DATA environment TYPE REF TO if_cds_test_environment.

    DATA td_i_businesspartner           TYPE STANDARD TABLE OF i_businesspartner WITH EMPTY KEY.
    DATA td_i_customer                  TYPE STANDARD TABLE OF i_customer WITH EMPTY KEY.
    DATA act_results                    TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY.

    "! In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.
    CLASS-METHODS class_setup RAISING cx_static_check.
    "! In CLASS_TEARDOWN, Generated database entities (doubles & clones) should be deleted at the end of test class execution.
    CLASS-METHODS class_teardown.

    "! SETUP method creates a common start state for each test method,
    "! clear_doubles clears the test data for all the doubles used in the test method before each test method execution.
    METHODS setup RAISING cx_static_check.
    METHODS prepare_testdata.
    "! In this method test data is inserted into the generated double(s) and the test is executed and
    "! the results should be asserted with the actuals.
    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.
  METHOD class_setup.
    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPERFOBLGNWITHTOCURPERDAMT' ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD aunit_for_cds_method.
    prepare_testdata( ).
    " query the view under test
    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt
      INTO TABLE @act_results.
    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).
  ENDMETHOD.

  METHOD prepare_testdata.
    " Prepare test data for 'i_businesspartner'
    " TODO: Provide the test data here
    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )
                                    ( BusinessPartner = 'other' ) ).
    environment->insert_test_data( i_data = td_i_businesspartner ).

    " Prepare test data for 'i_customer'
    " TODO: Provide the test data here
    td_i_customer = VALUE #( ( ) ).
    environment->insert_test_data( i_data = td_i_customer ).
  ENDMETHOD.
ENDCLASS.
```

Resulting code:

```ABAP

"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT
CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CLASS-DATA environment TYPE REF TO if_cds_test_environment.

    DATA td_i_businesspartner           TYPE STANDARD TABLE OF i_businesspartner WITH EMPTY KEY.
    DATA td_i_customer                  TYPE STANDARD TABLE OF i_customer WITH EMPTY KEY.
    DATA act_results                    TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY.

    CLASS-METHODS class_setup RAISING cx_static_check.
    CLASS-METHODS class_teardown.

    METHODS setup RAISING cx_static_check.
    METHODS prepare_testdata.
    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.
  METHOD class_setup.
    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPERFOBLGNWITHTOCURPERDAMT' ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD aunit_for_cds_method.
    prepare_testdata( ).

    " query the view under test
    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt
      INTO TABLE @act_results.

    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).
  ENDMETHOD.

  METHOD prepare_testdata.
    " Prepare test data for 'i_businesspartner'
    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )
                                    ( BusinessPartner = 'other' ) ).

    environment->insert_test_data( i_data = td_i_businesspartner ).

    " Prepare test data for 'i_customer'
    " TODO: Provide the test data here
    td_i_customer = VALUE #( ( ) ).
    environment->insert_test_data( i_data = td_i_customer ).
  ENDMETHOD.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/emptylines/CdsTestClassLinesRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/emptylines/CdsTestClassLinesTest.java)

