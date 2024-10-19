[<-- previous rule](CamelCaseNameRule.md) | [overview](../rules.md) | [next rule -->](IndentRule.md)

# Use CamelCase in test class for CDS view

Changes known VDM CDS view names to CamelCase \(in class name, variable names, literals, comments etc.\).

This rule only works on local test classes with a "\!@testing <CDS view name> annotation, as created by the ADT command 'New ABAP Test Class'. Custom view and field names from rule 'Use CamelCase for known CDS names' are reused.

## Options

* \[X\] Apply CamelCase to class name
* \[X\] Apply CamelCase to variable names
* \[X\] Apply CamelCase to literals in CL\_CDS\_TEST\_ENVIRONMENT=>CREATE calls
* \[X\] Apply CamelCase to generated comments 'Prepare test data for ...'
* \[X\] Add pseudo comment "\#EC CI\_NOWHERE to SELECT on tested view

## Examples


```ABAP

"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT
CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CLASS-DATA environment TYPE REF TO if_cds_test_environment.

    " table names are put to CamelCase if they start with a prefix like td_, lt_, lts_ etc.,
    " followed by the CDS view name from the respective TYPE definition (possibly shortened)
    DATA td_i_raperfoblgnwithtocurperda TYPE STANDARD TABLE OF i_raperfoblgnwithtocurperdamt WITH EMPTY KEY.
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
    " CamelCase can even be applied to literals: inside the test environment, to_upper( ) is used
    environment = cl_cds_test_environment=>create(
                    i_for_entity      = 'C_RAPERFOBLGNWITHTOCURPERDAMT'
                    i_dependency_list = VALUE #( type ='CDS_VIEW'
                                                 ( name = 'I_RAPERFOBLGNWITHTOCURPERDAMT' )
                                                 ( name = 'I_BUSINESSPARTNER' )
                                                 ( name = 'I_CUSTOMER' ) ) ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD prepare_testdata.
    " Prepare test data for 'i_raperfoblgnwithtocurperdamt'
    td_i_raperfoblgnwithtocurperda = VALUE #( ( ) ).
    environment->insert_test_data( i_data = td_i_raperfoblgnwithtocurperda ).

    " Prepare test data for 'i_businesspartner'
    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )
                                    ( BusinessPartner = 'other' ) ).
    environment->insert_test_data( i_data = td_i_businesspartner ).

    " Prepare test data for 'i_customer'
    td_i_customer = VALUE #( ( Customer = 'Any' ) ).
    environment->insert_test_data( i_data = td_i_customer ).
  ENDMETHOD.

  METHOD aunit_for_cds_method.
    prepare_testdata( ).

    " since a SELECT on the view under test only accesses the test doubles that were prepared above,
    " a WHERE clause is often not required, so #EC CI_NOWHERE can be added to satisfy code checks
    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt INTO TABLE @act_results.

    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).
  ENDMETHOD.
ENDCLASS.
```

Resulting code:

```ABAP

"!@testing C_RAPerfOblgnWithToCurPerdAmt
CLASS ltc_C_RAPerfOblgnWithToCurPerd DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CLASS-DATA environment TYPE REF TO if_cds_test_environment.

    " table names are put to CamelCase if they start with a prefix like td_, lt_, lts_ etc.,
    " followed by the CDS view name from the respective TYPE definition (possibly shortened)
    DATA td_I_RAPerfOblgnWithToCurPerdA TYPE STANDARD TABLE OF I_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.
    DATA td_I_BusinessPartner           TYPE STANDARD TABLE OF I_BusinessPartner WITH EMPTY KEY.
    DATA td_I_Customer                  TYPE STANDARD TABLE OF I_Customer WITH EMPTY KEY.
    DATA act_results                    TYPE STANDARD TABLE OF C_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.

    CLASS-METHODS class_setup RAISING cx_static_check.
    CLASS-METHODS class_teardown.

    METHODS setup RAISING cx_static_check.
    METHODS prepare_testdata.
    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_C_RAPerfOblgnWithToCurPerd IMPLEMENTATION.
  METHOD class_setup.
    " CamelCase can even be applied to literals: inside the test environment, to_upper( ) is used
    environment = cl_cds_test_environment=>create(
                    i_for_entity      = 'C_RAPerfOblgnWithToCurPerdAmt'
                    i_dependency_list = VALUE #( type ='CDS_VIEW'
                                                 ( name = 'I_RAPerfOblgnWithToCurPerdAmt' )
                                                 ( name = 'I_BusinessPartner' )
                                                 ( name = 'I_Customer' ) ) ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD prepare_testdata.
    " Prepare test data for 'I_RAPerfOblgnWithToCurPerdAmt'
    td_I_RAPerfOblgnWithToCurPerdA = VALUE #( ( ) ).
    environment->insert_test_data( i_data = td_I_RAPerfOblgnWithToCurPerdA ).

    " Prepare test data for 'I_BusinessPartner'
    td_I_BusinessPartner = VALUE #( ( BusinessPartner = 'any' )
                                    ( BusinessPartner = 'other' ) ).
    environment->insert_test_data( i_data = td_I_BusinessPartner ).

    " Prepare test data for 'I_Customer'
    td_I_Customer = VALUE #( ( Customer = 'Any' ) ).
    environment->insert_test_data( i_data = td_I_Customer ).
  ENDMETHOD.

  METHOD aunit_for_cds_method.
    prepare_testdata( ).

    " since a SELECT on the view under test only accesses the test doubles that were prepared above,
    " a WHERE clause is often not required, so #EC CI_NOWHERE can be added to satisfy code checks
    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt INTO TABLE @act_results. "#EC CI_NOWHERE

    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).
  ENDMETHOD.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/prettyprinter/CamelCaseInCdsTestRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/prettyprinter/CamelCaseInCdsTestTest.java)

