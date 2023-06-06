[<-- previous rule](EmptyLinesInClassDefinitionRule.md) | [overview](../rules.md) | [next rule -->](ClosingBracketsPositionRule.md)

# Standardize spaces next to parentheses

Removes multiple spaces from empty parentheses and adds missing spaces between parentheses and character literals.

## References

* [Clean ABAP Styleguide: Condense your code](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#condense-your-code)

## Options

* \[X\] Remove multiple spaces from empty parentheses
* \[X\] Add space between parentheses and character literals
* \[ \] Add space in condensed cases with single character literal: ...\('...'\)

## Examples


```ABAP

  METHOD standardize_spaces_in_parens.
    ev_result = cl_any_factory=>get(     )->get_utility(   )->get_value(      ).

    get_util(    )->any_method( iv_any_param   = get_default_value(    )
                                iv_other_param = VALUE #(       ) ).

    " these cases are syntactically correct even without spaces:
    any_method('text field literal').
    any_method('other literal' ).
    any_method( 'third literal').

    lt_any_table = VALUE #( ( '111')
                            ( '222' )
                            ( '333') ).

    lt_value = lt_other_table[ name = 'abc']-value.

    lt_amount = VALUE #( ( CONV #( '11.11') )
                         ( CONV #('22.22' ) )
                         ( CONV #( '33.33') ) ).

    lt_amount = VALUE #( ( CONV #('11.11') )
                         ( CONV #('22.22') )
                         ( CONV #('33.33') ) ).

    " introducing spaces here would be a syntax error:
    CALL METHOD lo_instance->('METHOD_NAME').
    ls_struc-('COMPONENT_NAME') = 1.
    lr_data_ref->('COMPONENT_NAME') = 1.

    " the first two cases are executable but create a syntax warning without a space after the opening (
    other_method(`text string literal`).
    other_method(`other literal` ).
    other_method( `third literal`).

    lt_other_table = VALUE #( ( text = `abc` )
                              ( text = `def`)
                              ( text = `ghi`) ).
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD standardize_spaces_in_parens.
    ev_result = cl_any_factory=>get( )->get_utility( )->get_value( ).

    get_util( )->any_method( iv_any_param   = get_default_value( )
                             iv_other_param = VALUE #( ) ).

    " these cases are syntactically correct even without spaces:
    any_method('text field literal').
    any_method( 'other literal' ).
    any_method( 'third literal' ).

    lt_any_table = VALUE #( ( '111' )
                            ( '222' )
                            ( '333' ) ).

    lt_value = lt_other_table[ name = 'abc' ]-value.

    lt_amount = VALUE #( ( CONV #( '11.11' ) )
                         ( CONV #( '22.22' ) )
                         ( CONV #( '33.33' ) ) ).

    lt_amount = VALUE #( ( CONV #('11.11') )
                         ( CONV #('22.22') )
                         ( CONV #('33.33') ) ).

    " introducing spaces here would be a syntax error:
    CALL METHOD lo_instance->('METHOD_NAME').
    ls_struc-('COMPONENT_NAME') = 1.
    lr_data_ref->('COMPONENT_NAME') = 1.

    " the first two cases are executable but create a syntax warning without a space after the opening (
    other_method(`text string literal`).
    other_method( `other literal` ).
    other_method( `third literal` ).

    lt_other_table = VALUE #( ( text = `abc` )
                              ( text = `def` )
                              ( text = `ghi` ) ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/spaces/SpacesInEmptyBracketsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/spaces/SpacesInEmptyBracketsTest.java)

