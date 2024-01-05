[<-- previous rule](EmptyLinesInClassDefinitionRule.md) | [overview](../rules.md) | [next rule -->](ClosingBracketsPositionRule.md)

# Put spaces around text literals

Adds missing spaces before and after text field literals '...' and text string literals \`...\`.

## Options

* \[X\] Add space between keywords and text literals
* \[X\] Add space between operators and text literals
* \[X\] Add space between text literals and comments
* \[X\] Add space between parentheses and character literals
* \[X\] Add space in condensed cases with single character literal: ...\('...'\)

## Examples


```ABAP

  METHOD space_around_text_literal.
    DATA lv_any TYPE string VALUE`abc`.
    DATA lv_other TYPE string VALUE'abc'.

    ASSERT lv_any EQ`abc`.
    ASSERT lv_any EQ'abc'.

    lv_any =`abc` &&`def`.
    lv_any ='abc' &&'def'.
    lv_any =:`abc`,`def`.
    lv_any =:'abc','def'.

    lv_any =`abc`"comment
          &&`def`.
    lv_any ='abc'"comment
          &&'def'.

    " these cases are syntactically correct even without spaces:
    any_method('text field literal').
    any_method('other literal' ).
    any_method( 'third literal').

    lt_any_table = VALUE #( ( '111')
                            ( '222' )
                            ( '333') ).

    lt_value = lt_other_table[ name ='abc' ]-value.
    lt_any[`1` ]-num =`a`.
    lt_any['1' ]-num ='a'.

    lt_amount = VALUE #( ( CONV #( '11.11') )
                         ( CONV #('22.22' ) )
                         ( CONV #( '33.33') ) ).

    lt_amount = VALUE #( ( CONV #('11.11') )
                         ( CONV #('22.22') )
                         ( CONV #('33.33') ) ).

    " the first two cases are executable but create a syntax warning without a space after the opening (
    other_method(`text string literal`).
    other_method(`other literal` ).
    other_method( `third literal`).

    lt_other_table = VALUE #( ( text = `abc` )
                              ( text = `def`)
                              ( text = `ghi`) ).

    " introducing spaces here would be a syntax error:
    CALL METHOD lo_instance->('METHOD_NAME').
    CALL METHOD ('CLASS_NAME')=>('METHOD_NAME').
    ls_struc-('COMPONENT_NAME') = 1.
    lr_data_ref->('COMPONENT_NAME') = 1.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD space_around_text_literal.
    DATA lv_any TYPE string VALUE `abc`.
    DATA lv_other TYPE string VALUE 'abc'.

    ASSERT lv_any EQ `abc`.
    ASSERT lv_any EQ 'abc'.

    lv_any = `abc` && `def`.
    lv_any = 'abc' && 'def'.
    lv_any =: `abc`, `def`.
    lv_any =: 'abc', 'def'.

    lv_any = `abc` "comment
          && `def`.
    lv_any = 'abc' "comment
          && 'def'.

    " these cases are syntactically correct even without spaces:
    any_method( 'text field literal' ).
    any_method( 'other literal' ).
    any_method( 'third literal' ).

    lt_any_table = VALUE #( ( '111' )
                            ( '222' )
                            ( '333' ) ).

    lt_value = lt_other_table[ name = 'abc' ]-value.
    lt_any[ `1` ]-num = `a`.
    lt_any[ '1' ]-num = 'a'.

    lt_amount = VALUE #( ( CONV #( '11.11' ) )
                         ( CONV #( '22.22' ) )
                         ( CONV #( '33.33' ) ) ).

    lt_amount = VALUE #( ( CONV #( '11.11' ) )
                         ( CONV #( '22.22' ) )
                         ( CONV #( '33.33' ) ) ).

    " the first two cases are executable but create a syntax warning without a space after the opening (
    other_method( `text string literal` ).
    other_method( `other literal` ).
    other_method( `third literal` ).

    lt_other_table = VALUE #( ( text = `abc` )
                              ( text = `def` )
                              ( text = `ghi` ) ).

    " introducing spaces here would be a syntax error:
    CALL METHOD lo_instance->('METHOD_NAME').
    CALL METHOD ('CLASS_NAME')=>('METHOD_NAME').
    ls_struc-('COMPONENT_NAME') = 1.
    lr_data_ref->('COMPONENT_NAME') = 1.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/spaces/SpaceAroundTextLiteralRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/spaces/SpaceAroundTextLiteralTest.java)

