[<-- previous rule](SpaceAroundCommentSignRule.md) | [overview](../rules.md) | [next rule -->](ChainRule.md)

# Remove needless spaces

Removes multiple spaces where no alignment intention can be identified.

This rule deliberately skips commands and expressions which are covered by more dedicated rules on alignment and spaces.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Condense your code](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#condense-your-code)

## Options

* \[X\] Search for intended alignment across empty lines
* \[X\] Search for intended alignment across comment lines
* \[ \] Process line-end comments, too
* \[X\] Remove multiple spaces from empty parentheses

## Examples


```ABAP

  METHOD   remove_needless_spaces.
    CLEAR:    ev_value,
              ev_other_value.
    CLEAR     ev_third_value.

    SORT lt_table   BY  first_comp    ASCENDING
                        second_comp   DESCENDING
                        third_comp.

    " these assignments contain needless spaces, but also intentional alignment
    a           =   1.     " comment A
    bbb         =   3.     " comment B

    ccccc       =   5.     " comment C

    " the next two assignments may also be intentionally aligned with previous ones,
    " however, with a comment in between, they may as well be aligned independently
    ddddddd     =   7.      " comment D
    eeeeeeeee   =   9.      " comment E

    " existing right-alignment of assignment operators is kept:
    lv_instance     ?=   get_utility( ).
    lv_any_value     =   'abc'   &&  'def'.
    lv_other_value   =   get_value( )  +  get_another_value( ).
    lv_third_value  +=   3    *   4.

    " for numbers, existing alignment by the decimal separator is preserved:
    lts_table[ 1 ]-value    =      42.
    lts_table[ 2 ]-value    =    -100.
    lts_table[ 3 ]-value    =      '3.1415'.
    lts_table[ 4 ]-value    =    '-12.34'.

    ev_result = cl_any_factory=>get(     )->get_utility(   )->get_value(      ).

    get_util(    )->any_method( iv_any_param   = get_default_value(    )
                                iv_other_param = VALUE #(       ) ).

  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD remove_needless_spaces.
    CLEAR: ev_value,
           ev_other_value.
    CLEAR  ev_third_value.

    SORT lt_table BY first_comp  ASCENDING
                     second_comp DESCENDING
                     third_comp.

    " these assignments contain needless spaces, but also intentional alignment
    a         = 1.     " comment A
    bbb       = 3.     " comment B

    ccccc     = 5.     " comment C

    " the next two assignments may also be intentionally aligned with previous ones,
    " however, with a comment in between, they may as well be aligned independently
    ddddddd   = 7.      " comment D
    eeeeeeeee = 9.      " comment E

    " existing right-alignment of assignment operators is kept:
    lv_instance    ?= get_utility( ).
    lv_any_value    = 'abc' && 'def'.
    lv_other_value  = get_value( ) + get_another_value( ).
    lv_third_value += 3 * 4.

    " for numbers, existing alignment by the decimal separator is preserved:
    lts_table[ 1 ]-value =   42.
    lts_table[ 2 ]-value = -100.
    lts_table[ 3 ]-value =   '3.1415'.
    lts_table[ 4 ]-value = '-12.34'.

    ev_result = cl_any_factory=>get( )->get_utility( )->get_value( ).

    get_util( )->any_method( iv_any_param   = get_default_value( )
                             iv_other_param = VALUE #( ) ).

  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/spaces/NeedlessSpacesRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/spaces/NeedlessSpacesTest.java)

