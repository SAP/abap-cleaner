[<-- previous rule](MoveToRule.md) | [overview](../rules.md) | [next rule -->](CondenseRule.md)

# Replace TRANSLATE with string functions

Replaces the deprecated TRANSLATE statement with corresponding string processing functions.

'TRANSLATE text USING mask' is only replaced if the mask is a literal.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Prefer functional to procedural language constructs](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-functional-to-procedural-language-constructs)
* [Code Pal for ABAP: Deprecated Key Word Check](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/deprecated-key-word.md)
* [ABAP Keyword Documentation: TRANSLATE](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaptranslate.htm)
* [ABAP Keyword Documentation: string\_func - Processing Functions](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenprocess_functions.htm)

## Options

* \[X\] Replace TRANSLATE ... TO UPPER|LOWER
* \[X\] Replace TRANSLATE ... USING
* \[X\] Replace TRANSLATE ... USING if mask has an uneven number of chars
* \[X\] Unchain TRANSLATE: chains \(required for processing them with this rule\)
* \[X\] Only replace TRANSLATE for known unstructured types \(STRING, C, N, CHAR1 etc.\)
* Warning: deactivating this option might lead to syntax errors if your code contains TRANSLATE with structured types \(but at least the syntax check will immediately show this\)

## Examples


```ABAP

  METHOD replace_deprecated_translate.
    " change text to lower and then to upper case
    DATA lv_text TYPE string VALUE `Any Text`.
    TRANSLATE lv_text TO LOWER CASE. " `any text`
    TRANSLATE lv_text TO UPPER CASE. " `ANY TEXT`

    " replace a with b, A with B and back to get `Abracadabra`
    DATA lv_magic TYPE string VALUE `Barbcbdbarb`.
    TRANSLATE lv_magic USING 'abbaABBA'.

    " replace invalid chars in file name to get `not-all-chars-are-allowed`
    DATA lv_file_name TYPE string VALUE `not:all?chars\are/allowed`.
    TRANSLATE lv_file_name USING `\-/-:-*-?-"-<->-|-`.

    " TRANSLATE results in `a b c ` here; for translate( ), it is important to use
    " text string literals `...` for the FROM and TO parameters, because the trailing
    " spaces of a text field literal TO = '   ' would instead result in `abc`:
    DATA lv_abc TYPE string VALUE `a1b2c3`.
    TRANSLATE lv_abc USING '1 2 3 '.

    " if the mask has an uneven number of characters, TRANSLATE simply ignores
    " the last replacement, so in this case we get `C++` (NOT `C  ` or `C`); therefore,
    " translate( ) must have FROM = `c`, since FROM = `c+`  TO = `C` would remove the `+`
    DATA lv_c_plus_plus TYPE string VALUE `c++`.
    TRANSLATE lv_c_plus_plus USING 'cC+'.

    " chains can only be processed if they are first unchained
    DATA lv_other_text TYPE char5 VALUE 'abcde'.
    TRANSLATE: lv_text TO LOWER CASE, lv_other_text TO UPPER CASE.

    TRANSLATE lv_abc USING: '1 2 3 ', 'a-b-c-'.

    " unlike TRANSLATE, the string functions do not work on structured data; therefore,
    " changing the next statements might cause syntax errors. You can activate the option
    " 'Only replace TRANSLATE for known unstructured types' to restrict this cleanup rule
    " to cases in which ABAP cleaner can clearly determine the type (as above)
    DATA(ls_unknown_type) = get_charlike_structure( ).
    DATA ls_ddic_type TYPE some_ddic_type.
    DATA ls_other_type TYPE if_any_interface=>ty_s_typedef_out_of_sight.
    TRANSLATE ls_unknown_type TO UPPER CASE.
    TRANSLATE ls_ddic_type TO LOWER CASE.
    TRANSLATE ls_other_type USING 'a1b2'.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD replace_deprecated_translate.
    " change text to lower and then to upper case
    DATA lv_text TYPE string VALUE `Any Text`.
    lv_text = to_lower( lv_text ). " `any text`
    lv_text = to_upper( lv_text ). " `ANY TEXT`

    " replace a with b, A with B and back to get `Abracadabra`
    DATA lv_magic TYPE string VALUE `Barbcbdbarb`.
    lv_magic = translate( val  = lv_magic
                          from = `abAB`
                          to   = `baBA` ).

    " replace invalid chars in file name to get `not-all-chars-are-allowed`
    DATA lv_file_name TYPE string VALUE `not:all?chars\are/allowed`.
    lv_file_name = translate( val  = lv_file_name
                              from = `\/:*?"<>|`
                              to   = `---------` ).

    " TRANSLATE results in `a b c ` here; for translate( ), it is important to use
    " text string literals `...` for the FROM and TO parameters, because the trailing
    " spaces of a text field literal TO = '   ' would instead result in `abc`:
    DATA lv_abc TYPE string VALUE `a1b2c3`.
    lv_abc = translate( val  = lv_abc
                        from = `123`
                        to   = `   ` ).

    " if the mask has an uneven number of characters, TRANSLATE simply ignores
    " the last replacement, so in this case we get `C++` (NOT `C  ` or `C`); therefore,
    " translate( ) must have FROM = `c`, since FROM = `c+`  TO = `C` would remove the `+`
    DATA lv_c_plus_plus TYPE string VALUE `c++`.
    lv_c_plus_plus = translate( val  = lv_c_plus_plus
                                from = `c`
                                to   = `C` ).

    " chains can only be processed if they are first unchained
    DATA lv_other_text TYPE char5 VALUE 'abcde'.
    lv_text = to_lower( lv_text ).
    lv_other_text = to_upper( lv_other_text ).

    lv_abc = translate( val  = lv_abc
                        from = `123`
                        to   = `   ` ).
    lv_abc = translate( val  = lv_abc
                        from = `abc`
                        to   = `---` ).

    " unlike TRANSLATE, the string functions do not work on structured data; therefore,
    " changing the next statements might cause syntax errors. You can activate the option
    " 'Only replace TRANSLATE for known unstructured types' to restrict this cleanup rule
    " to cases in which ABAP cleaner can clearly determine the type (as above)
    DATA(ls_unknown_type) = get_charlike_structure( ).
    DATA ls_ddic_type TYPE some_ddic_type.
    DATA ls_other_type TYPE if_any_interface=>ty_s_typedef_out_of_sight.
    TRANSLATE ls_unknown_type TO UPPER CASE.
    TRANSLATE ls_ddic_type TO LOWER CASE.
    TRANSLATE ls_other_type USING 'a1b2'.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/TranslateRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/TranslateTest.java)

