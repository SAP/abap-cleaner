[<-- previous rule](UpperAndLowerCaseRule.md) | [overview](../rules.md) | [next rule -->](AlignAbapDocRule.md)

# Indent lines

Moves all commands and comments to the correct indentation. Relative indents within the lines of a command \(or command chain\) are not changed.

## References

* [Clean ABAP Styleguide: Use the Pretty Printer before activating](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-the-pretty-printer-before-activating)
* [Clean ABAP Styleguide: Use your Pretty Printer team settings](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-your-pretty-printer-team-settings)
* [Clean Code Checks: Comment Position](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/comment-position.md)

## Options

* \[X\] Execute on CLASS ... DEFINITION sections
* Align with following ELSEIF or ELSE \[if comment is preceded by blank line\]
* Align with following WHEN \[if comment is preceded by blank line\]
* Align with following CATCH or CLEANUP \[if comment is preceded by blank line\]

## Examples


```ABAP

METHOD pretty_print_indent.
DO 4 TIMES.
LOOP AT its_table ASSIGNING FIELD-SYMBOL(<ls_row>).
IF <ls_row>-ignore = abap_true.
" comment
CONTINUE.
ENDIF.
TRY.
" comment
IF iv_value = 1.
" comment on next line
iv_value += 3.
" comment with no empty line above it
ELSEIF iv_value = 2.
iv_value += 2.

" comment on ELSE branch with an empty line above it
ELSE.
iv_value += 1.
ENDIF.

CASE iv_value.
" comment on WHEN
WHEN 1.
" comment on next line
iv_value += 1.

" comment on WHEN with an empty line above it
WHEN 2.
iv_value += 2.
" comment with no empty line above it
WHEN 3.
iv_value += 3.
ENDCASE.

" comment on CATCH with an empty line above it
CATCH.
" no handler
ENDTRY.
ENDLOOP.
ENDDO.
ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD pretty_print_indent.
    DO 4 TIMES.
      LOOP AT its_table ASSIGNING FIELD-SYMBOL(<ls_row>).
        IF <ls_row>-ignore = abap_true.
          " comment
          CONTINUE.
        ENDIF.
        TRY.
            " comment
            IF iv_value = 1.
              " comment on next line
              iv_value += 3.
              " comment with no empty line above it
            ELSEIF iv_value = 2.
              iv_value += 2.

            " comment on ELSE branch with an empty line above it
            ELSE.
              iv_value += 1.
            ENDIF.

            CASE iv_value.
              " comment on WHEN
              WHEN 1.
                " comment on next line
                iv_value += 1.

              " comment on WHEN with an empty line above it
              WHEN 2.
                iv_value += 2.
                " comment with no empty line above it
              WHEN 3.
                iv_value += 3.
            ENDCASE.

          " comment on CATCH with an empty line above it
          CATCH.
            " no handler
        ENDTRY.
      ENDLOOP.
    ENDDO.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/prettyprinter/IndentRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/prettyprinter/IndentTest.java)

