[<-- previous rule](EndOfCommentRule.md) | [overview](../rules.md) | [next rule -->](PragmaPositionRule.md)

# Replace obsolete pseudo comments with pragmas

Replaces obsolete pseudo comments \("\#EC ...\) for the Extended Program Check \(SLIN\) with corresponding pragmas.

This rule requires a NetWeaver version >= 7.0 EhP2. Note that pseudo comments for Code Inspector \("\#EC CI\_...\) are kept, as they have not been replaced by pragmas yet.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Prefer pragmas to pseudo comments](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-pragmas-to-pseudo-comments)
* [Code Pal for ABAP: Prefer Pragmas to Pseudo Comments](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/prefer-pragmas-to-pseudo-comments.md)
* [ABAP Keyword Documentation: Pseudo Comments for the Extended Program Check](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenpseudo_comment_slin.htm)
* [ABAP Keyword Documentation: Pragmas](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenpragma.htm)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD replace_pseudo_comments.
    CONSTANTS lc_key TYPE ty_key VALUE 'abc'. "#EC NOTEXT

    DATA: a TYPE string, "#ec NEEDED  " additional textual comment
          b TYPE string.

    a = b.

    " pseudo comments for Code Inspector (CI_...) are kept
    LOOP AT lt_data ASSIGNING <ls_data> WHERE id <= iv_id. "#EC CI_SORTSEQ
      MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest. "#ec ENHOK
    ENDLOOP.

    DO 5 TIMES.
      "#EC NEEDED
    ENDDO.

    TRY.
      GET BADI lo_any_badi.
    CATCH cx_badi_not_implemented. "#EC NO_HANDLER nothing to do here
    ENDTRY.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD replace_pseudo_comments.
    CONSTANTS lc_key TYPE ty_key VALUE 'abc' ##NO_TEXT.

    DATA: a TYPE string ##NEEDED, " additional textual comment
          b TYPE string.

    a = b.

    " pseudo comments for Code Inspector (CI_...) are kept
    LOOP AT lt_data ASSIGNING <ls_data> WHERE id <= iv_id. "#EC CI_SORTSEQ
      MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest ##ENH_OK.
    ENDLOOP.

    DO 5 TIMES ##NEEDED.
    ENDDO.

    TRY.
      GET BADI lo_any_badi.
    CATCH cx_badi_not_implemented ##NO_HANDLER. " nothing to do here
    ENDTRY.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/PseudoCommentRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/PseudoCommentTest.java)

