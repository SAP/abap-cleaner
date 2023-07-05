[<-- previous rule](AbapDocLangRule.md) | [overview](../rules.md) | [next rule -->](EndOfCommentRule.md)

# Comment with ", not with \* \(for text\)

Replaces \* comments with " comments, if the comment contains English text \(rather than ABAP code\). If consecutive lines have different indents, relative indents are kept.

## References

* [Clean ABAP Styleguide: Comment with ", not with \*](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#comment-with--not-with-)
* [Clean Code Checks: Comment Type](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/comment-type.md)

## Options

* Leading \*\*\* separators: \[remove\]
* Trailing \*\*\* separators: \[convert to ---\]

## Examples


```ABAP

  METHOD comment_type_for_text.
*****************************************************************
* this check automatically distinguishes between 
*   a) text comments, which will be transformed into " comments
*   b) commented-out code lines, which will be kept as * comments
*****************************************************************

* ------ 'given' ----------------
* create a contract with two fulfillments 
    lo_contract_crt = create_contract( ).

*    " fulfill the contract
*    lo_contract_ff1 = fulfill( io_contract           = lo_contract_crt
*                               iv_fulfillment_number = lc_fulfill_num_1 ).
*    lo_contract_ff2 = fulfill( io_contract           = lo_contract_ff1
*                               iv_fulfillment_number = lc_fulfill_num_2 ).

* ------ 'when' ----------------
* perform contract change
    lo_contract_chg = change_contract( io_contract               = lo_contract_ff2
                                       iv_contract_change_number = lc_contract_change_num ).

* ------ 'then' ----------------
* set expected values 
    lo_contract_exp = lo_contract_ff2.
    lo_contract_act = lo_contract_chg.

*    lo_contract_exp->assert_equals_fully( io_act_contract = lo_contract_act ).

*    " sometimes, the asterisk is also used to separate code sections
***  leading asterisks
*    trailing asterisks ******************************
**** leading and trailing asterisks ******************
******************************************************
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD comment_type_for_text.
    " ----------------------------------------------------------------
    " this check automatically distinguishes between 
    "   a) text comments, which will be transformed into " comments
    "   b) commented-out code lines, which will be kept as * comments
    " ----------------------------------------------------------------

    " ------ 'given' ----------------
    " create a contract with two fulfillments 
    lo_contract_crt = create_contract( ).

*    " fulfill the contract
*    lo_contract_ff1 = fulfill( io_contract           = lo_contract_crt
*                               iv_fulfillment_number = lc_fulfill_num_1 ).
*    lo_contract_ff2 = fulfill( io_contract           = lo_contract_ff1
*                               iv_fulfillment_number = lc_fulfill_num_2 ).

    " ------ 'when' ----------------
    " perform contract change
    lo_contract_chg = change_contract( io_contract               = lo_contract_ff2
                                       iv_contract_change_number = lc_contract_change_num ).

    " ------ 'then' ----------------
    " set expected values 
    lo_contract_exp = lo_contract_ff2.
    lo_contract_act = lo_contract_chg.

*    lo_contract_exp->assert_equals_fully( io_act_contract = lo_contract_act ).

    " sometimes, the asterisk is also used to separate code sections
    " leading asterisks
    " trailing asterisks ------------------------------
    " leading and trailing asterisks ------------------
    " -----------------------------------------------------
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/CommentTypeRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/CommentTypeTest.java)

