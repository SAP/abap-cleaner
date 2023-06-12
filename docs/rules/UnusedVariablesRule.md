[<-- previous rule](LocalDeclarationOrderRule.md) | [overview](../rules.md) | [next rule -->](ChainOfOneRule.md)

# Delete unused variables

Deletes unused variables, or comments them out if they are 'used' in commented-out code. TODO comments can be added for variables that are assigned but never used.

Note that this rule will skip methods in which macros are used.

## Options

* Action for local variables that are never used: \[delete\]
* Action for variables only used in commented-out code: \[comment out with \*\]
* Action for assigned but unused local variables: \[add TODO comment\]
* Action for assigned variables only used in commented-out code: \[add TODO comment\]
* Action for local constants that are never used: \[comment out with \*\]
* Action for constants only used in commented-out code: \[comment out with \*\]

## Examples


```ABAP

  METHOD delete_unused_variables.
    CONSTANTS lc_unused        TYPE i VALUE 1200.
    CONSTANTS lc_commented_out TYPE i VALUE 2.
    CONSTANTS lc_used_constant TYPE i VALUE 3.

    DATA lv_unused_a      TYPE i.
    DATA: lv_unused_b      TYPE string,
          lv_used_var      TYPE i,
          lv_commented_out LIKE lv_used_var.

    DATA lv_only_assigned TYPE i.
    DATA lv_assigned_but_used_incomment TYPE i.

    " with the ##NEEDED pragma, an unused variable will be kept and no TODO added;
    " the pragma also prevents a warning from the Extended Check (SLIN)
    DATA lv_unused_but_needed TYPE string ##NEEDED.

    FIELD-SYMBOLS:
      <ls_unused_a>      TYPE ty_s_structure,
      <ls_commented_out> LIKE LINE OF its_table,
      <ls_used>          TYPE tt_table.

    CLEAR ev_count.

    " the following variable assigned but unused; however, the call may have side effects
    DATA(lo_unused_util) = get_utility( ). 

    lv_used_var = 0.
    DO lc_used_constant TIMES.
      LOOP AT its_table ASSIGNING <ls_used>.
        lv_used_var += <ls_used>-num.
      ENDLOOP.
    ENDDO.
    ev_count = lv_used_var.

    " lv_only_assigned is only used to modifying its own value
    CLEAR lv_only_assigned.
    lv_only_assigned = lv_only_assigned + 2.
    MULTIPLY lv_only_assigned BY 2.

    lv_assigned_but_used_incomment = 1.

*    this comment mentions the variable lv_unused_a, but that does not count:
*    only variables in commented-out ABAP code count, not variables in text comments!
*
*    lv_commented_out = 0.
*    DO lc_commented_out * lv_assigned_but_used_incomment TIMES.
*      LOOP AT its_table ASSIGNING <ls_commented_out>.
*        lv_commented_out += 1.
*      ENDLOOP.
*    ENDDO.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD delete_unused_variables.
*    CONSTANTS lc_unused        TYPE i VALUE 1200.
*    CONSTANTS lc_commented_out TYPE i VALUE 2.
    CONSTANTS lc_used_constant TYPE i VALUE 3.

    DATA: lv_used_var      TYPE i.
*          lv_commented_out LIKE lv_used_var.

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_only_assigned TYPE i.
    " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
    DATA lv_assigned_but_used_incomment TYPE i.

    " with the ##NEEDED pragma, an unused variable will be kept and no TODO added;
    " the pragma also prevents a warning from the Extended Check (SLIN)
    DATA lv_unused_but_needed TYPE string ##NEEDED.

*    FIELD-SYMBOLS: <ls_commented_out> LIKE LINE OF its_table,
    FIELD-SYMBOLS:
      <ls_used>          TYPE tt_table.

    CLEAR ev_count.

    " the following variable assigned but unused; however, the call may have side effects
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lo_unused_util) = get_utility( ).

    lv_used_var = 0.
    DO lc_used_constant TIMES.
      LOOP AT its_table ASSIGNING <ls_used>.
        lv_used_var += <ls_used>-num.
      ENDLOOP.
    ENDDO.
    ev_count = lv_used_var.

    " lv_only_assigned is only used to modifying its own value
    CLEAR lv_only_assigned.
    lv_only_assigned = lv_only_assigned + 2.
    MULTIPLY lv_only_assigned BY 2.

    lv_assigned_but_used_incomment = 1.

*    this comment mentions the variable lv_unused_a, but that does not count:
*    only variables in commented-out ABAP code count, not variables in text comments!
*
*    lv_commented_out = 0.
*    DO lc_commented_out * lv_assigned_but_used_incomment TIMES.
*      LOOP AT its_table ASSIGNING <ls_commented_out>.
*        lv_commented_out += 1.
*      ENDLOOP.
*    ENDDO.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/UnusedVariablesRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/UnusedVariablesTest.java)

