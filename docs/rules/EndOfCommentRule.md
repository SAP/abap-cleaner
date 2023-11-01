[<-- previous rule](CommentTypeRule.md) | [overview](../rules.md) | [next rule -->](PseudoCommentRule.md)

# Remove end-of comments

Removes comments after ENDCLASS, ENDMETHOD, ENDLOOP, ENDIF etc., esp. if they are redundant, only repeating the method name, loop variable etc.

Pseudo-comments are always kept.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Don't add method signature and end-of comments](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-add-method-signature-and-end-of-comments)

## Options

* Action for end-of comments inside methods: \[remove if redundant\]
* Action for end-of comments outside of methods: \[remove if redundant\]
* Keep comment in method if opening command is > \[50\] lines away

## Examples


```ABAP

CLASS lcl_any_class DEFINITION.
  PUBLIC SECTION.
    DATA mv_any_attribute TYPE i.
ENDCLASS.                    " LCL_ANY_CLASS definition


CLASS lcl_any_class IMPLEMENTATION.
  METHOD remove_end_of_comments.
    " most end-of comments in this method are redundant, meaning that they only
    " repeat the beginning of the corresponding METHOD, LOOP, IF etc. statement
    LOOP AT its_item INTO DATA(ls_item).
      AT NEW group.
        init( its_item-group ).
      ENDAT. " new group

      IF ls_item-inner_item IS NOT INITIAL.
        LOOP AT ls_item-inner_item TRANSPORTING NO FIELDS.
          " some useful comment
        ENDLOOP. " at ls_item-inner_item
      ENDIF. " ls_item-inner_item not initial.

      AT END OF group.
        finalize( its_item-group ).
      ENDAT. " some non-redundant comment
    ENDLOOP. " at item
  ENDMETHOD.                    " REMOVE_END_OF_COMMENTS

  METHOD other_method.
    DO iv_num_processes TIMES.
      CASE sy-index.
        WHEN 1.
          initialize( ).
          process( ).
        WHEN OTHERS.
          process( ).
      ENDCASE. " sy-index
    ENDDO. " num processes times
  ENDMETHOD. " remove_end_of_comments [copy error!]
ENDCLASS. " lcl_any_class IMPLEMENTATION
```

Resulting code:

```ABAP

CLASS lcl_any_class DEFINITION.
  PUBLIC SECTION.
    DATA mv_any_attribute TYPE i.
ENDCLASS.


CLASS lcl_any_class IMPLEMENTATION.
  METHOD remove_end_of_comments.
    " most end-of comments in this method are redundant, meaning that they only
    " repeat the beginning of the corresponding METHOD, LOOP, IF etc. statement
    LOOP AT its_item INTO DATA(ls_item).
      AT NEW group.
        init( its_item-group ).
      ENDAT.

      IF ls_item-inner_item IS NOT INITIAL.
        LOOP AT ls_item-inner_item TRANSPORTING NO FIELDS.
          " some useful comment
        ENDLOOP.
      ENDIF.

      AT END OF group.
        finalize( its_item-group ).
      ENDAT. " some non-redundant comment
    ENDLOOP.
  ENDMETHOD.

  METHOD other_method.
    DO iv_num_processes TIMES.
      CASE sy-index.
        WHEN 1.
          initialize( ).
          process( ).
        WHEN OTHERS.
          process( ).
      ENDCASE.
    ENDDO.
  ENDMETHOD. " remove_end_of_comments [copy error!]
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/EndOfCommentRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/EndOfCommentTest.java)

