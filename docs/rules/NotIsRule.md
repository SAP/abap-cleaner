[<-- previous rule](ComparisonOperatorRule.md) | [overview](../rules.md) | [next rule -->](LogicalOperatorPositionRule.md)

# Prefer IS NOT to NOT IS

Transforms logical expressions to use IS NOT instead of NOT IS, if possible.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Prefer IS NOT to NOT IS](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-is-not-to-not-is)
* [Clean Code Checks: Prefer IS NOT to NOT IS](https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/prefer-is-not-to-not-is.md)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD prefer_is_not_to_not_is.
    " simple cases
    IF NOT iv_param IS SUPPLIED.
      IF NOT <ls_data> IS ASSIGNED.
        IF NOT lo_object IS BOUND.
          IF NOT lo_object IS INSTANCE OF cl_any_class.
            IF NOT lts_table[ a = 1
                              b = 2
                              c = 3 ]-field IS INITIAL.
              " do nothing
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    " complex cases
    IF NOT iv_param IS SUPPLIED
    OR NOT iv_other_param IS SUPPLIED.

      IF NOT <ls_data> IS ASSIGNED
      OR ( NOT lo_object IS BOUND
           AND NOT lo_object IS INSTANCE OF cl_any_class ).
        " do nothing
      ENDIF.

    ENDIF.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD prefer_is_not_to_not_is.
    " simple cases
    IF iv_param IS NOT SUPPLIED.
      IF <ls_data> IS NOT ASSIGNED.
        IF lo_object IS NOT BOUND.
          IF lo_object IS NOT INSTANCE OF cl_any_class.
            IF lts_table[ a = 1
                              b = 2
                              c = 3 ]-field IS NOT INITIAL.
              " do nothing
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    " complex cases
    IF    iv_param       IS NOT SUPPLIED
       OR iv_other_param IS NOT SUPPLIED.

      IF    <ls_data> IS NOT ASSIGNED
         OR (     lo_object IS NOT BOUND
              AND lo_object IS NOT INSTANCE OF cl_any_class ).
        " do nothing
      ENDIF.

    ENDIF.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/NotIsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/NotIsTest.java)

