[<-- previous rule](CdsTestClassLinesRule.md) | [overview](../rules.md) | [next rule -->](SpaceAroundTextLiteralRule.md)

# Move commands to own lines

If a line contains multiple commands, each command is moved to its own line.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: No more than one statement per line](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#no-more-than-one-statement-per-line)

## Options

* \[X\] Keep one-liners after WHEN
* \[ \] Keep multi-liners after WHEN

## Examples


```ABAP

  METHOD any_method.
    DATA lv_x TYPE i VALUE 1. DATA lv_y TYPE i VALUE 2. DATA lv_z TYPE i VALUE 3.

    lv_x += 2.lv_y -= 4.lv_z *= 6.

    IF lv_x > lv_y. IF lv_y < lv_z.
      lv_x = lv_z - lv_y.ELSE.lv_y = lv_z - lv_x.ENDIF.
    ENDIF.

    DO 3 TIMES.lv_y *= 3.DO 5 TIMES.IF lv_x < lv_y + lv_z.lv_x += 2.lv_y -= 1.ELSE.lv_z *= 1.ENDIF.ENDDO.ENDDO.

    CASE lv_x.
      WHEN 1. lv_x += 1. lv_y += 1.
      WHEN 2. lv_y -= 1. lv_z -= 2.
      WHEN 3. lv_z *= 2. lv_x *= 3.
    ENDCASE.

    CASE lv_x.
      WHEN 1. lv_x += 1. do_something( iv_value = lv_x
                                       iv_name  = 'X' ).
      WHEN 2. lv_y -= 1. do_something( iv_value = lv_y
                                       iv_name  = 'Y' ).
      WHEN 3. lv_z *= 2. do_something( iv_value = lv_z ).
        do_something_else( iv_value = lv_z ).
    ENDCASE. RETURN lv_x + lv_y + lv_z.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD any_method.
    DATA lv_x TYPE i VALUE 1.
    DATA lv_y TYPE i VALUE 2.
    DATA lv_z TYPE i VALUE 3.

    lv_x += 2.
    lv_y -= 4.
    lv_z *= 6.

    IF lv_x > lv_y.
      IF lv_y < lv_z.
        lv_x = lv_z - lv_y.
      ELSE.
        lv_y = lv_z - lv_x.
      ENDIF.
    ENDIF.

    DO 3 TIMES.
      lv_y *= 3.
      DO 5 TIMES.
        IF lv_x < lv_y + lv_z.
          lv_x += 2.
          lv_y -= 1.
        ELSE.
          lv_z *= 1.
        ENDIF.
      ENDDO.
    ENDDO.

    CASE lv_x.
      WHEN 1. lv_x += 1. lv_y += 1.
      WHEN 2. lv_y -= 1. lv_z -= 2.
      WHEN 3. lv_z *= 2. lv_x *= 3.
    ENDCASE.

    CASE lv_x.
      WHEN 1.
        lv_x += 1.
        do_something( iv_value = lv_x
                      iv_name  = 'X' ).
      WHEN 2.
        lv_y -= 1.
        do_something( iv_value = lv_y
                      iv_name  = 'Y' ).
      WHEN 3.
        lv_z *= 2.
        do_something( iv_value = lv_z ).
        do_something_else( iv_value = lv_z ).
    ENDCASE.
    RETURN lv_x + lv_y + lv_z.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/emptylines/OneCommandPerLineRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/emptylines/OneCommandPerLineTest.java)

