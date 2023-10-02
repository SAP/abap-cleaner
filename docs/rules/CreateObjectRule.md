[<-- previous rule](CallMethodRule.md) | [overview](../rules.md) | [next rule -->](RaiseTypeRule.md)

# Replace CREATE OBJECT with NEW constructor

Transforms CREATE OBJECT statements into functional style using NEW.

This rule requires a NetWeaver version >= 7.40.

## References

* [Clean ABAP Styleguide: Prefer NEW to CREATE OBJECT](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-new-to-create-object)

## Options

* \(no options available for this rule\)

## Examples


```ABAP

  METHOD replace_create_object.
    CREATE OBJECT lx_message.

    CREATE OBJECT lo_instance TYPE cl_any_class.

    CREATE OBJECT mo_instance TYPE cl_other_class
      EXPORTING
        io_contract    = me
        io_msg_handler = mo_msg_handler.

    " dynamic typing is not possible with NEW
    CREATE OBJECT mo_instance TYPE (lv_class_name).

    " EXCEPTIONS are not possible with NEW
    CREATE OBJECT mo_instance TYPE cl_other_class
      EXPORTING
        io_msg_handler = mo_msg_handler
      EXCEPTIONS
        cx_message     = 1
        others         = 2.
  ENDMETHOD.
```

Resulting code:

```ABAP

  METHOD replace_create_object.
    lx_message = NEW #( ).

    lo_instance = NEW cl_any_class( ).

    mo_instance = NEW cl_other_class(
        io_contract    = me
        io_msg_handler = mo_msg_handler ).

    " dynamic typing is not possible with NEW
    CREATE OBJECT mo_instance TYPE (lv_class_name).

    " EXCEPTIONS are not possible with NEW
    CREATE OBJECT mo_instance TYPE cl_other_class
      EXPORTING
        io_msg_handler = mo_msg_handler
      EXCEPTIONS
        cx_message     = 1
        others         = 2.
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/CreateObjectRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/CreateObjectTest.java)

