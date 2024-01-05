[<-- previous rule](CallMethodRule.md) | [overview](../rules.md) | [next rule -->](RaiseTypeRule.md)

# Replace CREATE OBJECT with NEW constructor

Transforms CREATE OBJECT statements into functional style using NEW.

This rule requires a NetWeaver version >= 7.40.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Prefer NEW to CREATE OBJECT](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#prefer-new-to-create-object)

## Options

* \[X\] Unchain CREATE OBJECT: chains \(required for processing them with this rule\)

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

    " chains can only be processed if they are first unchained
    CREATE OBJECT: lx_message, lx_other_message.

    CREATE OBJECT: lo_any TYPE cl_any_class,
                   lo_other TYPE (lv_class_name),
                   lo_third TYPE cl_othird_class
                     EXPORTING io_contract = me.
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

    " chains can only be processed if they are first unchained
    lx_message = NEW #( ).
    lx_other_message = NEW #( ).

    lo_any = NEW cl_any_class( ).
    CREATE OBJECT lo_other TYPE (lv_class_name).
    lo_third = NEW cl_othird_class(
                      io_contract = me ).
  ENDMETHOD.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/commands/CreateObjectRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/commands/CreateObjectTest.java)

