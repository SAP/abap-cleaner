[<-- previous rule](ChainOfOneRule.md) | [overview](../rules.md) | [next rule -->](FinalVariableRule.md)

# Make implicit type explicit

Replaces obsolete short forms of declarations with explicit forms that specify the TYPE, LENGTH, and DECIMALS \(if applicable\).

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Avoid obsolete language elements](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#avoid-obsolete-language-elements)
* [ABAP Keyword Documentation: Obsolete Declarations: TYPES, implicit](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaptypes_implicit.htm)
* [ABAP Keyword Documentation: For legibility, use LENGTH instead of parentheses](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapdata_simple.htm)

## Options

* \[X\] Execute on TYPES statements
* \[X\] Execute on CONSTANTS and STATICS statements
* \[X\] Execute on DATA and CLASS-DATA statements
* \[X\] Replace parentheses with LENGTH

## Examples


```ABAP

CLASS cl_implicit_type DEFINITION.
  PUBLIC SECTION.
    " in the object-oriented context, TYPES only allows for two short forms with implicit TYPE,
    " while more short forms (with implicit LENGTH and DECIMALS) are tolerated with (CLASS-)DATA, CONSTANTS, STATICS:
    TYPES: ty_b(5),
           ty_c LENGTH 5.

    TYPES: BEGIN OF ty_s_implicit,
             b(5),
             c LENGTH 5,
           END OF ty_s_implicit.

    " for built-in types c, n, and x, the default length is 1
    CONSTANTS: gc_a VALUE 'a',
               gc_b(5) VALUE 'abcde',
               gc_d TYPE c VALUE 'a',
               gc_e TYPE n VALUE '2',
               gc_f TYPE x VALUE ''.

    " the cleanup rule also ensures that each of the expanded declarations gets a line of its own
    CLASS-DATA:
      gv_a, gv_b(5),
      gv_d TYPE c, gv_e TYPE n, gv_f TYPE x,
      " for built-in type p (packed numbers), the default is LENGTH 8 DECIMALS 0
      gv_g TYPE p, gv_h(5) TYPE p, gv_i TYPE p LENGTH 3, gv_j TYPE p DECIMALS 4.

    CLASS-METHODS make_implicit_type_explicit.
ENDCLASS.

CLASS cl_implicit_type IMPLEMENTATION.
  METHOD make_implicit_type_explicit.
    STATICS:
      st_g TYPE p VALUE '123456789012345-',
      st_h(5) TYPE p VALUE '123456789-',
      st_i TYPE p LENGTH 3 VALUE '12345-',
      st_j TYPE p DECIMALS 4 VALUE '12345678901.2345-'.

    " DATA allows for all short forms except 'x LENGTH n':
    DATA lv_a.
    DATA lv_b(5).
    DATA lv_d TYPE c.
    DATA lv_e TYPE n.
    DATA lv_f TYPE x.

    DATA lv_g TYPE p.
    DATA lv_h(5) TYPE p.
    DATA lv_i TYPE p LENGTH 3.
    DATA lv_j TYPE p DECIMALS 4.
  ENDMETHOD.
ENDCLASS.
```

Resulting code:

```ABAP

CLASS cl_implicit_type DEFINITION.
  PUBLIC SECTION.
    " in the object-oriented context, TYPES only allows for two short forms with implicit TYPE,
    " while more short forms (with implicit LENGTH and DECIMALS) are tolerated with (CLASS-)DATA, CONSTANTS, STATICS:
    TYPES: ty_b TYPE c LENGTH 5,
           ty_c TYPE c LENGTH 5.

    TYPES: BEGIN OF ty_s_implicit,
             b TYPE c LENGTH 5,
             c TYPE c LENGTH 5,
           END OF ty_s_implicit.

    " for built-in types c, n, and x, the default length is 1
    CONSTANTS: gc_a TYPE c LENGTH 1 VALUE 'a',
               gc_b TYPE c LENGTH 5 VALUE 'abcde',
               gc_d TYPE c LENGTH 1 VALUE 'a',
               gc_e TYPE n LENGTH 1 VALUE '2',
               gc_f TYPE x LENGTH 1 VALUE ''.

    " the cleanup rule also ensures that each of the expanded declarations gets a line of its own
    CLASS-DATA:
      gv_a TYPE c LENGTH 1,
      gv_b TYPE c LENGTH 5,
      gv_d TYPE c LENGTH 1,
      gv_e TYPE n LENGTH 1,
      gv_f TYPE x LENGTH 1,
      " for built-in type p (packed numbers), the default is LENGTH 8 DECIMALS 0
      gv_g TYPE p LENGTH 8 DECIMALS 0,
      gv_h TYPE p LENGTH 5 DECIMALS 0,
      gv_i TYPE p LENGTH 3 DECIMALS 0,
      gv_j TYPE p LENGTH 8 DECIMALS 4.

    CLASS-METHODS make_implicit_type_explicit.
ENDCLASS.

CLASS cl_implicit_type IMPLEMENTATION.
  METHOD make_implicit_type_explicit.
    STATICS:
      st_g TYPE p LENGTH 8 DECIMALS 0 VALUE '123456789012345-',
      st_h TYPE p LENGTH 5 DECIMALS 0 VALUE '123456789-',
      st_i TYPE p LENGTH 3 DECIMALS 0 VALUE '12345-',
      st_j TYPE p LENGTH 8 DECIMALS 4 VALUE '12345678901.2345-'.

    " DATA allows for all short forms except 'x LENGTH n':
    DATA lv_a TYPE c LENGTH 1.
    DATA lv_b TYPE c LENGTH 5.
    DATA lv_d TYPE c LENGTH 1.
    DATA lv_e TYPE n LENGTH 1.
    DATA lv_f TYPE x LENGTH 1.

    DATA lv_g TYPE p LENGTH 8 DECIMALS 0.
    DATA lv_h TYPE p LENGTH 5 DECIMALS 0.
    DATA lv_i TYPE p LENGTH 3 DECIMALS 0.
    DATA lv_j TYPE p LENGTH 8 DECIMALS 4.
  ENDMETHOD.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/ImplicitTypeRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/ImplicitTypeTest.java)

