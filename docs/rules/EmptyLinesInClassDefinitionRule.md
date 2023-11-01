[<-- previous rule](EmptyLinesOutsideMethodsRule.md) | [overview](../rules.md) | [next rule -->](SpaceAroundTextLiteralRule.md)

# Standardize empty lines in class definitions

Restricts the number of consecutive empty lines in class definition sections, adds an empty line above each SECTION and between different types of definitions.

## References

* [Clean ABAP Styleguide: Add a single blank line to separate things, but not more](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#add-a-single-blank-line-to-separate-things-but-not-more)
* [Clean ABAP Styleguide: Don't obsess with separating blank lines](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-obsess-with-separating-blank-lines)

## Options

* \[X\] Add empty line above PUBLIC / PROTECTED / PRIVATE SECTION
* \[X\] Remove empty line below PUBLIC / PROTECTED / PRIVATE SECTION
* \[ \] Remove empty line above ENDCLASS
* \[X\] Remove notorious 'do not include other source files here\!\!\!' comments
* Max. empty lines within definition sections: \[1\] 
* Add empty line between CONSTANTS, DATA, METHODS etc.: \[add line, even between CLASS-DATA and DATA etc.\]

## Examples


```ABAP

CLASS cl_any_class DEFINITION
  PUBLIC
  CREATE PROTECTED.


  PUBLIC SECTION.

    INTERFACES if_any_class.
    INTERFACES if_other.
*"* public components of class CL_ANY_CLASS
*"* do not include other source files here!!!

    ALIASES any_methody
      FOR if_any_class~any_method .
    ALIASES other_method
      FOR if_any_class~other_method .
  PROTECTED SECTION.

*"* protected components of class CL_ANY_CLASS
*"* do not include other source files here!!!
    CLASS-DATA mv_any_static_text TYPE string.
    DATA mo_any   TYPE REF TO if_any_class.
    DATA ms_other TYPE ty_s_other.
    DATA mv_third TYPE i.



    METHODS any_protected_method.
    METHODS other_protected_method.
  PRIVATE SECTION.


    TYPES:
      ty_any   TYPE char10,
      ty_other TYPE char20.
    CONSTANTS gc_any_private_const   TYPE i VALUE 1.
    CONSTANTS gc_other_private_const TYPE i VALUE 2.
    " comment on static methods
    CLASS-METHODS: any_static_private_method,
      other_static_private_method.
    METHODS any_instance_private_method.

ENDCLASS.
```

Resulting code:

```ABAP

CLASS cl_any_class DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES if_any_class.
    INTERFACES if_other.

    ALIASES any_methody
      FOR if_any_class~any_method .
    ALIASES other_method
      FOR if_any_class~other_method .

  PROTECTED SECTION.
    CLASS-DATA mv_any_static_text TYPE string.

    DATA mo_any   TYPE REF TO if_any_class.
    DATA ms_other TYPE ty_s_other.
    DATA mv_third TYPE i.

    METHODS any_protected_method.
    METHODS other_protected_method.

  PRIVATE SECTION.
    TYPES:
      ty_any   TYPE char10,
      ty_other TYPE char20.

    CONSTANTS gc_any_private_const   TYPE i VALUE 1.
    CONSTANTS gc_other_private_const TYPE i VALUE 2.

    " comment on static methods
    CLASS-METHODS: any_static_private_method,
      other_static_private_method.

    METHODS any_instance_private_method.

ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/emptylines/EmptyLinesInClassDefinitionRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/emptylines/EmptyLinesInClassDefinitionTest.java)

