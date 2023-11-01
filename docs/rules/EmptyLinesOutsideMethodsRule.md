[<-- previous rule](EmptyLinesWithinMethodsRule.md) | [overview](../rules.md) | [next rule -->](EmptyLinesInClassDefinitionRule.md)

# Separate methods and classes with empty lines

Separates consecutive methods, classes and interfaces with empty lines.

This rule is part of the **essential** profile, as it is explicitly demanded by the [Clean ABAP Styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md).

## References

* [Clean ABAP Styleguide: Add a single blank line to separate things, but not more](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#add-a-single-blank-line-to-separate-things-but-not-more)
* [Clean ABAP Styleguide: Don't obsess with separating blank lines](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#dont-obsess-with-separating-blank-lines)

## Options

* Empty lines between classes or interfaces: \[2\] 
* Empty lines between methods: \[1\] 
* Empty lines before first / after last method: \[0\] 

## Examples


```ABAP

INTERFACE lif_empty_lines PUBLIC.
  " definition code
ENDINTERFACE.
CLASS lcl_empty_lines DEFINITION.
  " definition code
ENDCLASS.





CLASS lcl_empty_lines IMPLEMENTATION.


  METHOD empty_lines_between_methods_1.
    " code
  ENDMETHOD.
  METHOD empty_lines_between_methods_2.
    " more code
  ENDMETHOD.



  METHOD empty_lines_between_methods_3.
    " even more code
  ENDMETHOD.
ENDCLASS.
```

Resulting code:

```ABAP

INTERFACE lif_empty_lines PUBLIC.
  " definition code
ENDINTERFACE.


CLASS lcl_empty_lines DEFINITION.
  " definition code
ENDCLASS.


CLASS lcl_empty_lines IMPLEMENTATION.
  METHOD empty_lines_between_methods_1.
    " code
  ENDMETHOD.

  METHOD empty_lines_between_methods_2.
    " more code
  ENDMETHOD.

  METHOD empty_lines_between_methods_3.
    " even more code
  ENDMETHOD.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/emptylines/EmptyLinesOutsideMethodsRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/emptylines/EmptyLinesOutsideMethodsTest.java)

