[<-- previous rule](FinalVariableRule.md) | [overview](../rules.md) | [next rule -->](EscapeCharForParametersRule.md)

# Standardize CLASS ... DEFINITION

Rearranges CLASS ... DEFINITION options into the order given in the ABAP Keyword Documentation and standardizes their layout.

## References

* [ABAP Keyword Documentation: CLASS, class\_options](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapclass_options.htm)

## Options

* Maximum line length \[120\] 
* Indentation: \[2 spaces\]
* One-/Two-liners: \[keep existing\]
* For multi-liners, start new line for:
* \[X\] PUBLIC
* \[X\] INHERITING FROM ...
* \[ \] ABSTRACT, FINAL
* \[X\] CREATE...
* \[X\] SHARED MEMORY ENABLED
* \[X\] FOR TESTING ...
* \[ \] RISK LEVEL ..., DURATION ...
* \[X\] FOR BEHAVIOR OF ..., FOR EVENTS OF ...
* \[X\] \[GLOBAL\] FRIENDS ...
* \[ \] LOCAL FRIENDS ...
* \[X\] Further class names of FRIENDS

## Examples


```ABAP

" here are some frequent CLASS ... DEFINITION patterns, including common displacement of options:
CLASS cl_any_class DEFINITION
  INHERITING FROM cl_any_parent
  PUBLIC CREATE PUBLIC.
ENDCLASS.


CLASS cl_other_class DEFINITION PUBLIC FINAL CREATE PUBLIC. "#EC INTF_IN_CLASS
ENDCLASS.


CLASS cl_any_test_class DEFINITION
  FINAL INHERITING FROM cl_any_test_base
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.
ENDCLASS.


CLASS cl_other_test_class DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.
ENDCLASS.


CLASS cl_any_class_with_friend DEFINITION
PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS cl_any_friend.
ENDCLASS.


CLASS cl_other_class_with_friend DEFINITION PUBLIC
  INHERITING FROM   cl_other_parent CREATE PRIVATE
  GLOBAL FRIENDS cl_any_friend   cl_other_friend
   cl_third_friend.
ENDCLASS.


CLASS cl_any_rap_behavior_def_class DEFINITION ABSTRACT FINAL PUBLIC
FOR BEHAVIOR OF bdef.
ENDCLASS.


CLASS cl_any_rap_event_handler_class DEFINITION
                                     PUBLIC ABSTRACT FINAL FOR EVENTS OF  bdef.
ENDCLASS.


" for multi-line definitions, an empty line is added above PUBLIC SECTION
CLASS cl_any_shared_memory_enabled DEFINITION
  PUBLIC FINAL CREATE   PUBLIC SHARED
  MEMORY ENABLED.
  PUBLIC SECTION.
ENDCLASS.


" definition of local friends is a different statement, but can be formatted similarly:
CLASS cl_any_local_class DEFINITION DEFERRED.
CLASS cl_other_local_class DEFINITION DEFERRED.
CLASS cl_public_class DEFINITION LOCAL FRIENDS 
    cl_any_local_class
    cl_other_local_class.
```

Resulting code:

```ABAP

" here are some frequent CLASS ... DEFINITION patterns, including common displacement of options:
CLASS cl_any_class DEFINITION
  PUBLIC
  INHERITING FROM cl_any_parent
  CREATE PUBLIC.
ENDCLASS.


CLASS cl_other_class DEFINITION PUBLIC FINAL CREATE PUBLIC. "#EC INTF_IN_CLASS
ENDCLASS.


CLASS cl_any_test_class DEFINITION
  INHERITING FROM cl_any_test_base FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
ENDCLASS.


CLASS cl_other_test_class DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
ENDCLASS.


CLASS cl_any_class_with_friend DEFINITION
  PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS cl_any_friend.
ENDCLASS.


CLASS cl_other_class_with_friend DEFINITION
  PUBLIC
  INHERITING FROM cl_other_parent
  CREATE PRIVATE
  GLOBAL FRIENDS cl_any_friend
                 cl_other_friend
                 cl_third_friend.
ENDCLASS.


CLASS cl_any_rap_behavior_def_class DEFINITION
  PUBLIC ABSTRACT FINAL
  FOR BEHAVIOR OF bdef.
ENDCLASS.


CLASS cl_any_rap_event_handler_class DEFINITION
  PUBLIC ABSTRACT FINAL FOR EVENTS OF bdef.
ENDCLASS.


" for multi-line definitions, an empty line is added above PUBLIC SECTION
CLASS cl_any_shared_memory_enabled DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC
  SHARED MEMORY ENABLED.

  PUBLIC SECTION.
ENDCLASS.


" definition of local friends is a different statement, but can be formatted similarly:
CLASS cl_any_local_class DEFINITION DEFERRED.
CLASS cl_other_local_class DEFINITION DEFERRED.
CLASS cl_public_class DEFINITION LOCAL FRIENDS cl_any_local_class
                                               cl_other_local_class.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/ClassDefinitionRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/ClassDefinitionTest.java)

