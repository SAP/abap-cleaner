[<-- previous rule](AlignMethodsForTestingRule.md) | [overview](../rules.md) | [next rule -->](AlignAliasesForRule.md)

# Align METHODS ... REDEFINITION

Aligns consecutive METHODS ... \[FINAL\] REDEFINITION declarations.

Activate this rule if you want to override the result of 'Align METHODS declarations' with special settings.

## Options

* Continue line after METHODS \[always\]
* Continue line after method name \[always\]
* \[X\] Align across empty lines
* \[X\] Align across comment lines

## Examples


```ABAP

  " comment on first group of redefinitions
  METHODS if_any_interface~medium_method_name
    REDEFINITION.
  METHODS if_any_interface~short_name
    REDEFINITION.
  METHODS if_any_interface~extra_long_method_name
    REDEFINITION.

  " comment on second group of redefinitions
  METHODS if_other_interface~any_method_name
    FINAL REDEFINITION.
  METHODS if_other_interface~other_method_name
    FINAL REDEFINITION.

  METHODS
    if_other_interface~create REDEFINITION.

  " chains are aligned independently:
  METHODS:
    if_yet_another_interface~any_method_name
      REDEFINITION,
    if_yet_another_interface~other_method_name
      REDEFINITION,
    if_yet_another_interface~create
      REDEFINITION.

```

Resulting code:

```ABAP

  " comment on first group of redefinitions
  METHODS if_any_interface~medium_method_name     REDEFINITION.
  METHODS if_any_interface~short_name             REDEFINITION.
  METHODS if_any_interface~extra_long_method_name REDEFINITION.

  " comment on second group of redefinitions
  METHODS if_other_interface~any_method_name      FINAL REDEFINITION.
  METHODS if_other_interface~other_method_name    FINAL REDEFINITION.

  METHODS if_other_interface~create               REDEFINITION.

  " chains are aligned independently:
  METHODS: if_yet_another_interface~any_method_name   REDEFINITION,
           if_yet_another_interface~other_method_name REDEFINITION,
           if_yet_another_interface~create            REDEFINITION.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignMethodsRedefinitionRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignMethodsRedefinitionTest.java)

