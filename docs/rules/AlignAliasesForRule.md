[<-- previous rule](AlignMethodsRedefinitionRule.md) | [overview](../rules.md) | [next rule -->](AlignDeclarationsRule.md)

# Align ALIASES ... FOR ...

Aligns consecutive ALIASES ... FOR ... declarations.

## Options

* \[X\] Align across empty lines
* \[X\] Align across comment lines

## Examples


```ABAP

  " comment on first group of aliases
  ALIASES medium_method_name
    FOR if_any_interface~medium_method_name.
  ALIASES short_name
    FOR if_any_interface~short_name.
  ALIASES extra_long_method_name
    FOR if_any_interface~extra_long_method_name.

  " comment on first group of aliases
  ALIASES any_method_name
    FOR if_other_interface~any_method_name.
  ALIASES other_method_name
    FOR if_other_interface~other_method_name.

  ALIASES create
    FOR if_other_interface~create.

  " chains are aligned independently:
  ALIASES:
    first_method_name
      FOR if_yet_another_interface~first_method_name,
    second_method_name
      FOR if_yet_another_interface~second_method_name,
    last_method_name
      FOR if_yet_another_interface~last_method_name.

```

Resulting code:

```ABAP

  " comment on first group of aliases
  ALIASES medium_method_name     FOR if_any_interface~medium_method_name.
  ALIASES short_name             FOR if_any_interface~short_name.
  ALIASES extra_long_method_name FOR if_any_interface~extra_long_method_name.

  " comment on first group of aliases
  ALIASES any_method_name        FOR if_other_interface~any_method_name.
  ALIASES other_method_name      FOR if_other_interface~other_method_name.

  ALIASES create                 FOR if_other_interface~create.

  " chains are aligned independently:
  ALIASES: first_method_name  FOR if_yet_another_interface~first_method_name,
           second_method_name FOR if_yet_another_interface~second_method_name,
           last_method_name   FOR if_yet_another_interface~last_method_name.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignAliasesForRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignAliasesForTest.java)

