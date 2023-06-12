[<-- previous rule](AlignMethodsDeclarationRule.md) | [overview](../rules.md) | [next rule -->](AlignMethodsRedefinitionRule.md)

# Align METHODS ... FOR TESTING

Aligns consecutive METHODS ... FOR TESTING declarations.

Activate this rule if you want to override the result of 'Align METHODS declarations' with special settings.

## Options

* Continue line after METHODS \[always\]
* Continue line after method name \[always\]
* \[X\] Align across empty lines
* \[X\] Align across comment lines

## Examples


```ABAP

  " comment on first group of test methods
  METHODS test_setup
    FOR TESTING.
  METHODS test_functionality
    FOR TESTING.
  METHODS test_display
    FOR TESTING.

  " comment on second group of test methods
  METHODS any_test_method_name
    FOR TESTING RAISING cx_static_check.
  METHODS other_test_method_name
    FOR TESTING RAISING cx_static_check.

  METHODS test_invalid_input
    FOR TESTING.

  " chains are aligned independently:
  METHODS:
    first_test_method
      FOR TESTING,
    second_test_method
      FOR TESTING RAISING cx_message,
    last_test_method
      FOR TESTING.

```

Resulting code:

```ABAP

  " comment on first group of test methods
  METHODS test_setup             FOR TESTING.
  METHODS test_functionality     FOR TESTING.
  METHODS test_display           FOR TESTING.

  " comment on second group of test methods
  METHODS any_test_method_name   FOR TESTING RAISING cx_static_check.
  METHODS other_test_method_name FOR TESTING RAISING cx_static_check.

  METHODS test_invalid_input     FOR TESTING.

  " chains are aligned independently:
  METHODS: first_test_method  FOR TESTING,
           second_test_method FOR TESTING RAISING cx_message,
           last_test_method   FOR TESTING.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/alignment/AlignMethodsForTestingRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/alignment/AlignMethodsForTestingTest.java)

