# Release Notes

These release notes list all enhancements of ABAP cleaner (added rules, configuration, menus etc.) 
as well as bugfixes of reported issues, i.e. anything that enhances or changes the behavior of ABAP cleaner. 

For a complete list of changes (including documentation, tests, refactoring etc.), please refer to 
the list of [commits](../../../commits/main).

## 2023-04-28 (version 1.0.1)

* Fixed rule '**Align parameters and components**' for functional calls after INSERT LINES OF
* Fixed rule '**Align METHODS ... FOR TESTING**' for line breaks without subsequent space
* Fixed rule '**Align logical expressions**' for AND/OR in own line
* Fixed rule '**Delete unused variables**' for variable usage with LIKE TABLE OF etc.

## 2023-04-21 (version 1.0.0)

* This is the initial Open Source release, offering 57 cleanup rules with a total of 162 configuration options!

**Continue reading**: [Available cleanup rules](rules.md)
