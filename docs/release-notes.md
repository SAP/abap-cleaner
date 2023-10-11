# Release Notes

These release notes list all enhancements of ABAP cleaner (added rules, configuration, menus etc.) 
as well as bugfixes of reported issues, i.e. anything that enhances or changes the behavior of ABAP cleaner. 

For a complete list of changes (including documentation, tests, refactoring etc.), please refer to 
the list of [commits](../../../commits/main).

## 2023-10-10 (version 1.7.1)

Many **thanks** to [**Lightirius**](https://github.com/Lightirius) for reporting this bug so quickly!

* Fixed missing custom profiles ([#138](../../../issues/138))

## 2023-10-10 (version 1.7.0)

**Thank you** very much [**blackfish5**](https://github.com/blackfish5), [**bastianStr**](https://github.com/bastianStr), 
[**Kronrir**](https://github.com/Kronrir), [**JoachimRees**](https://github.com/JoachimRees), [**stockbal**](https://github.com/stockbal) 
and [**ConjuringCoffee**](https://github.com/ConjuringCoffee) for your contributions, ideas and bug reports that led to these improvements! 

* Added support for additional, synchronized, **read-only [team profile folders](profiles.md#sharing-and-synchronizing-profiles-with-colleagues)** ([#28](../../../issues/28))
* **Profiles editor**: Added context menu and shortcut to **copy rule name and description** to the clipboard ([#130](../../../issues/130))
* Enhanced rule '**Convert CHECK outside loop ...**' with option to '**Allow CHECK after ASSERT ...**' ([#135](../../../issues/135))
* Enhanced rule '**Standardize empty lines within methods**' to allow setting **0 empty lines** within methods ([#131](../../../issues/131))
* Added **documentation** on [how to **synchronize team profiles**](profiles.md#sharing-and-synchronizing-profiles-with-colleagues) ([#28](../../../issues/28))
* Added **documentation** for [**GitHub Actions workflow usage**](usage.md#github-actions-workflow-usage) ([#127](../../../issues/127))
* Added **info on code signing** in [installation instructions](../README.md#requirements-and-installation) ([#34](../../../issues/34))
* Added link to **blog post** on [**ABAP Tools for Clean ABAP**](https://blogs.sap.com/2023/10/09/abap-tools-for-clean-abap/)
* Fixed rule '**Align declarations**' for table types with **complex key definitions** ([#129](../../../issues/129))
* Fixed **Parser** for **SELECT** with **aggregate functions** inside parentheses ([#134](../../../issues/134))
* Fixed **Parser** to accept **chain colon inside of brackets** for chains of one ([#133](../../../issues/133))

## 2023-10-02 (version 1.6.0)

**Great thanks** to [**stockbal**](https://github.com/stockbal) for the first Open-Source code contribution to ABAP cleaner!, 
as well as [**ZEAL-IT**](https://github.com/ZEAL-IT), [**oscardelama**](https://github.com/oscardelama), 
[**b4loghpeter**](https://github.com/b4loghpeter), [**DirkBor**](https://github.com/DirkBor), 
[**m-badura**](https://github.com/m-badura) and [**vonglan**](https://github.com/vonglan) 
for their ideas and bug reports!

* Added new **command line options** for cleaning **multiple files** from one ```--sourcedir``` ([#118](../../../issues/118))
* Added menu item to '**Show Read-Only Preview** With ABAP Cleaner...' without locking ([#114](../../../issues/114))
* Added **documentation** on why ABAP cleaner has no **pragmas and pseudo-comments** ([#121](../../../issues/121))
* Added info on **ABAP release** of current system in **window title** of interactive UI
* Fixed **command line** cleanup adding **LF at end** of file ([#124](../../../issues/124))
* Fixed rule '**Replace obsolete MOVE ... TO with =**' for **chains** of one and late chains ([#120](../../../issues/120))
* Fixed rule '**Replace CREATE OBJECT with NEW constructor**' to require ABAP release **7.40** ([#119](../../../issues/119))
* Fixed rule '**Use FINAL for immutable variables**' for MODIFY ENTITY with **fields tables** ([#117](../../../issues/117))
* Fixed **parser** for escape char ! with composed identifiers ([#115](../../../issues/115))

## 2023-09-26 (version 1.5.5)

**Thanks a lot**, [**bnichell**](https://github.com/bnichell) for reporting the bug behind this release!

* Added link to the recording of the **Devtoberfest session** in **README** chapter 'Demo'
* Fixed rule '**Use FINAL for immutable variables**' for assignments to table expressions ([#112](../../../issues/112))

## 2023-09-22 (version 1.5.4)

**Thank you** [**MDagni**](https://github.com/MDagni), [**suynwa**](https://github.com/suynwa), 
[**AlexMFrank**](https://github.com/AlexMFrank), [**FirdousP**](https://github.com/FirdousP)
and [**ConjuringCoffee**](https://github.com/ConjuringCoffee) for the issues behind these improvements!

* Fixed rule '**Standardize empty lines within methods**' for function modules ([#106](../../../issues/106))
* Changed rule '**Add missing parameters to ABAP Doc**' to not create ABAP Doc lines for **test method exceptions** ([#103](../../../issues/103))
* Updated **ABAP grammar file** to fix capitalization of CORRESPONDING ... MAPPING ... DEFAULT ([#102](../../../issues/102))
* Added hint on restriction of rule '**Align declarations**', option '**Maximum line length**' ([#100](../../../issues/100))
* Improved **parser error** message for **chain colon inside parentheses** ([#91](../../../issues/91))
* Improved **documentation** for cleanup range '**Current class**' ([#62](../../../issues/62))

## 2023-09-04 (version 1.5.3)

Many **thanks** to [**suynwa**](https://github.com/suynwa), [**ConjuringCoffee**](https://github.com/ConjuringCoffee), [**jan-jezek**](https://github.com/jan-jezek), 
[**AlexMFrank**](https://github.com/AlexMFrank) and [**cgrail**](https://github.com/cgrail) for the reporting the bugs behind these fixes!

* Fixed rule '**Indent lines**' for WITH ... ENDWITH loops ([#98](../../../issues/98))
* Fixed rule '**Use FINAL for immutable variables**' for method calls inside of constructors expressions ([#96](../../../issues/96))
* Fixed rule '**Convert upper and lower case**' for text symbol IDs with letters ([#95](../../../issues/95))
* Fixed **parser** for CLASS inside of macros ([#93](../../../issues/93))
* Fixed rule '**Convert upper and lower case**', option '**Auto-determine ...: derive from first ...**' for implementation sections ([#92](../../../issues/92))

## 2023-08-15 (version 1.5.2)

**Thanks** a lot to [**openPhiL**](https://github.com/openPhiL), [**thebestabapdeveloper**](https://github.com/thebestabapdeveloper) 
and [**ConjuringCoffee**](https://github.com/ConjuringCoffee) for the bug reports behind these fixes!

* Fixed rule '**Make implicit type explicit**' for obsolete DATA ... OCCURS ([#89](../../../issues/89))
* Fixed rule '**Move AND/OR etc. from line end to next line start**' for comments at line end ([#88](../../../issues/88))
* Fixed rule '**Delete unused variables**' to skip methods with test seams or test injection ([#87](../../../issues/87))
* Changed default configuration of rule '**Standardize spaces next to parentheses**', option '**Add space in condensed cases ...**' (now active by default)

## 2023-07-31 (version 1.5.1)

Great **thanks** to [**alexlukas**](https://github.com/alexlukas) and [**ConjuringCoffee**](https://github.com/ConjuringCoffee) 
for their ideas and bug reports!

* Enhanced online **documentation** with **screenshots**
* Enhanced rule '**Align declarations**' with option for '**Maximum line length**' ([#77](../../../issues/77))
* Enhanced rule '**Align parameters and components**' with documentation for option '**Maximum line length B**' ([#81](../../../issues/81))
* Fixed rule '**Align parameters and components**' for **table expressions** chained with ```][``` ([#75](../../../issues/75))
* Fixed parser for **```ULINE AT /pos(len)```** ([#80](../../../issues/80))

## 2023-07-05 (version 1.5.0)

Many **thanks** to [**se38**](https://github.com/se38), [**ConjuringCoffee**](https://github.com/ConjuringCoffee)
and [**richardbruenning**](https://github.com/richardbruenning) for the ideas and bug reports behind these improvements!

* Added new rule '**Remove end-of comments**' ([#60](../../../issues/60))
* Fixed rules '**Convert CHECK in/outside loop**' for SELECT...ENDSELECT ([#71](../../../issues/71))
* Fixed rule '**Align parameters and components**' for row with structure variable ([#70](../../../issues/70))
* Added "not reachable" to known installation issues ([#69](../../../issues/69))

## 2023-06-26 (version 1.4.1)

**Thank you** [**jelliottp**](https://github.com/jelliottp), [**ConjuringCoffee**](https://github.com/ConjuringCoffee)
and [**zmsMarc**](https://github.com/zmsMarc) for opening the issues behind these improvements!

* Enhanced rule '**Align parameters and components**' for one-liners behind the call ([#66](../../../issues/66))
* Fixed rule '**Align parameters and components**' to keep pseudo comments at line end ([#65](../../../issues/65))
* Fixed rule '**Rearrange local declarations**' for LIKE across different declaration keywords ([#64](../../../issues/64))
* Fixed rule '**Delete unused variables**' to remove TODO comments from line end ([#58](../../../issues/58))

## 2023-06-12 (version 1.4.0)

Great **thanks** to [**ConjuringCoffee**](https://github.com/ConjuringCoffee), [**fabianlupa**](https://github.com/fabianlupa), 
[**jelliottp**](https://github.com/jelliottp) and [**bjoern-jueliger-sap**](https://github.com/bjoern-jueliger-sap) for all your ideas!

* Main window: Added option for '**Default cleanup range**' to enable cleanup of **entire code document** ([#42](../../../issues/42))
* Enhanced rule '**Align declarations**' with options '**Action for chains**' etc. and '**Condense inner spaces**' ([#35](../../../issues/35))
* Enhanced rule '**Align parameters and components**' with option to '**Align assignments across rows ...**' ([#54](../../../issues/54))
* Enhanced rule '**Align parameters and components**' for **RAISE ... MESSAGE ... EXPORTING** ([#16](../../../issues/16))
* Enhanced rule '**Align logical expressions**' for alignment of **FOR ... IN ... WHERE** ( ... ) ([#29](../../../issues/29))
* Enhanced rule '**Move AND/OR etc. from line end to next line start**' with option to '**Move keywords**' ([#29](../../../issues/29))
* Enhanced rule '**Remove the self-reference me-&gt;**' to consider **local interfaces** and aliases ([#41](../../../issues/41))
* Enhanced rule '**Use assert class instead of ASSERT**' with descriptions and **example implementation**
* Fixed rule '**Align parameters and components**' for line starts **left of the assignment** operator in **nested cases** ([#53](../../../issues/53))
* Fixed rule '**Delete unused variables**' options wording, replacing 'measure' with '**action**' ([#52](../../../issues/52))
* Fixed **background color** of options in Profiles editor for **dark theme** ([#47](../../../issues/47))


## 2023-06-06 (version 1.3.0)

Many **thanks** to [**jelliottp**](https://github.com/jelliottp), [**ConjuringCoffee**](https://github.com/ConjuringCoffee), [**GPR8**](https://github.com/GPR8), 
[**JanisBur**](https://github.com/JanisBur) and [**thebestabapdeveloper**](https://github.com/thebestabapdeveloper) for inspiring these improvements and fixes!

* Added new rule '**Add missing parameters to ABAP Doc**' ([#24](../../../issues/24))
* Enhanced rule '**Standardize spaces next to parentheses**' with option to '**Add space between parentheses and character literals**' ([#20](../../../issues/20))
* Enhanced rule '**Standardize escaping of !parameters**' with option '**only to avoid syntax errors**' ([#31](../../../issues/31))
* Enhanced rule '**Replace obsolete MOVE ... TO with =**' with option to '**Process MOVE: chains**' ([#32](../../../issues/32))
* Enhanced rule '**Delete unused variables**' with example for **##NEEDED** pragma ([#38](../../../issues/38))
* Profiles editor: In **rule descriptions**, moved of **conditions and restrictions** to second line
* Profiles editor: Enhanced 'Save Profiles and Exit' to **overwrite changed profiles only** ([#28](../../../issues/28))
* Profiles editor: Fixed partly hidden buttons in **'Profile name:' input box** ([#40](../../../issues/40))
* Fixed rule '**Delete unused variables**' and '**Use FINAL for immutable variables**' for macro usage ([#43](../../../issues/43))
* Documentation: Added solution to **installation issue 'No updates found'** ([#39](../../../issues/39))

## 2023-05-25 (version 1.2.1)

Many **thanks** to [**ConjuringCoffee**](https://github.com/ConjuringCoffee) and [**SmogulT**](https://github.com/SmogulT) for opening the issues that led to these improvements!

* Enabled (de)activation of option '**Highlight text and line changes**' on main window ([#25](../../../issues/25))
* Fixed number of lines shown on option '**Highlight inner space changes**' on main window ([#25](../../../issues/25))
* Fixed rule '**Align keywords with second word of first line**' for CALL TRANSFORMATION ([#22](../../../issues/22))
* Fixed cleanup for systems with **unknown ABAP release** ([#21](../../../issues/21))

## 2023-05-22 (version 1.2.0)

Many **thanks** to [**fabianlupa**](https://github.com/fabianlupa) for opening the issues that led to these enhancements and fixes!

* Enhanced rule '**Align declarations**' with option on '**Alignment of nested structures**' ([#9](../../../issues/9), [#14](../../../issues/14))
* Enhanced rule '**Unchain into multiple statements**' with option to '**Unchain declarations in interfaces**' ([#12](../../../issues/12))
* Fixed rule '**Align METHODS declarations**' for mixed chains of one-liners and multi-liners ([#13](../../../issues/13))
* Fixed rule '**Delete unused variables**' for variable usage in INSERT ... FROM TABLE
* Fixed rule '**Use FINAL for immutable variables**' for changes with PERFORM ... USING
* Fixed rule '**Replace obsolete pseudo comments with pragmas**' for multiple consecutive pseudo comments

## 2023-05-07 (version 1.1.0)

* Added new rule '**Align CLEAR:, FREE: and SORT**'

## 2023-05-05 (version 1.0.2)

* Fixed rule '**Rearrange local declarations**' for LIKE referring to a variable that is declared inline
* Fixed rule '**Delete unused variables**' for declarations in macros

## 2023-04-28 (version 1.0.1)

* Fixed rule '**Align parameters and components**' for functional calls after INSERT LINES OF
* Fixed rule '**Align METHODS ... FOR TESTING**' for line breaks without subsequent space
* Fixed rule '**Align logical expressions**' for AND/OR in own line
* Fixed rule '**Delete unused variables**' for variable usage with LIKE TABLE OF etc.

## 2023-04-21 (version 1.0.0)

* This is the initial Open Source release, offering 57 cleanup rules with a total of 162 configuration options!

**Continue reading**: [Available cleanup rules](rules.md)
