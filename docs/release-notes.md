# Release Notes

These release notes list all enhancements of ABAP cleaner (added rules, configuration, menus etc.) 
as well as bugfixes of reported issues, i.e. anything that enhances or changes the behavior of ABAP cleaner. 

For a complete list of changes (including documentation, tests, refactoring etc.), please refer to 
the list of [commits](../../../commits/main).

## 2024-09-18 (version 1.19.1)

**Thanks a lot**, [**MDagni**](https://github.com/MDagni) for opening the issue that led to the bugfix!

* **Enhanced** parser, object model and UI **for Data Definition Language (DDL)** as used in **CDS Views**
* Added new DDL rule '**Standardize annotation layout**'
* Added new DDL rule '**Rearrange annotations**'
* Added new DDL rule '**Break before DEFINE etc.**'
* Added new DDL rule '**Break before AS SELECT etc.**'
* Added new DDL rule '**Break before JOINs**'
* Added new DDL rule '**Break before ASSOCIATIONs**'
* Added new DDL rule '**Break before select list braces**'
* Added new DDL rule '**Break before WHERE clause etc.**'
* Added new DDL rule '**Standardize spaces around colon, comma etc.**'
* Added new DDL rule '**Standardize spaces around brackets**'
* Added new DDL rule '**Use CamelCase for known entity and field names**'
* Added new DDL rule '**Correct frequent typos in DDL comments**'
* Added new DDL rule '**Align view parameters**'
* Added new DDL rule '**Align source parameters**'
* Added new DDL rule '**Align function parameters after =>**'
* Added new DDL rule '**Align logical expressions in views**'
* Added new DDL rule '**Align name list and GROUP BY list**'
* Added new DDL rule '**Align JOINs and ASSOCIATIONs**'
* Added new DDL rule '**Align select list**'
* Added new DDL rule '**Standardize empty lines between sections**'
* Added new DDL rule '**Standardize empty lines within sections**'
* Profiles editor: Made **height of rule references** section **flexible**
* Fixed rule '**Make implicit type explicit**' for constant used as length in parentheses ([#350](../../../issues/350))
* Updated **known CamelCase names** for CDS views and fields

## 2024-07-15 (version 1.18.1)

Great **thanks** to [**suynwa**](https://github.com/suynwa) for the bug report behind this fix!

* Fixed **local blocking of rule** in Rules Used list **for manual selection of cleanup range** from ADT
* Updated images with ADT-style color profile

## 2024-07-08 (version 1.18.0)

Great **thanks** to [**vonglan**](https://github.com/vonglan), [**Ennowulff**](https://github.com/Ennowulff),
[**JoachimEck**](https://github.com/JoachimEck), [**apanys**](https://github.com/apanys), [**MichiFr**](https://github.com/MichiFr),
[**cawoodmonads**](https://github.com/cawoodmonads), [**ConjuringCoffee**](https://github.com/ConjuringCoffee) and 
[**openPhiL**](https://github.com/openPhiL) for inspiring these improvements and fixes!

* Added menu "View / **Use ADT-Style Colors**" to select between ADT-style and classic **color profile** for diff view ([#282](../../../issues/282))
* Added labels "**Before cleanup**" and "**After cleanup**" to diff view ([#249](../../../issues/249))
* Enhanced rule '**Align parameters and components**' for **group key** in LOOP AT ... GROUP BY ([#330](../../../issues/330))
* Enhanced rule '**Use string templates to assemble text**' with option for **lines without operands** ([#323](../../../issues/323))
* Enhanced '**Use assert class instead of ASSERT**' with **explanation** in examples ([#314](../../../issues/314))
* Fixed rule '**Replace TRANSLATE with string functions**' for unknown, potentially **structured types** ([#318](../../../issues/318))
* Fixed rule '**Align declarations**' for comment line and **line-start comma** ([#335](../../../issues/335))
* Fixed rule '**Delete unused variables**' for obsolete **REPLACE ... INTO** ([#331](../../../issues/331))
* Fixed rule '**Align SELECT ... FROM ... JOIN**' for **ON ... JOIN** ([#332](../../../issues/332))
* Updated **ABAP grammar file**
* Updated **known CamelCase names** for CDS views and fields

## 2024-06-24 (version 1.17.1)

**Thanks a lot**, [**agoeb**](https://github.com/agoeb) and [**matthewdjb**](https://github.com/matthewdjb) for the bug reports behind these improvements!

* Raised **minimum required** versions to **Java 17** and **ABAP Development Tools 3.38 / Eclipse 2023-06**
* Fixed rule '**Remove needless CLEAR**' for multiple **commands on same line** ([#315](../../../issues/315))
* Removed **duplicate resource files** from JAR file ([#325](../../../issues/325))
* Updated **known CamelCase names** for CDS views and fields

## 2024-06-03 (version 1.16.2)

Many **thanks** to [**hansdampfinger666**](https://github.com/hansdampfinger666) and [**fmabap**](https://github.com/fmabap) 
for your contributions and ideas that led to these improvements!

* Fixed rule '**Indent lines**' for **PrettyPrinter indentation of INCLUDE** ([#278](../../../issues/278))
* Fixed rule '**Use string templates to assemble text**' for concatenation **operators at line end** ([#306](../../../issues/306))
* Updated **known CamelCase names** for CDS views and fields

## 2024-05-13 (version 1.16.1)

**Thanks** a lot to [**xkollar2**](https://github.com/xkollar2), [**phenschke**](https://github.com/phenschke) and
[**mraht**](https://github.com/mraht) for the issues behind these improvements!

* Fixed **parser** for CLASS: ... **DEFINITION DEFERRED chains** ([#294](../../../issues/294))
* Fixed rule '**Use FINAL for immutable variables**' for **ASSIGN COMPONENT** ... OF STRUCTURE ([#297](../../../issues/297))
* Fixed rule '**Replace TRANSLATE with string functions**' etc. for **placeholders** in macro definitions ([#298](../../../issues/298))
* Fixed rule '**Align declarations**' for TYPES at the end of an **INCLUDE TYPE** line
* Fixed rule '**Align parameters and components**' for assignments after **BASE**
* Updated **known CamelCase names** for CDS views and fields

## 2024-04-12 (version 1.16.0)

Many **thanks** to [**HuprichT**](https://github.com/HuprichT), [**mraht**](https://github.com/mraht) and 
[**ConjuringCoffee**](https://github.com/ConjuringCoffee) for all your ideas!

* Changed **Cleanup Settings** (last profile, cleanup range, syntax restriction) to be **workspace-specific** ([#211](../../../issues/211))
* Renamed rule '**Prefer =, <>, <= etc. to EQ, NE, LE etc.**' to '**Use consistent set of comparison operators**' ([#273](../../../issues/273))
* Enhanced rule '**Use consistent set of comparison operators**' with option for **preferred** set ([#273](../../../issues/273))
* Enhanced rule '**Use consistent set of comparison operators**' with replacement of **obsolete** operators ([#283](../../../issues/283))
* Enhanced rule '**Delete unused variables**' with option to add **##NEEDED** in case of MESSAGE ... INTO ([#275](../../../issues/275))
* Enhanced rule '**Delete unused variables**' for **MESSAGE INTO** with **upfront declaration** ([#285](../../../issues/285))
* Updated **ABAP grammar** file
* Updated **known CamelCase names** for CDS views and fields

## 2024-03-25 (version 1.15.0)

**Thank you** very much [**jelliottp**](https://github.com/jelliottp), [**BenjaminWeisheit**](https://github.com/BenjaminWeisheit) and
[**cyb3rko**](https://github.com/cyb3rko) for your contributions, ideas and bug reports that led to these improvements!

* Added new rule '**Use CamelCase for known CDS names**'
* Added new rule '**Use string templates to assemble text**' ([#116](../../../issues/116))
* Added new rule '**Replace CONDENSE with string function**' ([#36](../../../issues/36))
* Fixed rule '**Report unused parameters**' for **CALL TRANSFORMATION** ... RESULT ([#267](../../../issues/267))

## 2024-03-11 (version 1.14.0)

Many **thanks** to [**jelliottp**](https://github.com/jelliottp) for inspiring these improvements!

* Added new rule '**Replace READ TABLE with table expression**' ([#36](../../../issues/36))
* Added new rule '**Replace DESCRIBE TABLE ... LINES with lines( )**' ([#36](../../../issues/36))
* Added further **references to Code Pal** for ABAP
* Updated **link** to webinar on **Open Source journey** of ABAP cleaner

## 2024-02-28 (version 1.13.3)

* Fixed rule '**Shorten VALUE statements**' for **comment line at end** of parenthesis

## 2024-02-27 (version 1.13.2)

**Thanks** a lot to [**JoachimEck**](https://github.com/JoachimEck), [**Falcon7EH**](https://github.com/Falcon7EH) 
and [**VladGhitulescu**](https://github.com/VladGhitulescu) for the bug reports behind these fixes!

* Enhanced rule '**Delete unused variables**' to **clean up obsolete comments** ([#257](../../../issues/257))
* Fixed rule '**Rearrange local declarations**' for **MODULE** ... ENDMODULE ([#258](../../../issues/258))
* Fixed rule '**Align SELECT lists**' for line breaks with **inline declarations** ([#251](../../../issues/251))
* Fixed rule '**Indent lines**' for multiple **commands on same line** ([#248](../../../issues/248))

## 2024-02-12 (version 1.13.1)

Many **thanks** to [**flying-crane**](https://github.com/flying-crane), [**owelzel**](https://github.com/owelzel), 
[**ConjuringCoffee**](https://github.com/ConjuringCoffee), [**I034943**](https://github.com/I034943), [**xczar0**](https://github.com/xczar0),
[**fmabap**](https://github.com/fmabap), [**this-yash**](https://github.com/this-yash) and [**mraht**](https://github.com/mraht) 
for your contributions, ideas and bug reports that led to these improvements!

* **Enabled** ABAP cleaner menus for **WebDynpro implementations** ([#220](../../../issues/220))
* Fixed rule '**Delete unused variables**' etc. for **dynamic ASSIGN** ([#237](../../../issues/237))
* Fixed rule '**Delete unused variables**' for **CONVERT DATE ... TIME ZONE** ([#243](../../../issues/243))
* Fixed rule '**Report unused parameters**' for **AMDP methods** ([#244](../../../issues/244))
* Fixed rule '**Align SELECT lists**' for SELECT **SINGLE FOR UPDATE** * ([#241](../../../issues/241))
* Fixed rule '**Align SELECT lists**' for FIELDS with one **CASE element** ([#229](../../../issues/229))
* Fixed rule '**Move pragmas to correct position**' for moving **multiple pragmas to line end** ([#228](../../../issues/228))
* Fixed rule '**Standardize empty lines within methods**' for classes without methods ([#225](../../../issues/225))
* Added **grayed-out Install New Software menu** to known **installation issues** ([#235](../../../issues/235))
* Fixed **partial recalculation** after local **blocking of rule**

## 2024-01-05 (version 1.13.0)

**Thank you** [**ConjuringCoffee**](https://github.com/ConjuringCoffee), [**kjetil-kilhavn**](https://github.com/kjetil-kilhavn),
 [**this-yash**](https://github.com/this-yash), [**VladGhitulescu**](https://github.com/VladGhitulescu) and [**stockbal**](https://github.com/stockbal) 
 for the ideas and bug reports behind these improvements!

* Added new rule '**Report unused parameters**' ([#224](../../../issues/224))
* Enhanced rule '**Align declarations**' for **enumerations** ([#194](../../../issues/194))
* Enhanced rule '**Align parameters and components**' with option '**Keep other one-liners**' ([#170](../../../issues/170))
* Enhanced rule '**Align logical expressions**' with examples for **ABAP SQL** ([#215](../../../issues/215))
* Aligned order of **OK and Cancel buttons** with OS standards ([#188](../../../issues/188))
* Added info message if profile is missing and fallback profile is used ([#219](../../../issues/219))
* Added info message if higher technical version is required ([#140](../../../issues/140))
* Fixed rule '**Shorten VALUE statements**' for **GROUP BY** group_key ([#226](../../../issues/226))
* Fixed rule '**Delete unused variables**' for variable with **same name** as a method ([#222](../../../issues/222))
* Fixed rule '**Align SELECT ... FROM ... JOIN**' for JOIN without **optional INNER** ([#221](../../../issues/221))
* Updated **ABAP grammar file** to latest version

## 2023-11-27 (version 1.12.0)

**Many thanks** to [**christianguenter2**](https://github.com/christianguenter2), [**VladGhitulescu**](https://github.com/VladGhitulescu) 
and [**bnichell**](https://github.com/bnichell) for the ideas behind these new features and fixes!

* Added links to **Open Source webinar** recording and slides
* Added new rule '**Align SELECT clauses**' ([#6](../../../issues/6))
* Added new rule '**Align SELECT ... FROM ... JOIN**' ([#6](../../../issues/6))
* Added new rule '**Align SELECT lists**' ([#6](../../../issues/6))
* Enhanced rule '**Align logical expressions**' with four options on '**SQL: Align AND / OR ...**' ([#193](../../../issues/193))
* Fixed rule '**Use FINAL for immutable variables**' for **CALL FUNCTION ... TABLES** ([#209](../../../issues/209))

## 2023-11-16 (version 1.11.2)

**Thank you** very much [**fmabap**](https://github.com/fmabap), [**LechnerClemens**](https://github.com/LechnerClemens) and
 [**RicardoLadeiro**](https://github.com/RicardoLadeiro) for the bug reports behind these improvements!

* Fixed rule '**Unchain into multiple statements**' for BEGIN OF ... END OF with **ABAP Doc** ([#204](../../../issues/204))
* Fixed rule '**Delete unused variables**' for variable names with **special chars in non-OO contexts** ([#202](../../../issues/202))
* Fixed rule '**Delete unused variables**' for **pragmas at line start** ([#200](../../../issues/200))

## 2023-11-14 (version 1.11.1)

Great **thanks** to [**VladGhitulescu**](https://github.com/VladGhitulescu) and
 [**Koch013**](https://github.com/Koch013) for reporting the bugs behind this release!

* Fixed rule '**Align declarations**' for mixed **TYPE** clauses with **tables** ([#186](../../../issues/186))
* Fixed rule '**Rearrange local declarations**' for multiple **empty lines** ([#195](../../../issues/195))
* Fixed **Profiles editor** layout for **long descriptions** ([#197](../../../issues/197))

## 2023-11-06 (version 1.11.0)

**Thanks** a lot to [**jan-jezek**](https://github.com/jan-jezek), [**jelliottp**](https://github.com/jelliottp) and
 [**ConjuringCoffee**](https://github.com/ConjuringCoffee) for opening the issues that led to these new features!

* Added new rule '**Remove needless parentheses**' ([#105](../../../issues/105))
* Added new rule '**Standardize assertion parameter order**' ([#85](../../../issues/85))
* Enhanced rule '**Unchain into multiple statements**' for **TYPES: BEGIN OF** and related table types ([#94](../../../issues/94))

## 2023-11-01 (version 1.10.0)

**Thanks a lot**, [**ConjuringCoffee**](https://github.com/ConjuringCoffee), [**csopaki**](https://github.com/csopaki), 
[**fmabap**](https://github.com/fmabap) and [**bnichell**](https://github.com/bnichell) for the ideas behind these new features and fixes!

* Added new rule '**Align PERFORM parameters**' ([#83](../../../issues/83))
* Added new rule '**Align FORM declarations**' ([#84](../../../issues/84))
* Added **command line option --CRLF**, changed default line separator to LF ([#177](../../../issues/177))
* Moved handling of spaces in empty brackets to rule '**Remove needless spaces**' ([#143](../../../issues/143))
* Enhanced rule '**Put spaces around text literals**' (renamed) for keywords, operators and comments ([#143](../../../issues/143))
* Enhanced rule '**Remove needless CLEAR**' for CLEAR **after BREAK-POINT and LOG-POINT**
* Fixed rule '**Standardize CLASS ... DEFINITION**' for dark theme ([#179](../../../issues/179))

## 2023-10-30 (version 1.9.0)

Great **thanks** to [**StefanRutzmoser**](https://github.com/StefanRutzmoser), [**ConjuringCoffee**](https://github.com/ConjuringCoffee),
 [**GKaschke**](https://github.com/GKaschke), [**thorsten79G**](https://github.com/thorsten79G), [**m-badura**](https://github.com/m-badura) and
 [**conlutio**](https://github.com/conlutio) for their ideas and bug reports!

* Added new rule '**Standardize CLASS ... DEFINITION**' ([#157](../../../issues/157))
* Added new rule '**Remove needless CLEAR**' ([#61](../../../issues/61), [#111](../../../issues/111))
* Added **syntax check** with RND Parser **after cleanup**
* Enhanced 8 rules in **rule group 'COMMANDS'** with '**unchain**' option ([#120](../../../issues/120))
* Fixed rule '**Replace CALL METHOD ...**' for 'CALL METHOD OF **ole**' ([#175](../../../issues/175))
* Fixed rule '**Replace CREATE OBJECT ...**' for 'CREATE OBJECT **ole** class' ([#175](../../../issues/175))
* Fixed rule '**Resolve equals sign chain ...**' for **implicit conversions** ([#173](../../../issues/173))
* Fixed rule '**Delete unused variables**' for **table expression after CLEAR** ([#169](../../../issues/169))

## 2023-10-23 (version 1.8.0)

**Great thanks** to [**PilotFlying**](https://github.com/PilotFlying), [**fmabap**](https://github.com/fmabap),
[**ConjuringCoffee**](https://github.com/ConjuringCoffee), [**GKaschke**](https://github.com/GKaschke), 
[**franzreitmayer**](https://github.com/franzreitmayer), [**JoachimEck**](https://github.com/JoachimEck) 
and [**MagneeR**](https://github.com/MagneeR) for opening the issues that led to these enhancements and fixes!

* Added **Ctrl + click navigation** from "Rules Used ..." list **to rule configuration** ([#161](../../../issues/161))
* Added menus '**Code / From Clipboard**' and 'Code / **From File**' **in read-only preview** ([#155](../../../issues/155))
* Added **documentation** of limitation to **non-functional changes** ([#158](../../../issues/158))
* Enhanced rule '**Align parameters and components**' for **RECEIVE RESULTS** FROM FUNCTION ([#166](../../../issues/166))
* Fixed read-only **team folders** for **non-existing directories** ([#160](../../../issues/160))
* Fixed **Parser** for **literals, host variables**, and **arithmetic expressions in SELECT** ([#165](../../../issues/165))
* Fixed **Parser** for **UP TO ... ROWS** in unusual position ([#159](../../../issues/159))
* Fixed rule '**Align conditional expressions**' for **SWITCH one-liners** ([#156](../../../issues/156))
* Fixed rule '**Align parameters and components**' for **non-table-row one-liners** ([#153](../../../issues/153))

## 2023-10-16 (version 1.7.2)

**Thanks a lot**, [**openPhiL**](https://github.com/openPhiL), [**TMNielsenApS**](https://github.com/TMNielsenApS) and 
[**ruhnla**](https://github.com/ruhnla) for your bug reports that led to these improvements!

* Fixed several **issues with comments, pragmas and colons** from **stress test** ([#147](../../../issues/147))
* Fixed **Parser** for chain colon in **CLASS: ... DEFINITION DEFERRED** ([#142](../../../issues/142))
* Fixed rule '**Align CLEAR:, FREE: and SORT**' for SORT: chains ([#141](../../../issues/141))

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
