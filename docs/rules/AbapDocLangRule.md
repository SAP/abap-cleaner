[<-- previous rule](AbapDocParametersRule.md) | [overview](../rules.md) | [next rule -->](CommentTypeRule.md)

# Remove lang="en" from ABAP Doc

Removes lang="en" from the HMTL tag <p class="shorttext synchronized" lang="en"> in ABAP Doc, since anyway, only the original language of the ABAP object is allowed, as defined in TADIR.

## Options

* \[ \] Also remove 'lang' attribute for other languages like lang="de"

## Examples


```ABAP

CLASS cl_any_class DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="EN">any method documentation</p>
    "!
    "! @parameter iv_any_parameter   | <p class="shorttext synchronized" lang="en">any parameter documentation</p>
    "! @parameter iv_other_parameter | <p class="shorttext synchronized" lang="en">other parameter documentation</p>
    "! @raising   cx_any_exception   | <p class="shorttext synchronized" lang="en">exception documentation</p>
    METHODS any_method
      IMPORTING
        !iv_any_parameter   TYPE i
        !iv_other_parameter TYPE i
      RAISING
        cx_any_exception.

    " if the original language of this class is English, lang="de" in the following lines triggers the warning
    " 'Language DE is not allowed. You have to use the original language EN'.

    "! <p class="shorttext synchronized" lang="DE">Dokumentation der anderen Methode</p>
    "!
    "! @parameter iv_first_parameter  | <p class="shorttext synchronized" lang="de">Dokumentation des 1. Parameters</p>
    "! @parameter iv_second_parameter | <p class="shorttext synchronized" lang="de">Dokumentation des 2. Parameters</p>
    METHODS other_method
      IMPORTING
        !iv_first_parameter  TYPE c
        !iv_second_parameter TYPE c.

    "! <p class="shorttext" lang="en">For non-synchronized shorttexts, lang="..." will NOT be removed,</p>
    "! <p class="shorttext" lang="en">because texts in non-original languages are allowed here.</p>
    "! <p class="shorttext" lang="DE">Fuer nicht-synchronisierte Kurztexte wird lang="..." NICHT entfernt,</p>
    "! <p class="shorttext" lang="DE">weil hier Texte in Nicht-Originalsprachen erlaubt sind.</p>
    CLASS-DATA mv_any_variable TYPE string.
ENDCLASS.
```

Resulting code:

```ABAP

CLASS cl_any_class DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    "! <p class="shorttext synchronized">any method documentation</p>
    "!
    "! @parameter iv_any_parameter   | <p class="shorttext synchronized">any parameter documentation</p>
    "! @parameter iv_other_parameter | <p class="shorttext synchronized">other parameter documentation</p>
    "! @raising   cx_any_exception   | <p class="shorttext synchronized">exception documentation</p>
    METHODS any_method
      IMPORTING
        !iv_any_parameter   TYPE i
        !iv_other_parameter TYPE i
      RAISING
        cx_any_exception.

    " if the original language of this class is English, lang="de" in the following lines triggers the warning
    " 'Language DE is not allowed. You have to use the original language EN'.

    "! <p class="shorttext synchronized" lang="DE">Dokumentation der anderen Methode</p>
    "!
    "! @parameter iv_first_parameter  | <p class="shorttext synchronized" lang="de">Dokumentation des 1. Parameters</p>
    "! @parameter iv_second_parameter | <p class="shorttext synchronized" lang="de">Dokumentation des 2. Parameters</p>
    METHODS other_method
      IMPORTING
        !iv_first_parameter  TYPE c
        !iv_second_parameter TYPE c.

    "! <p class="shorttext" lang="en">For non-synchronized shorttexts, lang="..." will NOT be removed,</p>
    "! <p class="shorttext" lang="en">because texts in non-original languages are allowed here.</p>
    "! <p class="shorttext" lang="DE">Fuer nicht-synchronisierte Kurztexte wird lang="..." NICHT entfernt,</p>
    "! <p class="shorttext" lang="DE">weil hier Texte in Nicht-Originalsprachen erlaubt sind.</p>
    CLASS-DATA mv_any_variable TYPE string.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/declarations/AbapDocLangRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/declarations/AbapDocLangTest.java)

