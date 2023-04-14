[<-- previous rule](PragmaPositionRule.md) | [overview](../rules.md) | [next rule -->](EqualsSignChainRule.md)

# Correct frequent typos

Corrects frequent typos in textual comments, ABAP documentation, and literals. Only considers typos with unambiguous correction.

## Options

* \[X\] Correct frequent typos
* \[X\] Change from British English to American English
* \[X\] Apply on ABAP Doc
* \[X\] Apply on \(synchronized\) shorttexts
* \[X\] Apply on textual comments
* \[X\] Add TODO if textual comment is followed by MESSAGE command
* Apply on literals: \[add TODO comment\]

## Examples


```ABAP

"! This rule changes occurences of 723 frequent errorneous words to the correspondig correct word.
"! Typos are only changed if the correct word is unambiguous, as with 'succesfull' and 'availble'.
"! If there is any doubt - e.g. wether 'convered' meant 'covered' or 'converted' - NO chnage is made.
"! 
"! Additionally, some frequent word forms can be changed from British English to American English,
"! e.g. 'flavour', 'customisation', 'favourite' and 'initialisation'.
CLASS cl_frequent_typos DEFINITION.
  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="EN">Synchronised comment releated to method defintion</p>
    "! Additonal ABAP Doc comment which is not synchronised and only visibile in ADT
    "!
    "! @parameter iv_any   | <p class="shorttext synchronized">coresponding parameter desciption</p>
    "! @parameter is_other | <p class="shorttext synchronized">required strucutre paramater</p>
    METHODS correct_frequent_typos
      IMPORTING iv_any   TYPE string
                is_other TYPE ty_s_any.
ENDCLASS.


CLASS cl_frequent_typos IMPLEMENTATION.
  METHOD correct_frequent_typos.
*   very usefull comment on the busines logic in this method
*   and some describtion of its behaviour, depending on input paremeters

    " typos in text string literal
    task = `calcualte all availabe attributes`.
    perform_task( task ).

    " British English in string template
    info = |initialising { lv_count } occurences out of { lv_total }|.
    show_info( info ).

    " typos in text field literal
    message = 'analysing the optimisation of required authorisations'.
    show_message( message ).

    " The next comment is followed by a MESSAGE command, so it is likely that it contains the short text
    " of the message; in this case, typos may have to be manually corrected in the message class, too:
    " 'Fatal error ocurred! Processing was canceld.'
    MESSAGE e042(any_message_class) INTO lv_msg_str.
    lo_message_handler->add_symessage( ).
  ENDMETHOD.


  METHOD top_typos.
    " TOP TWELVE TYPOS by frequency in ABAP comments
    " ----------------------------------------------
    "  1. wether
    "  2. occurr; occured, occured; occurance, occurence
    "  3. successfull, succesful, succesfull, sucessfull; succesfully, sucessfully
    "  4. neccessary, neccesary, necesary
    "  5. existance; existant; exisiting, exisitng; exsits, exsists
    "  6. paramter, paramater; paramters, paremeters, parmeters, parametrs, parametes
    "  7. transfered, tranfered
    "  8. seperate; seperated; seperator; separatly
    "  9. dependant
    " 10. strucutre, strucure, struture, strcuture, sturcture, strcture, stucture
    " 11. choosen, choosed
    " 12. allready, alredy

    " TOP TEN WORDS by number of typo variants
    " ----------------------------------------
    " 7: strucutre, strucure, struture, strcuture, sturcture, strcture, stucture
    "    availabe, avaliable, avaialable, avaialble, availble, availible, avialable
    " 6: coresponding, correspondig, corrsponding, corresonding, corresponing, correspoding
    " 5: paramters, paremeters, parmeters, parametrs, parametes
    "    docuemnt, docuent, doucment, docuemt, docment
    " 4: successfull, succesful, succesfull, sucessfull
    "    selction, seleciton, selecton, seletion
    "    initalize, intialize, initalze, initialze
    "    atrributes, attibutes, atttributes, atributes
    "    applicaton, aplication, appliaction, applicaiton
  ENDMETHOD.
ENDCLASS.
```

Resulting code:

```ABAP

"! This rule changes occurrences of 723 frequent erroneous words to the corresponding correct word.
"! Typos are only changed if the correct word is unambiguous, as with 'successful' and 'available'.
"! If there is any doubt - e.g. whether 'convered' meant 'covered' or 'converted' - NO change is made.
"! 
"! Additionally, some frequent word forms can be changed from British English to American English,
"! e.g. 'flavor', 'customization', 'favorite' and 'initialization'.
CLASS cl_frequent_typos DEFINITION.
  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="EN">Synchronized comment related to method definition</p>
    "! Additional ABAP Doc comment which is not synchronized and only visible in ADT
    "!
    "! @parameter iv_any   | <p class="shorttext synchronized">corresponding parameter description</p>
    "! @parameter is_other | <p class="shorttext synchronized">required structure parameter</p>
    METHODS correct_frequent_typos
      IMPORTING iv_any   TYPE string
                is_other TYPE ty_s_any.
ENDCLASS.


CLASS cl_frequent_typos IMPLEMENTATION.
  METHOD correct_frequent_typos.
*   very useful comment on the business logic in this method
*   and some description of its behavior, depending on input parameters

    " typos in text string literal
    " TODO: check spelling: calcualte (typo) -> calculate (ABAP cleaner)
    " TODO: check spelling: availabe (typo) -> available (ABAP cleaner)
    task = `calcualte all availabe attributes`.
    perform_task( task ).

    " British English in string template
    " TODO: check spelling: initialising (BE) -> initializing (ABAP cleaner)
    " TODO: check spelling: occurences (typo) -> occurrences (ABAP cleaner)
    info = |initialising { lv_count } occurences out of { lv_total }|.
    show_info( info ).

    " typos in text field literal
    " TODO: check spelling: analysing (BE) -> analyzing (ABAP cleaner)
    " TODO: check spelling: optimisation (BE) -> optimization (ABAP cleaner)
    " TODO: check spelling: authorisations (BE) -> authorizations (ABAP cleaner)
    message = 'analysing the optimisation of required authorisations'.
    show_message( message ).

    " The next comment is followed by a MESSAGE command, so it is likely that it contains the short text
    " of the message; in this case, typos may have to be manually corrected in the message class, too:
    " TODO: check spelling: ocurred (typo) -> occurred (ABAP cleaner)
    " TODO: check spelling: canceld (typo) -> cancelled (ABAP cleaner)
    " 'Fatal error ocurred! Processing was canceld.'
    MESSAGE e042(any_message_class) INTO lv_msg_str.
    lo_message_handler->add_symessage( ).
  ENDMETHOD.


  METHOD top_typos.
    " TOP TWELVE TYPOS by frequency in ABAP comments
    " ----------------------------------------------
    "  1. whether
    "  2. occur; occurred, occurred; occurrence, occurrence
    "  3. successful, successful, successful, successful; successfully, successfully
    "  4. necessary, necessary, necessary
    "  5. existence; existent; existing, existing; exists, exists
    "  6. parameter, parameter; parameters, parameters, parameters, parameters, parameters
    "  7. transferred, transferred
    "  8. separate; separated; separator; separately
    "  9. dependent
    " 10. structure, structure, structure, structure, structure, structure, structure
    " 11. chosen, chosen
    " 12. already, already

    " TOP TEN WORDS by number of typo variants
    " ----------------------------------------
    " 7: structure, structure, structure, structure, structure, structure, structure
    "    available, available, available, available, available, available, available
    " 6: corresponding, corresponding, corresponding, corresponding, corresponding, corresponding
    " 5: parameters, parameters, parameters, parameters, parameters
    "    document, document, document, document, document
    " 4: successful, successful, successful, successful
    "    selection, selection, selection, selection
    "    initialize, initialize, initialize, initialize
    "    attributes, attributes, attributes, attributes
    "    application, application, application, application
  ENDMETHOD.
ENDCLASS.
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/syntax/TypoRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/syntax/TypoTest.java)

