[<-- previous rule](AlignPerformRule.md) | [overview](../rules.md) | [next rule -->](DdlAnnotationNestingRule.md)

# Standardize annotation layout

Standardizes spaces, one-liners and alignment of DDL annotations.

Starting new lines before a comma is not supported. Spaces before '@' and around '.' will always be removed.

## Options

* Spaces
* \[X\] Space after colon
* \[X\] Space inside braces \{ ... \}
* \[X\] Space inside array brackets \[ ... \]
* One-liners
* Maximum line length for one-liners \[120\] 
* Max. number of elements for one-liners in entity annotations: \[4\] 
* Max. number of elements for one-liners in select lists etc.: \[4\] 
* Alignment
* \[ \] Align values of nested elements \(unless an element contains a dot\)
* \[X\] Align 'tables' of structure-identical annotations in arrays

## Examples


```ASDDLS

@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'
@ObjectModel. lifecycle. draft .expiryInterval:'PT28D'
@ObjectModel.usageType: { serviceQuality: #D,
                            sizeCategory:#XXL,
                                 dataClass :#MIXED }

@ObjectModel.supportedCapabilities: [#ANALYTICAL_QUERY,
                                      #ANALYTICAL_DOCUMENT_STORE,
                                    #ANALYTICAL_KPI,
                                        #ANALYTICAL_PROVIDER ]
@ObjectModel.action:[{instance:{bound:true}},{enabled:true},{feature:''}]

@UI.selectionVariant: [ {
  qualifier: 'params',
  parameters: [ {
    name: 'P_AnyParam',
    value: 'any'
  }, {
    name: 'P_OtherParam',
    value: 'other'
  }, {
    name:  'P_Third',
    value: 'third'
} ] } ]

define view entity C_AnyEntity
  as select from I_OtherEntity

{
      @Consumption.filter: { selectionType: #SINGLE,
                             multipleSelections  :false,
                            mandatory:false}
      @Search.defaultSearchElement: true
      @  Search.fuzzinessThreshold :0.7
  key AnyKeyField,

      @ObjectModel.text.element:[   'SemanticObjectName']
      @Consumption.valueHelpDefinition:
      [
        {
          entity:
          {
            name: 'I_AnyName',
            element: 'ElementName'
          }
        }
      ]
      AnyNonKeyField
}
```

Resulting code:

```ASDDLS

@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'
@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'
@ObjectModel.usageType: { serviceQuality: #D, sizeCategory: #XXL, dataClass: #MIXED }

@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,
                                      #ANALYTICAL_DOCUMENT_STORE,
                                      #ANALYTICAL_KPI,
                                      #ANALYTICAL_PROVIDER ]
@ObjectModel.action: [ { instance: { bound: true } },
                       { enabled: true },
                       { feature: '' } ]

@UI.selectionVariant: [ { qualifier: 'params',
                          parameters: [ { name: 'P_AnyParam',   value: 'any'   },
                                        { name: 'P_OtherParam', value: 'other' },
                                        { name: 'P_Third',      value: 'third' } ] } ]

define view entity C_AnyEntity
  as select from I_OtherEntity

{
      @Consumption.filter: { selectionType: #SINGLE, multipleSelections: false, mandatory: false }
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.7
  key AnyKeyField,

      @ObjectModel.text.element: [ 'SemanticObjectName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName', element: 'ElementName' } } ]
      AnyNonKeyField
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/annotations/DdlAnnotationLayoutRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/annotations/DdlAnnotationLayoutTest.java)

