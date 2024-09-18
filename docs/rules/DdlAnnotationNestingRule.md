[<-- previous rule](DdlAnnotationLayoutRule.md) | [overview](../rules.md) | [next rule -->](DdlPositionDefineRule.md)

# Rearrange annotations

Rearranges the nesting and order of annotations and groups them with empty lines.

Note that this rule reuses the configuration of the rule 'Standardize annotation layout', even if it is deactivated.

## Options

* Nesting
* Use nesting for structured annotations: \[Starting from level 3\]
* To refine the above setting, you can enter comma-separated annotation names to always/never be nested \(e.g. "Consumption.valueHelpDefinition, ObjectModel.usageType, Search"\):
* Always allow nesting for annotations starting with: \[\]
* Always block nesting for annotations starting with: \[\]
* Order
* Order of annotations: \[Sort by first and second level, keep others as they are\]
* Empty lines
* Put empty lines between entity annotations: \[For multi-liners or if first element differs\]
* Put empty lines between select list annotations: \[Never\]

## Examples


```ASDDLS

@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'
@ObjectModel.usageType: { serviceQuality: #D }
@ObjectModel.usageType.sizeCategory: #XXL
@ObjectModel.modelingPattern: #ANALYTICAL_QUERY
@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,
                                      #ANALYTICAL_DOCUMENT_STORE,
                                      #ANALYTICAL_KPI,
                                      #ANALYTICAL_PROVIDER ]
@ObjectModel.usageType.dataClass: #MIXED
@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'
@ObjectModel.action: [ { instance: { bound: true } }, { enabled: true }, { feature: '' } ]
@ObjectModel.delegatedAction: [ { enabled: true } ]

define view entity C_AnyEntity
  as select from I_OtherEntity

{
      @Consumption.filter: { selectionType: #SINGLE }
      @Consumption.filter.multipleSelections: false
      @Consumption.filter.mandatory: false
      @Search.defaultSearchElement:true
      @Search.fuzzinessThreshold: 0.7
  key AnyKeyField,

      @Consumption.semanticObject: 'AnySemanticObject'
      @ObjectModel.text.element: [ 'SemanticObjectName' ]
      @Consumption.valueHelpDefinition: [ { entity.name: 'I_AnyName',
                                            entity.element: 'ElementName' } ]
      AnyNonKeyField
}
```

Resulting code:

```ASDDLS

@ObjectModel.action: [ { instance.bound: true },
                       { enabled: true },
                       { feature: '' } ]

@ObjectModel.delegatedAction: [ { enabled: true } ]
@ObjectModel.lifecycle.draft: { notificationBeforeExpiryInterval: 'PT10D', expiryInterval: 'PT28D' }
@ObjectModel.modelingPattern: #ANALYTICAL_QUERY

@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,
                                      #ANALYTICAL_DOCUMENT_STORE,
                                      #ANALYTICAL_KPI,
                                      #ANALYTICAL_PROVIDER ]

@ObjectModel.usageType: { serviceQuality: #D, sizeCategory: #XXL, dataClass: #MIXED }

define view entity C_AnyEntity
  as select from I_OtherEntity

{
      @Consumption.filter: { selectionType: #SINGLE, multipleSelections: false, mandatory: false }
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.7
  key AnyKeyField,

      @Consumption.semanticObject: 'AnySemanticObject'
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName', element: 'ElementName' } } ]
      @ObjectModel.text.element: [ 'SemanticObjectName' ]
      AnyNonKeyField
}
```

## Related code

* [Rule implementation](../../com.sap.adt.abapcleaner/src/com/sap/adt/abapcleaner/rules/ddl/annotations/DdlAnnotationNestingRule.java)
* [Tests](../../test/com.sap.adt.abapcleaner.test/src/com/sap/adt/abapcleaner/rules/ddl/annotations/DdlAnnotationNestingTest.java)

