package com.sap.adt.abapcleaner.rulehelpers;

public enum SelectClause {
	NONE,
	
	UNION,			// [UNION|INTERSECT|EXCEPT ...], possibly followed by one or multiple "("
	SELECT, 			// including [SINGLE [FOR UPDATE]]; possibly including select_clause (non-strict mode)
	FROM,				// FROM source (including joins)
	FIELDS,			// FIELDS select_clause (strict mode)
	FOR_ALL_ENTRIES, // FOR ALL ENTRIES IN itab 
	WHERE,			// WHERE sql_cond
	GROUP_BY,		// GROUP BY group
	HAVING,			// HAVING group_cond
	ORDER_BY,		// ORDER BY sort_key
	DB_HINTS,		// 
	
	INTO,				// INTO|APPENDING target 
	UP_TO_OFFSET,  // mainquery [UP TO ...] [OFFSET ...] / subquery [UP TO n ROWS [OFFSET o]] (before db_hints)
	ABAP_OPTIONS;

	public static final int length = values().length;
   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static SelectClause forValue(int value) {
      return values()[value];
   }	
}
