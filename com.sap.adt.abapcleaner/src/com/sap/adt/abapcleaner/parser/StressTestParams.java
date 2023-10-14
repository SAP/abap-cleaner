package com.sap.adt.abapcleaner.parser;

public class StressTestParams {
	public final int insertAfterTokenIndexMin;
	public final int insertAfterTokenIndexMax;
	public final StressTestType[] stressTestTypes;

	/** Create parameters for stress-testing code cleanup by inserting comments, pragmas, or chain colons */
	public static StressTestParams create(int insertAfterTokenIndexMin, int insertAfterTokenIndexMax, StressTestType[] stressTestTypes) {
		return new StressTestParams(insertAfterTokenIndexMin, insertAfterTokenIndexMax, stressTestTypes);
	}
	
	private StressTestParams(int insertAfterTokenIndexMin, int insertAfterTokenIndexMax, StressTestType[] stressTestTypes) {
		this.insertAfterTokenIndexMin = insertAfterTokenIndexMin;
		this.insertAfterTokenIndexMax = insertAfterTokenIndexMax;
		this.stressTestTypes = stressTestTypes;
	}

	public int getCount() { 
		return stressTestTypes.length * Math.max(insertAfterTokenIndexMax - insertAfterTokenIndexMin + 1, 0); 
	}
}
