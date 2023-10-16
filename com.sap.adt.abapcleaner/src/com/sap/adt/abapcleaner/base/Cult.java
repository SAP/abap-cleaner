package com.sap.adt.abapcleaner.base;

import java.time.*;

/**
 * Provides methods for culture-specific screen output such as number and date formatting.  
 */
public final class Cult {
	public static String format(int value) {
		return String.format("%,d", value);
	}

	public static String fromMillisec(int milliseconds) {
		return format(milliseconds) + " ms";
	}

	public static String format(double value, int digits) {
		return String.format("%." + (digits) + "f", value);
	}

	public static String getReverseDateTime(LocalDateTime dtm, boolean includeTime) {
		return getReverseDate(dtm, "") + (includeTime ? "_" + getReverseTime(dtm, "") : "");
	}

	public static String getReverseDate(LocalDateTime dtm, String separator) {
		return getPaddedString(dtm.getYear(), 4, '0') + separator 
			  + getPaddedString(dtm.getMonthValue(), 2, '0') + separator 
			  + getPaddedString(dtm.getDayOfMonth(), 2, '0');
	}

	public static String getReverseTime(LocalDateTime dtm, String separator) {
		return getPaddedString(dtm.getHour(), 2, '0') + separator
			  + getPaddedString(dtm.getMinute(), 2, '0') + separator
			  + getPaddedString(dtm.getSecond(), 2, '0');
	}

	public static String getPaddedString(int value, int minLength, char padding) {
		String result = SettingsCult.toString(value);
		if (result.length() >= minLength)
			return result;
		else
			return StringUtil.repeatChar(padding, minLength - result.length()) + result;
	}
}