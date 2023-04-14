package com.sap.adt.abapcleaner.base;

/**
 * Represents the invariant culture used to write to or read from settings files, and to create file names.
 */
public final class SettingsCult {
	public static boolean parseBoolean(String value) {
		return (value.equals("1"));
	}

	public static int parseInt(String value) {
		try {
			return Integer.parseInt(value);
		} catch (Exception e) {
			return 0;
		}
	}

	public static float parseFloat(String value) {
		try {
			return Float.parseFloat(value);
		} catch (Exception e) {
			return 0F;
		}
	}

	public static double parseDouble(String value) {
		try {
			return Double.parseDouble(value);
		} catch (Exception e) {
			return 0.0;
		}
	}

	public static String toString(boolean value) {
		return value ? "1" : "0";
	}

	public static String toString(int value) {
		return Integer.toString(value);
	}

	public static String toString(float value) {
		return Float.toString(value);
	}

	public static String toString(double value) {
		return Double.toString(value);
	}
}
