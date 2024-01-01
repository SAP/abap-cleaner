package com.sap.adt.abapcleaner.base;

public class SystemInfo {
	private static OperatingSystem operatingSystem = OperatingSystem.UNKNOWN; 

	public static OperatingSystem getOperatingSystem() {
		if (operatingSystem == OperatingSystem.UNKNOWN) {
			try { 
				String osNameUpper = System.getProperty("os.name").toUpperCase();
				if (osNameUpper.indexOf("WIN") >= 0) { // "Windows 10", "Windows 8.1" etc.
					operatingSystem = OperatingSystem.WINDOWS;

				} else if (osNameUpper.indexOf("MAC") >= 0) { // "Mac OS X" etc.
					operatingSystem = OperatingSystem.MACOS;
					
				} else if (osNameUpper.indexOf("NIX") >= 0 || osNameUpper.indexOf("NUX") >= 0 || osNameUpper.indexOf("AIX") > 0) { // "Linux" etc.
					operatingSystem = OperatingSystem.LINUX;
				
				} else {
					// for all other ("SunOS", "FreeBSD" etc.), we use Linux behavior 
					operatingSystem = OperatingSystem.LINUX;
				}

			} catch (SecurityException ex) {
			
			}
		}
		return operatingSystem;
	}
	
	public static boolean putOKBeforeCancel() {
		// Windows usually has "OK - Cancel" order, while macOS and Linux prefer "Cancel - OK" order
		return (getOperatingSystem() == OperatingSystem.WINDOWS); 
	}
}
