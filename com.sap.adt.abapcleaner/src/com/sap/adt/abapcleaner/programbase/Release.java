package com.sap.adt.abapcleaner.programbase;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;

import org.osgi.framework.Version;

public class Release {
	public final LocalDate releaseDate;
	public final Version version;
	
	public static Release create(String version, int year, int month, int date) {
		return new Release(Version.parseVersion(version), LocalDate.of(year, month, date));
	}

	public Release(Version version, LocalDate releaseDate) {
		this.version = version;
		this.releaseDate = releaseDate;
	}
	
	@Override
	public String toString() {
		return "v" + version + " (" + releaseDate.format(DateTimeFormatter.ofLocalizedDate(FormatStyle.MEDIUM)) + ")";
	}
	
	public boolean wasEarlierThan(int year, int month, int dayOfMonth) {
		return (releaseDate.compareTo(LocalDate.of(year, month, dayOfMonth)) < 0);
	}

	public boolean wasEarlierThan(LocalDate featureDate) {
		return (releaseDate.compareTo(featureDate) < 0);
	}
}
