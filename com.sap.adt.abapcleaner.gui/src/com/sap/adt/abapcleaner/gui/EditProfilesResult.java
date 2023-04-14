package com.sap.adt.abapcleaner.gui;

class EditProfilesResult {
	final boolean saved;
	final String lastProfileName;
	
	EditProfilesResult(boolean saved, String lastProfileName) {
		this.saved = saved;
		this.lastProfileName = lastProfileName;
	}
}
