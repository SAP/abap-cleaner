package com.sap.adt.abapcleaner.gui;

import java.util.ArrayList;

import com.sap.adt.abapcleaner.programbase.Release;
import com.sap.adt.abapcleaner.rulebase.Profile;

public class ProfileHighlightItems {
	private ArrayList<ProfileHighlightItem> items = new ArrayList<>();
	
	public ArrayList<ProfileHighlightItem> getItems() { return items; }
	
	public ProfileHighlightItems(ArrayList<Profile> compareToProfiles, Release[] releases) {
		items.add(ProfileHighlightItem.createForNoHighlight());
		for (Release release : releases) 
			items.add(ProfileHighlightItem.createForRelease(release));
		
		items.add(ProfileHighlightItem.createForCompareToProgramDefaults());
		for (Profile compareToProfile : compareToProfiles) 
			items.add(ProfileHighlightItem.createForCompareToProfile(compareToProfile));
	}
	
	public ProfileHighlightItem getAt(int index) {
		return (index < 0 || index >= items.size()) ? null : items.get(index);
	}

	public ProfileHighlightItem findItem(String persistentString) {
		for (ProfileHighlightItem item : items) {
			if (item.toPersistentString().equals(persistentString)) {
				return item;
			}
		}
		return null;
	}

	public int findIndex(String persistentString) {
		int result = 0;
		for (ProfileHighlightItem item : items) {
			if (item.toPersistentString().equals(persistentString)) {
				return result;
			}
			++result;
		}
		return -1;
	}
	
	static int indexOfPreviousRelease() {
		return 2;
	}
}
