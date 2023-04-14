package com.sap.adt.abapcleaner.gui.eclipse.application;

import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;

import com.sap.adt.abapcleaner.gui.FrmMain;

public class AbapCleanerApplication implements IApplication {

	@Override
	public Object start(IApplicationContext context) throws Exception {
		FrmMain.main(new String[] {});
		return IApplication.EXIT_OK;
	}

	@Override
	public void stop() {
	}

}
