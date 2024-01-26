package com.sap.adt.abapcleaner.gui.eclipse;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.expressions.PropertyTester;

/**
 * Always returns <code>true</code>. Used in plugin.xml in combination with
 * <code>forcePluginActivation="true"</code> to make sure that the plugin is
 * activated and handlers' {@link AbstractHandler#setEnabled(Object)} is
 * considered even very early after ADT startup (Eclipse is optimistic with
 * regards to Handler enablement until the plugin is activated).
 */
public class DummyForcePluginActivationPropertyTester extends PropertyTester {

	@Override
	public boolean test(Object receiver, String property, Object[] args, Object expectedValue) {
		return true;
	}
}
