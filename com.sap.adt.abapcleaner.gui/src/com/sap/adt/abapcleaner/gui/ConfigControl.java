package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.graphics.Color;

import com.sap.adt.abapcleaner.rulebase.*;

abstract class ConfigControl {
   protected abstract void detachControls();
   public abstract Control[] getControls();
   public abstract Control[] getControlsForHighlight();
   public abstract void setDefault();
   public abstract void setEnabled(boolean enabled);
   
   protected ConfigValue configValue;
   protected IConfigDisplay configDisplay;

   private Color backgroundColorNormal;
   
   protected ConfigControl(ConfigValue configValue, IConfigDisplay configDisplay) {
      this.configValue = configValue;
      this.configDisplay = configDisplay;
   }

   public final void detachAndDispose() {
      Control[] controls = getControls();
      detachControls();
      configDisplay.remove(controls);
      for (Control control : controls)
         control.dispose();
   }
   
   protected void removeListeners(Control control, int eventType) {
   	Listener[] listeners = control.getListeners(eventType);
   	for (Listener listener : listeners)
   		control.removeListener(eventType, listener);
   }

   ConfigValue getConfigValue() {
   	return configValue;
   }
   
   public final void setHighlighted(boolean highlighted, Color highlightColor) {
      Control[] controls = getControlsForHighlight();
      if (controls != null && controls.length > 0) {
         if (backgroundColorNormal == null)
         	backgroundColorNormal = controls[0].getBackground();
	      for (Control control : controls) {
	      	Color color = highlighted ? highlightColor : backgroundColorNormal;
	      	if (!control.getBackground().equals(color))
	      		control.setBackground(color);
	      }
      }
   }
}
