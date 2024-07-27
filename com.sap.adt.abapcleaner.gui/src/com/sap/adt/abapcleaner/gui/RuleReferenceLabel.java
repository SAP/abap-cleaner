package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;

import com.sap.adt.abapcleaner.base.StringUtil;

class RuleReferenceLabel {
   private final IConfigDisplay configDisplay;
   Label lblRuleSource;
   Label lblRuleChapter;
   final String ruleChapterLink;
   
   RuleReferenceLabel(IConfigDisplay configDisplay, Composite parent, String source, String chapterTitle, String chapterLink) {
   	this.configDisplay = configDisplay;
   	lblRuleSource = new Label(parent, SWT.NONE);
		GridData gd_lblRuleSource0 = new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1);
		gd_lblRuleSource0.minimumWidth = 120;
		lblRuleSource.setLayoutData(gd_lblRuleSource0);
		lblRuleSource.setBounds(0, 0, 160, 15);
		lblRuleSource.setText(StringUtil.getLabelText(source));
		
		lblRuleChapter = new Label(parent, SWT.NONE);
		lblRuleChapter.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDown(MouseEvent e) {
		      if (!StringUtil.isNullOrEmpty(ruleChapterLink)) {
		         ProgramLauncher.startProcess(ruleChapterLink);
		      }
			}
		});
		lblRuleChapter.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		lblRuleChapter.setText(StringUtil.getLabelText(chapterTitle));

		ruleChapterLink = chapterLink;
   }
   
   int getSourceRight() {
      Rectangle bounds = lblRuleSource.getBounds();
      return bounds.x + bounds.width;
   }

   void setChapterLeft(int chapterLeft) {
      lblRuleChapter.setLocation(chapterLeft, lblRuleChapter.getLocation().y);
   }
   
   void setVisible() {
		lblRuleSource.setVisible(true);
		lblRuleChapter.setVisible(true);
   }
   
   void detachAndDispose() {
   	Control[] controls = new Control[] { lblRuleSource, lblRuleChapter };

   	if (lblRuleSource != null) {
   		lblRuleSource = null;
   	}
   	if (lblRuleChapter != null) {
   		removeListeners(lblRuleChapter, SWT.MouseDown);
   		lblRuleChapter = null;
   	}

   	configDisplay.remove(controls);
      for (Control control : controls) {
         control.dispose();
      }
   }

   private void removeListeners(Control control, int eventType) {
   	Listener[] listeners = control.getListeners(eventType);
   	for (Listener listener : listeners) {
   		control.removeListener(eventType, listener);
   	}
   }
}
