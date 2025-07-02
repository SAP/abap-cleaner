package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.FillLayout;

import org.eclipse.swt.SWT;

import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Point;

import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.wb.swt.SWTResourceManager;

import java.io.IOException;
import java.util.*;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.comparer.*;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Obfuscator;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;

import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.ModifyEvent;

public class FrmProfiles implements IConfigDisplay, IFallbackKeyListener {
   private final static boolean sortByGroup = true;
	private static final int CONTROL_KEY = SWT.MOD1; // better macOS support than SWT.CONTROL
	private static final int SHIFT_KEY = SWT.MOD2;

   private CodeDisplayColors codeDisplayColors;
   
   private ArrayList<String> initialProfileNames;
   private ArrayList<Profile> profiles = new ArrayList<Profile>();
   private Profile curProfile;
   private Rule curRule;
   private ProfileHighlightItems shownHighlightItems;
   private String curExampleCode;
   private RuleID defaultRuleID;
   
   private MainSettings settings;
   private boolean resultSave; // is set to false by Cancel button
   private String resultProfileName; 

   private int suspendItemCheck = 0;

   private final ArrayList<ConfigControl> configControls = new ArrayList<ConfigControl>();
   private final ArrayList<RuleReferenceLabel> ruleReferenceLabels = new ArrayList<RuleReferenceLabel>();
   private Object[] itemsInChkRules;

   private Shell shell;
	private Table chkRules;
	private List lstProfiles;
   private Button btnDeleteProfile;
	private Button btnRenameProfile;
   private Button btnImportProfile;
   private Button btnExportProfile;
   private Button btnExportAllProfiles;
   private Button chkAutoActivateNewFeatures;
	private Label lblRules;
	private Composite pnlRule;
	private Label lblRuleName;
	private Composite pnlRuleDescription;
	private Label lblRuleDescription;
	private Label lblRuleHintsAndRestrictions;
	private Composite pnlRuleReferences;
	private Button btnDefaultOptions;
	private Composite pnlRuleOptions;
	private CodeDisplay codeDisplay;
   private Button chkHighlightChanges;
   private Button btnPasteExample;
   private Composite pnlExamplesInfo;
   private Label lblExamples;
   private Label lblHighlight;
   private Combo cboHighlight;
   private Label lblFilter;
   private Text txtFilter;
   private Button btnObfuscate;
   private Button btnGenerateUnitTestClass;
   private Button btnGenerateUnitTest;
   private Button btnGenerateExample;
   private Composite pngProfileImportExport;
   private Button btnOK;
   private Button btnCancel;
   private Label lblActivate;
   private Button btnActivateAllRules; 
   private Button btnActivateDefaultRules;
   private Button btnActivateEssentialRules;
   private Button btnDeactivateAllRules;
   private Composite composite_1;
   private Button chkHighlightDeclarationKeywords;
   private Button chkHighlightWritePositions;
   private Button btnChangeProfilesFolder;

   /** encapsulates fonts and colors for highlighting rules, options and UI controls */
   private class Highlighter {
   	private final CodeDisplayColors codeDisplayColors;
   	
   	// fonts and colors for rule reference chapters
      private final Font ruleChaptersFontLink;
      private final Font ruleChaptersFontNormal;
      private final Color ruleChaptersForeColorNormal;
      private final Color ruleChaptersForeColorLink;
      
      // colors for rules list and rule name
      private final Color normalRuleListBackground;
      private final Color normalRuleNameBackground;
      
      private final Color normalAutoActivateBackground;
      private final Color normalHighlightBackground;
      private final Color normalPasteExampleBackground;
      private final Color normalImportExportBackground;
      private final Color normalProfilesFolderBackground;
      private final Color normalLblFilterBackground;
      private final Color normalActivateBackground;
      private final Color normalHighlightDeclarationsBackground;
      private final Color normalHighlightWritePositionsBackground;   
      
      public Highlighter(Display display, CodeDisplayColors codeDisplayColors) {
      	this.codeDisplayColors = codeDisplayColors;
      	
   		// determine fonts and colors of rule chapters
         ruleChaptersForeColorNormal = lblRuleDescription.getForeground();
         ruleChaptersForeColorLink = codeDisplayColors.textLink; // dark blue
         ruleChaptersFontNormal = lblRuleDescription.getFont();
         FontData modelFont = ruleChaptersFontNormal.getFontData()[0]; 
         ruleChaptersFontLink = new Font(display, modelFont.getName(), modelFont.getHeight(), SWT.NORMAL);

         // determine 'normal' colors for features that could be highlighted as "added" or "changed"
         normalRuleListBackground = chkRules.getBackground();
         normalRuleNameBackground = lblRuleName.getBackground();
         normalAutoActivateBackground = chkAutoActivateNewFeatures.getBackground();
         normalHighlightBackground = lblHighlight.getBackground();
         normalPasteExampleBackground = btnPasteExample.getBackground();
         normalImportExportBackground = btnImportProfile.getBackground();
         normalProfilesFolderBackground = btnChangeProfilesFolder.getBackground();
         normalLblFilterBackground = lblFilter.getBackground();
         normalActivateBackground = btnActivateDefaultRules.getBackground();
         normalHighlightDeclarationsBackground = chkHighlightDeclarationKeywords.getBackground();
         normalHighlightWritePositionsBackground = chkHighlightWritePositions.getBackground();
      }
      
      public void dispose() {
   		ruleChaptersFontLink.dispose();
      }

		public void setControlsHighlight(ProfileHighlightItem highlightItem) {
			setBackground(chkAutoActivateNewFeatures, highlightItem.highlightFeatureOf(2022, 6, 5) ? codeDisplayColors.configAddedBackground : normalAutoActivateBackground);
	      setBackground(lblHighlight, highlightItem.highlightFeatureOf(2022, 10, 31) ? codeDisplayColors.configAddedBackground : normalHighlightBackground); // first highlight on (2022, 9, 8)
	      setBackground(btnPasteExample, highlightItem.highlightFeatureOf(2022, 5, 25) ? codeDisplayColors.configAddedBackground : normalPasteExampleBackground);
	      
	      Color importExportBackground = highlightItem.highlightFeatureOf(2022, 10, 14) ? codeDisplayColors.configAddedBackground : normalImportExportBackground;
	      setBackground(btnImportProfile, importExportBackground);
	      setBackground(btnExportProfile, importExportBackground);
	      setBackground(btnExportAllProfiles, importExportBackground);
	      setBackground(btnChangeProfilesFolder, highlightItem.highlightFeatureOf(2023, 10, 3) ? codeDisplayColors.configAddedBackground : normalProfilesFolderBackground); // first highlight on (2023, 3, 9)
	
	      setBackground(lblFilter, highlightItem.highlightFeatureOf(2023, 3, 9) ? codeDisplayColors.configAddedBackground : normalLblFilterBackground);
	      
	      Color activateBackground = highlightItem.highlightFeatureOf(2023, 3, 7) ? codeDisplayColors.configAddedBackground : normalActivateBackground;
	      setBackground(btnActivateAllRules, normalActivateBackground);
	      setBackground(btnActivateDefaultRules, activateBackground);
	      setBackground(btnActivateEssentialRules, activateBackground);
	      setBackground(btnDeactivateAllRules, normalActivateBackground);
	
	      setBackground(chkHighlightDeclarationKeywords, highlightItem.highlightFeatureOf(2023, 3, 28) ? codeDisplayColors.configAddedBackground : normalHighlightDeclarationsBackground);
	      setBackground(chkHighlightWritePositions, highlightItem.highlightFeatureOf(2023, 3, 9) ? codeDisplayColors.configAddedBackground : normalHighlightWritePositionsBackground);
		}

	   private void setBackground(Control control, Color color) {
	   	if (!control.getBackground().equals(color)) {
	   		control.setBackground(color);
	   	}
		}

	   private Color getRuleBackground(Rule rule, ProfileHighlightItem highlightItem, boolean forRuleList) {
	   	Color normalBackground = forRuleList ? normalRuleListBackground : normalRuleNameBackground;
	   	if (rule == null || highlightItem == null)
	      	return normalBackground;

			Release release = highlightItem.getRelease();
			Profile compareToProfile = highlightItem.getCompareToProfile();
	   	if (release != null) {
		   	if (rule.wasAddedSince(release)) { 
		      	return codeDisplayColors.ruleAddedBackground;
		   	} else if(rule.wasEnhancedSince(release)) { 
		      	return codeDisplayColors.ruleChangedBackground;
		   	}
		   	
	   	} else if (compareToProfile != null) {
	   		Rule compareToRule = compareToProfile.getRule(rule.getID());
	   		if (compareToRule == null)  {
	   	   	return normalBackground;
	   		} else if (rule.isActive && !compareToRule.isActive) {
		      	return codeDisplayColors.ruleActivatedBackground;
	   		} else if (!rule.isActive && compareToRule.isActive) {
		      	return codeDisplayColors.ruleDeactivatedBackground;
	   		} else if (!rule.hasSameConfigurationAs(compareToRule)) {
	   			return codeDisplayColors.configChangedBackground;
	   		}
	   	}

	   	return normalBackground;
	   }
	   
	   private void setRuleHighlight(Rule rule, int index, ProfileHighlightItem highlightItem) {
	      Color ruleBackground = getRuleBackground(rule, highlightItem, true);
	   	TableItem tableItem = chkRules.getItem(index);
	      if (!tableItem.getBackground().equals(ruleBackground))
	      	tableItem.setBackground(ruleBackground);
	   }
	   
	   private void setRuleNameHighlight(Rule rule) {
	   	setBackground(lblRuleName, getRuleBackground(rule, getHighlightItem(), false));
		}

		public void setRuleChaptersStyle(Label lblRuleChapter, boolean hasLink) {
			lblRuleChapter.setFont(hasLink ? ruleChaptersFontLink : ruleChaptersFontNormal);
			lblRuleChapter.setForeground(hasLink ? ruleChaptersForeColorLink : ruleChaptersForeColorNormal);
		}

	   private void setConfigHighlight(Rule rule, ConfigControl configControl) {
	   	ConfigValue configValue = configControl.getConfigValue();
	      ProfileHighlightItem highlightItem = getHighlightItem();
	   	if (highlightItem != null && highlightItem.isNewConfig(configValue)) {
	      	configControl.setHighlighted(true, codeDisplayColors.configAddedBackground);
	   	} else if (highlightItem != null && highlightItem.isChangedConfig(rule, configValue)) {
	      	configControl.setHighlighted(true, codeDisplayColors.configChangedBackground);
	   	} else {
	      	configControl.setHighlighted(false, null);
	   	}
		}
   }

   private Highlighter highlighter;

   /**
	 * Open the window.
	 * @wbp.parser.entryPoint
	 */
	public EditProfilesResult open(String curProfileName, MainSettings settings, boolean showVerticalLine, int verticalLinePos, CodeDisplayColors codeDisplayColors) {
		this.settings = settings;
		this.codeDisplayColors = codeDisplayColors;
		this.defaultRuleID = settings.profilesLastRuleID;
		
      // load profiles 
      ArrayList<Profile> profiles = Profile.loadProfiles(settings.profilesDirectory, settings.readOnlyProfileDirs, null);
      this.profiles = profiles;

      // remember the profile names when opening FrmProfiles
		initialProfileNames = new ArrayList<>();
		for (Profile profile : profiles) {
      	initialProfileNames.add(profile.name);
      }

		Display display = Display.getDefault();
		createContents();

		createPopupMenuFor(lblRuleName, null, "Copy Rule Name to Clipboard");
		createPopupMenuFor(lblRuleDescription, lblRuleHintsAndRestrictions, "Copy Rule Description to Clipboard");

		btnDefaultOptions.setVisible(false);

      codeDisplay.setVerticalLine(showVerticalLine, verticalLinePos);
		codeDisplay.setFallbackKeyListener(this);
      codeDisplay.setHighlightDeclarationKeywords(settings.profilesHighlightDeclarationKeywords);
      codeDisplay.setHighlightWritePositions(settings.profilesHighlightWritePositions);

      // clear Labels to improve display of shell before it is filled with content
      lblRuleName.setText("");
      
      btnObfuscate.setVisible(Program.showDevFeatures());
      btnGenerateUnitTestClass.setVisible(Program.showDevFeatures());
      btnGenerateUnitTest.setVisible(Program.showDevFeatures());
      btnGenerateExample.setVisible(Program.showDevFeatures());

      // fill the lists of profiles and rules and show the selected rule, without highlighting them yet  
      refreshProfileList(curProfileName);

		shell.open();
		shell.layout();

      // only now, Colors for highlighting can be retrieved and highlighting applied - otherwise colors would be wrong for dark mode  
		highlighter = new Highlighter(display, codeDisplayColors);
		refreshHighlight(false);
      
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}

		highlighter.dispose();

      if (resultSave) { 
	      // delete files of deleted or renamed profiles
      	Persistency persistency = Persistency.get();
	      for (String profileName : initialProfileNames) {
	         if (findProfile(profileName) == null) {
	         	String deletePath = Profile.getSavePath(settings.profilesDirectory, profileName);
	         	if (persistency.fileExists(deletePath)) {
	         		persistency.deleteFile(deletePath);
	         	}
	         }
	      }
	      // save profiles in case they were changed
	      for (Profile profile : profiles) {
	      	if (!profile.isReadOnly) {
	      		profile.save(settings.profilesDirectory);
	      	}
	      }
      }
      return new EditProfilesResult(resultSave, resultProfileName);
	}

	private void createPopupMenuFor(Label lbl, Label lbl2, String menuText) {
      Menu mnuPopup = new Menu(shell, SWT.POP_UP);
      MenuItem mnuCopyLblTextToClip = new MenuItem(mnuPopup, SWT.NONE);
      mnuCopyLblTextToClip.setText(menuText);
      mnuCopyLblTextToClip.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (lbl == null || lbl.isDisposed())
					return;
				String lblText = lbl.getText();
				if (lbl2 != null && !StringUtil.isNullOrEmpty(lbl2.getText()))
					lblText += " " + lbl2.getText();
				SystemClipboard.setText(lblText);
			}
      });
      lbl.setMenu(mnuPopup);
      if (lbl2 != null) {
         lbl2.setMenu(mnuPopup);
      }
	}
	
	/**
	 * Create contents of the window.
	 */
	protected void createContents() {
		shell = new Shell(SWT.APPLICATION_MODAL | SWT.BORDER | SWT.TITLE | SWT.RESIZE | SWT.MAX);
		shell.setMinimumSize(new Point(880, 520));
		shell.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				keyPressedInCodeDisplay(e);
			}
		});

		shell.addShellListener(new ShellAdapter() {
			@Override
			public void shellActivated(ShellEvent e) {
				// refresh the example, because new custom CamelCase view or field names could have been added to the text files
				// (CamelCaseNameRule, CamelCaseInCdsTestRule, DdlCamelCaseNameRule)
				if (curRule != null && curRule.dependsOnExternalFiles()) { 
					refreshExample(curRule, null);
				}
			}
		});

		shell.setImage(SWTResourceManager.getImage(FrmProfiles.class, "/ShellImage.png"));
		shell.setSize(1024, 768);
		shell.setMaximized(true);
		shell.setText("Profiles and Rules");
		shell.setLayout(new GridLayout(2, false));

		Composite cpsProfilesAndRules = new Composite(shell, SWT.NONE);
		cpsProfilesAndRules.setLayout(new GridLayout(1, false));
		cpsProfilesAndRules.setLayoutData(new GridData(SWT.LEFT, SWT.FILL, false, true, 1, 1));
		
		Label lblProfiles = new Label(cpsProfilesAndRules, SWT.NONE);
		lblProfiles.setFont(SWTResourceManager.getFont("Segoe UI", 11, SWT.BOLD));
		lblProfiles.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		lblProfiles.setText("Profiles");
		
		lstProfiles = new List(cpsProfilesAndRules, SWT.BORDER | SWT.V_SCROLL);
		lstProfiles.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
	      	setCurrentProfile();
			}
		});
		GridData gd_lstProfiles = new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1);
		gd_lstProfiles.heightHint = 145;
		lstProfiles.setLayoutData(gd_lstProfiles);
		
		Composite pnlProfileButtons = new Composite(cpsProfilesAndRules, SWT.NONE);
		pnlProfileButtons.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		pnlProfileButtons.setLayout(new FillLayout(SWT.HORIZONTAL));
		
		Button btnCreateProfile = new Button(pnlProfileButtons, SWT.NONE);
		btnCreateProfile.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				createProfile();
			}
		});
		btnCreateProfile.setText("Create");
		
		Button btnCopyProfile = new Button(pnlProfileButtons, SWT.NONE);
		btnCopyProfile.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				copyProfile();
			}
		});
		btnCopyProfile.setText("Copy");
		
		btnDeleteProfile = new Button(pnlProfileButtons, SWT.NONE);
		btnDeleteProfile.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				deleteProfile();
			}
		});
		btnDeleteProfile.setText("Delete");
		
		btnRenameProfile = new Button(pnlProfileButtons, SWT.NONE);
		btnRenameProfile.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				renameProfile();
			}
		});
		btnRenameProfile.setText("Rename");
		
		pngProfileImportExport = new Composite(cpsProfilesAndRules, SWT.NONE);
		pngProfileImportExport.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		pngProfileImportExport.setLayout(new FillLayout(SWT.HORIZONTAL));
		
		btnImportProfile = new Button(pngProfileImportExport, SWT.NONE);
		btnImportProfile.setToolTipText("import profile(s) from a different folder");
		btnImportProfile.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				importProfiles();
			}
		});
		btnImportProfile.setText("Import...");
		
		btnExportProfile = new Button(pngProfileImportExport, SWT.NONE);
		btnExportProfile.setToolTipText("export selected profile to a different folder");
		btnExportProfile.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
		   	if (curProfile != null) {
		   		exportProfiles(new Profile[] { curProfile } );
		   	}
			}
		});
		btnExportProfile.setText("Export...");
		
		btnExportAllProfiles = new Button(pngProfileImportExport, SWT.NONE);
		btnExportAllProfiles.setToolTipText("export all profiles (excluding profiles from read-only folders) to a different folder");
		btnExportAllProfiles.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				// only export profiles from the own directory, not from the read-only (team) profile directories 
	   		ArrayList<Profile> ownProfiles = new ArrayList<>(profiles);
	   		ownProfiles.removeIf(p -> p.isReadOnly);
		   	if (ownProfiles.size() > 0) {
		   		Profile[] profileArray = new Profile[ownProfiles.size()];
		   		exportProfiles(ownProfiles.toArray(profileArray));
		   	}
			}
		});
		btnExportAllProfiles.setText("Export All...");
		
		btnChangeProfilesFolder = new Button(pngProfileImportExport, SWT.NONE);
		btnChangeProfilesFolder.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				changeProfileDirs();	
			}
		});
		btnChangeProfilesFolder.setToolTipText("change the folders in which own and team profiles are stored");
		btnChangeProfilesFolder.setText("Folders...");
		
		lblRules = new Label(cpsProfilesAndRules, SWT.NONE);
		GridData gd_lblRules = new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1);
		gd_lblRules.verticalIndent = 18;
		lblRules.setLayoutData(gd_lblRules);
		lblRules.setFont(SWTResourceManager.getFont("Segoe UI", 11, SWT.BOLD));
		lblRules.setText("Rules in Current Profile");
		 
		chkAutoActivateNewFeatures = new Button(cpsProfilesAndRules, SWT.CHECK);
		chkAutoActivateNewFeatures.setText("Automatically activate new features after updates");
		chkAutoActivateNewFeatures.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
		      boolean autoActivate = chkAutoActivateNewFeatures.getSelection();
		      if (curProfile != null)
		      	curProfile.autoActivateNewFeatures = autoActivate;
			}
		});

		Composite cpsHighlight = new Composite(cpsProfilesAndRules, SWT.NONE);
		cpsHighlight.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		GridLayout gl_cpsHighlight = new GridLayout(2, false);
		gl_cpsHighlight.marginWidth = 0;
		cpsHighlight.setLayout(gl_cpsHighlight);
		
		lblHighlight = new Label(cpsHighlight, SWT.NONE);
		lblHighlight.setText("Highlight:");
		
		cboHighlight = new Combo(cpsHighlight, SWT.READ_ONLY);
		cboHighlight.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		cboHighlight.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				setCurrentProfile();
		      ProfileHighlightItem highlightItem = getHighlightItem();
		      if (highlightItem != null) {
		      	settings.profilesHighlightItem = highlightItem.toPersistentString();
		      }
			}
		});
		
		lblFilter = new Label(cpsHighlight, SWT.NONE);
		lblFilter.setText("Filter:");

		Composite cpsFilterText = new Composite(cpsHighlight, SWT.NONE);
		cpsFilterText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		GridLayout gl_cpsFilterText = new GridLayout(2, false);
		gl_cpsFilterText.marginHeight = 0;
		gl_cpsFilterText.marginWidth = 0;
		cpsFilterText.setLayout(gl_cpsFilterText);
		
		txtFilter = new Text(cpsFilterText, SWT.BORDER);
		txtFilter.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				setCurrentProfile();
			}
		});
		txtFilter.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtFilter.setToolTipText("filter list by rule name");
		
		Button btnClearFilter = new Button(cpsFilterText, SWT.NONE);
		btnClearFilter.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				txtFilter.setText("");
			}
		});
		btnClearFilter.setText("X");
		btnClearFilter.setToolTipText("clear rule name filter");
		
		CheckboxTableViewer checkboxTableViewer = CheckboxTableViewer.newCheckList(cpsProfilesAndRules, SWT.BORDER | SWT.FULL_SELECTION);
		chkRules = checkboxTableViewer.getTable();
		chkRules.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				boolean shiftPressed = ((e.stateMask & SHIFT_KEY) != 0);
				boolean controlPressed = ((e.stateMask & CONTROL_KEY) != 0);
				if (controlPressed && e.keyCode == 'c') {
					int index = chkRules.getSelectionIndex();
					if (index >= 0) {
						String text;
						if (shiftPressed && itemsInChkRules[index] instanceof Rule && Program.showDevFeatures()) {
				      	Rule selectedRule = (Rule)itemsInChkRules[index];
				      	text = selectedRule.getClass().getSimpleName();
			      	} else {
							text = chkRules.getItem(index).getText();
						} 
						SystemClipboard.setText(text);
					}
				}
			}
		});
		chkRules.addSelectionListener(new SelectionAdapter() {
			private int lastIndex;
			@Override
			public void widgetSelected(SelectionEvent e) {
				// this event handles both moving the selection and (independent from it) checking a rule
				int index = chkRules.getSelectionIndex();
				boolean indexChanged = (index != lastIndex);
				lastIndex = index;
		      if (index >= 0) {
		      	if (itemsInChkRules[index] instanceof Rule) {
			      	Rule selectedRule = (Rule)itemsInChkRules[index];
			      	if (selectedRule != curRule) {
			      		setRule(selectedRule);
			      		return;
			      	}
		      	} else if (indexChanged) {
		      		// do not activate/deactivate the whole RuleGroup if moving on it with cursor keys   
		      		return;
		      	}
		      } 
		      for (int i = 0; i < chkRules.getItemCount(); ++i) {
		      	if (chkRules.getItem(i) == e.item) {
				      chkRulesItemCheck(i);
		      		break;
		      	}
		      }
			}
		});
		
		GridData gd_chkRules = new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1);
		gd_chkRules.minimumWidth = 290;
		chkRules.setLayoutData(gd_chkRules);
		
		Composite pnlRuleListButtons = new Composite(cpsProfilesAndRules, SWT.NONE);
		pnlRuleListButtons.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		pnlRuleListButtons.setLayout(new FillLayout(SWT.HORIZONTAL));
		
		composite_1 = new Composite(pnlRuleListButtons, SWT.NONE);
		GridLayout gl_composite_1 = new GridLayout(1, false);
		gl_composite_1.marginWidth = 0;
		composite_1.setLayout(gl_composite_1);
		
		lblActivate = new Label(composite_1, SWT.NONE);
		lblActivate.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		lblActivate.setText("Activate:");
		
		btnActivateAllRules = new Button(pnlRuleListButtons, SWT.NONE);
		btnActivateAllRules.setToolTipText("Activate all rules in this profile");
		btnActivateAllRules.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (curProfile != null) {
			      curProfile.activateAllRules();
			      refreshRuleActivation();
				}
			}
		});
		btnActivateAllRules.setText("All");
		
		btnActivateDefaultRules = new Button(pnlRuleListButtons, SWT.NONE);
		btnActivateDefaultRules.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (curProfile != null) {
			      curProfile.activateDefaultRulesOnly();
			      refreshRuleActivation();
				}
			}
		});
		btnActivateDefaultRules.setToolTipText("Activate rules that are active by " + Program.PRODUCT_NAME + " default");
		btnActivateDefaultRules.setText("Default");
		
		btnActivateEssentialRules = new Button(pnlRuleListButtons, SWT.NONE);
		btnActivateEssentialRules.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (curProfile != null) {
			      curProfile.activateEssentialRulesOnly();
			      refreshRuleActivation();
				}
			}
		});
		btnActivateEssentialRules.setToolTipText("Only activate rules that are explicitly demanded by the Clean ABAP styleguide");
		btnActivateEssentialRules.setText("Essential");
		
		btnDeactivateAllRules = new Button(pnlRuleListButtons, SWT.NONE);
		btnDeactivateAllRules.setToolTipText("Deactivate all rules in this profile");
		btnDeactivateAllRules.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (curProfile != null) {
			      curProfile.deactivateAllRules();
			      refreshRuleActivation();
				}
			}
		});
		btnDeactivateAllRules.setText("None");

		pnlRule = new Composite(shell, SWT.NONE);
		pnlRule.setLayout(new GridLayout(2, false));
		GridData gd_composite_1 = new GridData(SWT.FILL, SWT.FILL, true, false, 1, 1);
		gd_composite_1.widthHint = 65; 
		pnlRule.setLayoutData(gd_composite_1);
		
		Label lblRule = new Label(pnlRule, SWT.NONE);
		lblRule.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 1, 1));
		lblRule.setBounds(0, 0, 55, 15);
		
		lblRuleName = new Label(pnlRule, SWT.NONE);
		lblRuleName.setFont(SWTResourceManager.getFont("Segoe UI", 12, SWT.BOLD));
		lblRuleName.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, true, false, 1, 1));
		lblRuleName.setText(".");
      
		Label lblRuleDescriptionTitle = new Label(pnlRule, SWT.NONE);
		lblRuleDescriptionTitle.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		lblRuleDescriptionTitle.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 1, 1));
		lblRuleDescriptionTitle.setText("Description:");
		
		pnlRuleDescription = new Composite(pnlRule, SWT.NONE);
		pnlRuleDescription.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		GridLayout gl_pnlRuleDescription = new GridLayout(1, false);
		gl_pnlRuleDescription.marginHeight = 0;
		gl_pnlRuleDescription.verticalSpacing = 2;
		gl_pnlRuleDescription.marginWidth = 0;
		pnlRuleDescription.setLayout(gl_pnlRuleDescription);
		
		lblRuleDescription = new Label(pnlRuleDescription, SWT.NONE);
		lblRuleDescription.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		lblRuleDescription.setText(".");
		
		lblRuleHintsAndRestrictions = new Label(pnlRuleDescription, SWT.NONE);
		lblRuleHintsAndRestrictions.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		lblRuleHintsAndRestrictions.setText(".");

		Label lblRuleReferences = new Label(pnlRule, SWT.NONE);
		lblRuleReferences.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		lblRuleReferences.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 1, 1));
		lblRuleReferences.setText("References:");
		
		pnlRuleReferences = new Composite(pnlRule, SWT.NONE);
		pnlRuleReferences.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		pnlRuleReferences.setBounds(0, 0, 64, 64);
		GridLayout gl_pnlRuleReferences = new GridLayout(2, false);
		gl_pnlRuleReferences.marginHeight = 0;
		gl_pnlRuleReferences.verticalSpacing = 2;
		gl_pnlRuleReferences.marginWidth = 0;
		pnlRuleReferences.setLayout(gl_pnlRuleReferences);
		
		Composite pnlOptionsInfo = new Composite(pnlRule, SWT.NONE);
		GridData gd_pnlOptionsInfo = new GridData(SWT.FILL, SWT.TOP, false, false, 1, 1);
		gd_pnlOptionsInfo.verticalIndent = 4;
		pnlOptionsInfo.setLayoutData(gd_pnlOptionsInfo);
		GridLayout gl_pnlOptionsInfo = new GridLayout(1, false);
		gl_pnlOptionsInfo.marginHeight = 0;
		gl_pnlOptionsInfo.marginWidth = 0;
		pnlOptionsInfo.setLayout(gl_pnlOptionsInfo);
		
		Label lblRuleOptions = new Label(pnlOptionsInfo, SWT.NONE);
		lblRuleOptions.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		lblRuleOptions.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 1, 1));
		lblRuleOptions.setBounds(0, 0, 55, 15);
		lblRuleOptions.setText("Options:");
		
		btnDefaultOptions = new Button(pnlOptionsInfo, SWT.NONE);
		btnDefaultOptions.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnDefaultOptions.setToolTipText("Reset rule options to default settings");
		btnDefaultOptions.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
		      if (configControls != null) {
		         for (ConfigControl configControl : configControls)
		            configControl.setDefault();
		      }
			}
		});
		btnDefaultOptions.setText("Default");

		new Label(pnlOptionsInfo, SWT.NONE);
		new Label(pnlOptionsInfo, SWT.NONE);
		new Label(pnlOptionsInfo, SWT.NONE);
		new Label(pnlOptionsInfo, SWT.NONE);
		new Label(pnlOptionsInfo, SWT.NONE);
	
		pnlRuleOptions = new Composite(pnlRule, SWT.NONE);
		GridLayout gl_pnlRuleOptions = new GridLayout(3, false);
		gl_pnlRuleOptions.marginWidth = 0;
		gl_pnlRuleOptions.verticalSpacing = 3;
		pnlRuleOptions.setLayout(gl_pnlRuleOptions);
		pnlRuleOptions.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false, 1, 1));
		
		pnlExamplesInfo = new Composite(pnlRule, SWT.NONE);
		pnlExamplesInfo.setLayout(new GridLayout(1, false));
		pnlExamplesInfo.setLayoutData(new GridData(SWT.FILL, SWT.TOP, false, false, 1, 1));
		
		lblExamples = new Label(pnlExamplesInfo, SWT.NONE);
		lblExamples.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		lblExamples.setText("Examples:");
		
	   btnPasteExample = new Button(pnlExamplesInfo, SWT.NONE);
	   btnPasteExample.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnPasteExample.setToolTipText("Test current rule on example code from clipboard (default example is not changed)");
		btnPasteExample.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (exampleCodeFromClipboard()) {
					codeDisplay.forceFocus();
				}
			}
		});
		btnPasteExample.setText("&Paste ->");

		Button btnDefaultExample = new Button(pnlExamplesInfo, SWT.NONE);
		btnDefaultExample.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnDefaultExample.setToolTipText("Test current rule on default example code");
		btnDefaultExample.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (curRule != null) {
					refreshExample(curRule, curRule.getExample());
					codeDisplay.forceFocus();
				}
			}
		});
		btnDefaultExample.setText("Default");

		new Label(pnlExamplesInfo, SWT.NONE);

		btnObfuscate = new Button(pnlExamplesInfo, SWT.NONE);
		btnObfuscate.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnObfuscate.setToolTipText("Obfuscate code in left display. Shift+Click to obfuscate each statement individually; Ctrl+Click to get minimal, simplified identifiers and remove all comments.");
		btnObfuscate.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean commandScope = ((e.stateMask & SHIFT_KEY) != 0);
				boolean simplify = ((e.stateMask & CONTROL_KEY) != 0);
				Obfuscator obfuscator = Obfuscator.createFor(codeDisplay.getCodeLanguage(), commandScope, simplify, simplify, simplify, simplify, true);  
				obfuscateExampleCode(obfuscator);
				codeDisplay.forceFocus();
			}
		});
		btnObfuscate.setText("&Obfusc.");

		btnGenerateUnitTestClass = new Button(pnlExamplesInfo, SWT.NONE);
		btnGenerateUnitTestClass.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnGenerateUnitTestClass.setToolTipText("Generate Java code for a new Unit Test class; Ctrl+click for generating default configuration only");
		btnGenerateUnitTestClass.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean showMessage = ((e.stateMask & SHIFT_KEY) == 0);
				boolean defaultConfigOnly = ((e.stateMask & CONTROL_KEY) != 0);
				generateUnitTestClass(defaultConfigOnly, showMessage);
				codeDisplay.forceFocus();
			}
		});
		btnGenerateUnitTestClass.setText("UT Class");

		btnGenerateUnitTest = new Button(pnlExamplesInfo, SWT.NONE);
		btnGenerateUnitTest.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnGenerateUnitTest.setToolTipText("Generate Java code for Unit Test from code selection (left and right display)");
		btnGenerateUnitTest.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean showMessage = ((e.stateMask & SHIFT_KEY) == 0);
				codeDisplay.generateUnitTest(true, showMessage);
				codeDisplay.forceFocus();
			}
		});
		btnGenerateUnitTest.setText("Gen. &UT");

		btnGenerateExample = new Button(pnlExamplesInfo, SWT.NONE);
		btnGenerateExample.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnGenerateExample.setToolTipText("Generate Java code for rule example from code selection (left display); Ctrl+Click to get a full statement.");
		btnGenerateExample.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean showMessage = ((e.stateMask & SHIFT_KEY) == 0);
				boolean fullStatement = ((e.stateMask & CONTROL_KEY) != 0);
				codeDisplay.generateExample(fullStatement, showMessage);
				codeDisplay.forceFocus();
			}
		});
		btnGenerateExample.setText("Gen. &Ex.");

		codeDisplay = new CodeDisplay(pnlRule, SWT.NONE, true);
		codeDisplay.setColors(codeDisplayColors);
		codeDisplay.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
		codeDisplay.setCodeFontSize(CodeDisplay.DEFAULT_FONT_SIZE);
		new Label(pnlRule, SWT.NONE);
		
		Composite cpsHighlightCancelOk = new Composite(pnlRule, SWT.NONE);
		cpsHighlightCancelOk.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		GridLayout gl_cpsHighlightCancelOk = new GridLayout(9, false);
		gl_cpsHighlightCancelOk.marginWidth = 0;
		cpsHighlightCancelOk.setLayout(gl_cpsHighlightCancelOk);
		
		Label lblHighlight = new Label(cpsHighlightCancelOk, SWT.NONE);
		lblHighlight.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.NORMAL));
		lblHighlight.setText("Highlight:");
		
		chkHighlightChanges = new Button(cpsHighlightCancelOk, SWT.CHECK);
		GridData gd_chkHighlightChanges = new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1);
		gd_chkHighlightChanges.horizontalIndent = 5;
		chkHighlightChanges.setLayoutData(gd_chkHighlightChanges);
		chkHighlightChanges.setToolTipText("deactivate this option to appreciate the final look of the code before and after the rule is applied");
		chkHighlightChanges.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
		      ChangeTypes highlight = chkHighlightChanges.getSelection() ? ChangeTypes.createAllChanges() : ChangeTypes.createNoChanges();
		      codeDisplay.setHighlight(highlight);
			}
		});
		chkHighlightChanges.setSelection(true);
		chkHighlightChanges.setText("C&hanges");
		
		chkHighlightDeclarationKeywords = new Button(cpsHighlightCancelOk, SWT.CHECK);
		chkHighlightDeclarationKeywords.setToolTipText("activate this option to highlight declarations");
		if (settings != null)
			chkHighlightDeclarationKeywords.setSelection(settings.profilesHighlightDeclarationKeywords);
		chkHighlightDeclarationKeywords.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean highlight = chkHighlightDeclarationKeywords.getSelection();
				if (settings != null)
					settings.profilesHighlightDeclarationKeywords = highlight; 
				if (codeDisplay != null && !codeDisplay.isDisposed())
					codeDisplay.setHighlightDeclarationKeywords(highlight);
			}
		});
		chkHighlightDeclarationKeywords.setText("&Declarations");
		
		chkHighlightWritePositions = new Button(cpsHighlightCancelOk, SWT.CHECK);
		chkHighlightWritePositions.setToolTipText("activate this option to highlight variables and field-symbols in write positions");
		chkHighlightWritePositions.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		if (settings != null)
			chkHighlightWritePositions.setSelection(settings.profilesHighlightWritePositions);
		chkHighlightWritePositions.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean highlight = chkHighlightWritePositions.getSelection();
				if (settings != null)
					settings.profilesHighlightWritePositions = highlight; 
				if (codeDisplay != null && !codeDisplay.isDisposed()) {
					codeDisplay.setHighlightWritePositions(highlight);
				}
			}
		});
		chkHighlightWritePositions.setText("&Write positions");
		
		// depending on OS, create buttons "Save Profiles and Exit - Cancel" (Windows) or "Cancel - Save Profiles and Exit" (macOS, Linux)
		if (SystemInfo.putOKBeforeCancel()) {
			btnOK = new Button(cpsHighlightCancelOk, SWT.NONE);
			btnCancel = new Button(cpsHighlightCancelOk, SWT.NONE);
		} else {
			btnCancel = new Button(cpsHighlightCancelOk, SWT.NONE);
			btnOK = new Button(cpsHighlightCancelOk, SWT.NONE);
		}

		btnCancel.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				hideForm(false);
			}
		});
		btnCancel.setText("&Cancel");
		
		btnOK.setFont(SWTResourceManager.getFont("Segoe UI", 9, SWT.BOLD));
		btnOK.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				hideForm(true);
			}
		});
		btnOK.setText("&Save Profiles and Exit");
	}
	
   private void setCurrentProfile() {
		int index = lstProfiles.getSelectionIndex(); 
      if (index >= 0 && profiles != null && profiles.size() > index) {
         setProfile(profiles.get(index));
      }
   }
   
   private ProfileHighlightItem getHighlightItem() {
   	if (shownHighlightItems == null) {
   		return null;
   	} else {
   		return shownHighlightItems.getAt(cboHighlight.getSelectionIndex());
   	}
   }
   
   private void setProfile(Profile profile) {
      RuleID lastSelectedRuleID = (curRule == null) ? defaultRuleID : curRule.getID();
      setRule(null);

      curProfile = null; // prevent write-back when controls change
      if (profile == null) 
         return;

      // (de)activate features for profiles from own / read-only directories 
      boolean isWriteable = !profile.isReadOnly;
      btnDeleteProfile.setEnabled(isWriteable);
      btnRenameProfile.setEnabled(isWriteable);
      chkAutoActivateNewFeatures.setEnabled(isWriteable);
      btnActivateAllRules.setEnabled(isWriteable);
      btnActivateDefaultRules.setEnabled(isWriteable);
      btnActivateEssentialRules.setEnabled(isWriteable);
      btnDeactivateAllRules.setEnabled(isWriteable);
      btnDefaultOptions.setEnabled(isWriteable);
      
      chkAutoActivateNewFeatures.setSelection(profile.autoActivateNewFeatures);
      
      ProfileHighlightItem highlightItem = getHighlightItem();
      if (highlighter != null) {
	      highlighter.setControlsHighlight(highlightItem);
      }
      
      // determine item count
      Rule[] rules = sortByGroup ? profile.getRulesSortedByGroup() : profile.getRulesSortedByName();
      String filterTextUpper = txtFilter.getText().toUpperCase();
      if (!StringUtil.isNullOrEmpty(filterTextUpper)) {
      	ArrayList<Rule> filteredRules = new ArrayList<>();
      	for (Rule rule : rules) {
      		if (rule.getDisplayName().toUpperCase().indexOf(filterTextUpper) >= 0) {
      			filteredRules.add(rule);
      		}
      	}
      	rules = new Rule[filteredRules.size()];
      	filteredRules.toArray(rules);
      }
      int ruleCount = rules.length;
      int ruleGroupCount = 0;
   	RuleGroup lastRuleGroup = null;
      if (sortByGroup) { 
         for (Rule rule : rules) {
            if (lastRuleGroup == null || rule.getGroupID() != lastRuleGroup.iD) {
               lastRuleGroup = profile.getRuleGroup(rule.getGroupID());
               ++ruleGroupCount;
            }
         }
      }
		chkRules.setRedraw(false);
      chkRules.removeAll();
      chkRules.setItemCount(ruleCount + ruleGroupCount);
      itemsInChkRules = new Object[ruleCount + ruleGroupCount];

      // build chkRules and itemsInChkRules
      int itemIndex = 0;
      TableItem curItem = null;
      lastRuleGroup = null;
      for (Rule rule : rules) {
         if (sortByGroup && (lastRuleGroup == null || rule.getGroupID() != lastRuleGroup.iD)) {
            lastRuleGroup = profile.getRuleGroup(rule.getGroupID());
            curItem = chkRules.getItem(itemIndex);
            curItem.setText(lastRuleGroup.toString());
            curItem.setChecked(lastRuleGroup.isAnyRuleActive());
            itemsInChkRules[itemIndex] = lastRuleGroup;
            ++itemIndex;
         }
         curItem = chkRules.getItem(itemIndex);
         curItem.setText(rule.getDisplayName());
         curItem.setChecked(rule.isActive);
         if (highlighter != null) {
         	curItem.setBackground(highlighter.getRuleBackground(rule, highlightItem, true));
         }
         itemsInChkRules[itemIndex] = rule;
         ++itemIndex;
      }

      curProfile = profile;
		resultProfileName = profile.name;
      refreshActiveRuleCount();

      if (chkRules.getItemCount() > 0) {
         int index = findRuleIndex(lastSelectedRuleID);
         int setIndex = (index >= 0) ? index : (sortByGroup ? 1 : 0); // index 0 contains a RuleGroup, therefore use index 1 
         chkRules.select(setIndex);
         chkRules.showItem(chkRules.getItem(setIndex));
         if (setIndex >= 0 && itemsInChkRules[setIndex] instanceof Rule)
         	setRule((Rule)itemsInChkRules[setIndex]);
      } else {
      	codeDisplay.clear();
      }
		chkRules.setRedraw(true);
   }

   private int findRuleIndex(RuleID ruleID) {
   	if (ruleID == null)
   		return -1;
      for (int i = 0; i < itemsInChkRules.length; ++i) {
         if (itemsInChkRules[i] instanceof Rule) {
	         Rule rule = (Rule)itemsInChkRules[i];
	         if (rule.getID() == ruleID)
	            return i;
         }
      }
      return -1;
   }

   private int findRuleGroupIndex(RuleGroupID ruleGroupID) {
      for (int i = 0; i < itemsInChkRules.length; ++i) {
         if (itemsInChkRules[i] instanceof RuleGroup) {
	         RuleGroup ruleGroup = (RuleGroup)itemsInChkRules[i];
	         if (ruleGroup.iD == ruleGroupID)
	            return i;
			}
      }
      return -1;
   }

   private void setRule(Rule rule) {
      curRule = null; // prevent write-back when controls change

      disposeRuleReferenceControls();
      disposeConfigControls();

      lblRuleName.setText((rule != null) ? StringUtil.getLabelText(rule.getDisplayName()) : "");
      if (highlighter != null) {
      	highlighter.setRuleNameHighlight(rule);
      }
      
      lblRuleDescription.setText((rule != null) ? StringUtil.getLabelText(rule.getDescription()) : "");
      lblRuleHintsAndRestrictions.setText((rule != null) ? StringUtil.getLabelText(rule.getHintsAndRestrictions()) : "");

      createRuleReferenceControls(rule);

      String exampleCode = (rule == null) ? null : rule.getExample(); 
      refreshExample(rule, exampleCode);

      createConfigControls(rule);

      curRule = rule;
      if (rule != null) {
      	settings.profilesLastRuleID = rule.getID();
      }
   }

   private void disposeRuleReferenceControls() {
      // dispose of the ruleReferenceControls of the previous rule
      if (!ruleReferenceLabels.isEmpty()) {
         for (RuleReferenceLabel ruleReferenceLabel : ruleReferenceLabels) 
         	ruleReferenceLabel.detachAndDispose();
         ruleReferenceLabels.clear();
      }
   }

   private void createRuleReferenceControls(Rule rule) {
   	final int xGap = 8;
   	
      if (rule == null || rule.getReferences() == null)
      	return;

      pnlRuleReferences.setRedraw(false);
      
      int maxRight = 0;
      for (RuleReference reference : rule.getReferences()) {
         String source = "";
         String chapterTitle = reference.chapterTitle;
         if (!StringUtil.isNullOrEmpty(reference.getSourceText())) { 
         	source = reference.getSourceText();
         	if (!StringUtil.isNullOrEmpty(chapterTitle)) {
         		source += ":";
         	}
         }
         RuleReferenceLabel ruleReferenceLabel = new RuleReferenceLabel((IConfigDisplay)this, pnlRuleReferences, source, chapterTitle, reference.getLink());
         ruleReferenceLabels.add(ruleReferenceLabel);

         if (highlighter != null) {
         	highlighter.setRuleChaptersStyle(ruleReferenceLabel.lblRuleChapter, reference.hasLink());
            // TODO: set mouse cursor when hovering over lblRuleChapter - this may only be possible with StyledText, which apparently does this automatically for SWT.UNDERLINE_LINK
         }

         maxRight = Math.max(maxRight, ruleReferenceLabel.getSourceRight());
      }

      // align the chapter titles 
      int chapterLeft = maxRight + xGap;
      for (RuleReferenceLabel ruleReferenceLabel : ruleReferenceLabels) {
      	ruleReferenceLabel.setChapterLeft(chapterLeft);
         ruleReferenceLabel.setVisible();
      }

      pnlRuleReferences.layout();      
      pnlRuleReferences.setRedraw(true);
   }

   private void disposeConfigControls() {
      // dispose of the configControls of the previous rule
      if (!configControls.isEmpty()) {
         for (ConfigControl configControl : configControls)
            configControl.detachAndDispose();
         configControls.clear();
      }
   }

   private void createConfigControls(Rule rule) {
      ConfigValue[] configValues = (rule == null) ? null : rule.getConfigValues();
      btnDefaultOptions.setVisible(configValues != null);
      if (configValues == null) {
      	return;
      }

      pnlRuleOptions.setRedraw(false);

      GridLayout gl_pnlRuleOptions = new GridLayout(3, false);
		gl_pnlRuleOptions.marginWidth = 0;
		gl_pnlRuleOptions.verticalSpacing = 3;
		pnlRuleOptions.setLayout(gl_pnlRuleOptions);

		boolean isProfileWritable = !rule.parentProfile.isReadOnly;
		
      for (ConfigValue configValue : configValues) {
         ConfigControl configControl;
         if (configValue instanceof ConfigBoolValue) {
         	ConfigBoolValue configBoolValue = (ConfigBoolValue)configValue;
            configControl = new ConfigCheckBox(configBoolValue, (IConfigDisplay)this, pnlRuleOptions);

         } else if (configValue instanceof ConfigIntValue) {
            ConfigIntValue configIntValue = (ConfigIntValue)configValue;
            configControl = new ConfigIntBox(configIntValue, (IConfigDisplay)this, pnlRuleOptions);
         
         } else if (configValue instanceof ConfigTextValue) {
            ConfigTextValue configTextValue = (ConfigTextValue)configValue;
            configControl = new ConfigTextBox(configTextValue, (IConfigDisplay)this, pnlRuleOptions);
      	
         } else if (configValue instanceof ConfigSelectionValue) {
            ConfigSelectionValue configSelectionValue = (ConfigSelectionValue)configValue;
            configControl = new ConfigComboBox(configSelectionValue, (IConfigDisplay)this, pnlRuleOptions);
         
         } else if (configValue instanceof ConfigInfoValue) {
            ConfigInfoValue configInfoValue = (ConfigInfoValue)configValue;
            configControl = new ConfigLabel(configInfoValue, (IConfigDisplay)this, pnlRuleOptions);
         
         } else {
           throw new IndexOutOfBoundsException("unknown ConfigControl");
         }
      	configControl.setEnabled(isProfileWritable, rule.isConfigValueEnabled(configValue));
         configControls.add(configControl);
         Control[] controls = configControl.getControls();
         for (int column = 0; column < controls.length; ++column) { 
            controls[column].setVisible(true);
         }
      }
      
      pnlRuleOptions.layout();
      pnlRuleOptions.setRedraw(true);
      
      if (highlighter != null) {
	      // set the highlight only after .setRedraw(true), otherwise, if dark theme is used in ADT, 
	      // .getBackground() will still return light theme colors (see ConfigControl.setHighlighted())! 
	      for (ConfigControl configControl : configControls) {
	      	highlighter.setConfigHighlight(rule, configControl);
	      }
      }

      pnlRule.layout();
   }

	private boolean refreshExample(Rule rule, String newExampleCode) {
   	// refresh the example code if it is provided
   	int setPosition = -1; // keep current position
   	if (!StringUtil.isNullOrEmpty(newExampleCode)) {
   		curExampleCode = newExampleCode;
   		setPosition = 0; // reset to start position
   	}
   	
      if (rule == null || StringUtil.isNullOrEmpty(curExampleCode))
         return false;

      Job job = Job.createForRuleExample(rule.getDisplayName(), curExampleCode, rule);
      Task result = job.run();
      if (result.getSuccess()) {
         codeDisplay.setInfo(rule.getDisplayName() + " - example", "", curExampleCode, ABAP.NEWEST_RELEASE, rule);
         codeDisplay.refreshCode(result.getResultingCode(), result.getResultingDiffDoc(), rule.parentProfile, setPosition, setPosition, setPosition);
         return true;
      } else {
         Message.show(result.getErrorMessage(), shell);
         return false;
      }
   }

   private void refreshRuleActivation() {
	   ++suspendItemCheck;
	   for (int i = 0; i < chkRules.getItemCount(); ++i) {
         if (itemsInChkRules[i] instanceof Rule) {
         	boolean isActive = ((Rule)itemsInChkRules[i]).isActive;
         	chkRules.getItem(i).setChecked(isActive);
         } else if (itemsInChkRules[i] instanceof RuleGroup) {
		      boolean isActive = ((RuleGroup)itemsInChkRules[i]).isAnyRuleActive();
		      chkRules.getItem(i).setChecked(isActive);
         }
	   }
	   --suspendItemCheck;
	   refreshActiveRuleCount();
   }

   private void refreshActiveRuleCount() {
   	if (curProfile != null) {
	      int ruleCount = curProfile.getRuleCount();
	      int activeRuleCount = curProfile.getActiveRuleCount();
	      lblRules.setText("Rules in Current Profile: " + Cult.format(activeRuleCount) + " / " + Cult.format(ruleCount) + " Active");
   	}
   }

   private Profile findProfile(String name) {
      for (Profile profile : profiles) {
         if (profile.name.equalsIgnoreCase(name))
            return profile;
      }
      return null;
   }

   private void refreshProfileList(String setToProfileName) {
      Collections.sort(profiles, new Profile.Comparer());

		// suspend redraw
      lstProfiles.setRedraw(false);
      cboHighlight.setRedraw(false);

      // build cboHighlight, which includes a list of all profiles (with which the current profile can be compared)
		ProfileHighlightItems highlightItems = new ProfileHighlightItems(profiles, Program.getReleases());
		cboHighlight.removeAll();
		for (ProfileHighlightItem highlightItem : highlightItems.getItems()) {
			cboHighlight.add(highlightItem.toString());
		}
		if (cboHighlight.getItemCount() > 0) {
			int highlightIndex = highlightItems.findIndex(settings.profilesHighlightItem);
			if (highlightIndex < 0 || highlightIndex >= cboHighlight.getItemCount()) {
				// pre-select "Features added after <previous release>"
				int indexOfPrevRelease = ProfileHighlightItems.indexOfPreviousRelease();
				highlightIndex = (indexOfPrevRelease < cboHighlight.getItemCount()) ? indexOfPrevRelease : 0;
			}
			cboHighlight.select(highlightIndex); 
		}
		shownHighlightItems = highlightItems;

      // build lstProfiles - setProfile() must happen after building cboHighlight
      int setToIndex = 0; // default to first entry if setToProfileName is not found
      lstProfiles.removeAll();
      for (Profile profile : profiles) {
      	if (profile.name.equals(setToProfileName))
      		setToIndex = lstProfiles.getItemCount();
      	lstProfiles.add(profile.name);
      }
      if (profiles.size() > 0) {
      	lstProfiles.select(setToIndex);
         setProfile(profiles.get(setToIndex));
      }
      
		// resume redraw
      lstProfiles.setRedraw(true);
      cboHighlight.setRedraw(true);
   }

   public final void remove(Control[] controls) {
   	// nothing to do here, this will automatically be done when the Controls are disposed of
   }

   public final void configurationChanged() {
   	// refresh the example without changing the example code (which may have been changed from the clipboard)
      refreshExample(curRule, null);
      
      if (curRule != null && configControls != null) {
	      for (ConfigControl configControl : configControls) {
	      	configControl.setEnabled(!curRule.parentProfile.isReadOnly, curRule.isConfigValueEnabled(configControl.getConfigValue()));
	      }
      }
      
      refreshHighlight(true);
   }

   public final void buttonClicked(ConfigValue configValue) {
      if (curRule != null && configControls != null) {
      	String path = curRule.buttonClicked(configValue);
      	if (!StringUtil.isNullOrEmpty(path)) {
      		ProgramLauncher.startProcess(path);
      	}
      }
   }

   private void refreshHighlight(boolean curRuleOnly) {
   	// calls highlighter function as done by setProfile() and setRule(), but without everything else
   	// if curRuleOnly == true, then only update highlighting that is affected by changed configuration 
   	
   	if (curProfile == null || highlighter == null)
   		return;

   	ProfileHighlightItem highlightItem = getHighlightItem();
   	
   	// general (i.e. non-rule-specific) UI features (buttons, options, labels)
   	if (!curRuleOnly) {
   		highlighter.setControlsHighlight(highlightItem);
   	}
   	
   	// list of rules
      for (int i = 0; i < chkRules.getItemCount(); ++i) {
      	if (itemsInChkRules[i] instanceof Rule) {
      		Rule rule = (Rule)itemsInChkRules[i];
      		if (rule == curRule || !curRuleOnly) {
	         	highlighter.setRuleHighlight(rule, i, highlightItem);
      		}
      	}
      }

      if (curRule != null) {
      	// rule name
      	highlighter.setRuleNameHighlight(curRule);

      	// rule references
      	if (!curRuleOnly) {
	      	RuleReference[] references = curRule.getReferences();
	      	if (references != null) {
		         for (int i = 0; i < references.length; ++i) {
		         	if (i < ruleReferenceLabels.size()) {
		            	highlighter.setRuleChaptersStyle(ruleReferenceLabels.get(i).lblRuleChapter, references[i].hasLink());
		            }
		         }
	      	}
      	}

      	// rule configuration
      	if (configControls != null) {
		      for (ConfigControl configControl : configControls) {
		      	highlighter.setConfigHighlight(curRule, configControl);
		      }
         }
	   }
   }
   

   private void createProfile() {
   	String newName = inputNewProfileName("");
      if (!StringUtil.isNullOrEmpty(newName)) {
	      profiles.add(Profile.create(newName));
	      refreshProfileList(newName);
      }
   }

   private void copyProfile() {
      String oldName = (curProfile == null) ? "" : curProfile.getNameWithoutPrefix();
   	String newName = inputNewProfileName(oldName);
      if (!StringUtil.isNullOrEmpty(newName)) {
	      profiles.add(Profile.createFromModel(newName, curProfile));
	      refreshProfileList(newName);
      }
   }

   private void deleteProfile() {
      if (curProfile == null || profiles.size() <= 1)
         return;
      if (Message.show("Are you sure you want to delete the profile '" + curProfile.name + "'?", "Delete profile?", SWT.YES | SWT.NO | SWT.CANCEL | SWT.ICON_QUESTION, shell) != SWT.YES)
         return;
      profiles.remove(curProfile);
      int index = lstProfiles.getSelectionIndex();
      Profile selectProfile = null;
      if (index < profiles.size())
         selectProfile = profiles.get(index);
      else if (index > 0)
         selectProfile = profiles.get(index - 1);
      refreshProfileList((selectProfile == null) ? null : selectProfile.name);
   }

   private void renameProfile() {
   	String newName = inputNewProfileName(curProfile.name);
      if (!StringUtil.isNullOrEmpty(newName)) {
	      if (settings.profilesHighlightItem.equals(ProfileHighlightItem.getPersistentStringOfProfile(curProfile.name))) {
	      	settings.profilesHighlightItem = ProfileHighlightItem.getPersistentStringOfProfile(newName);
	      }
	      curProfile.name = newName;
	      refreshProfileList(newName);
      }
   }

   private String inputNewProfileName(String suggestedName) {
      FrmInputBox inputBox = new FrmInputBox();
      String newName = inputBox.open(suggestedName, "New profile name:", true, shell);
      if (findProfile(newName) != null) {
         Message.show("There already is a profile by the name of '" + newName + "'!", shell);
         return null;
      } else {
      	return newName;
      }
   }

   private void importProfiles() {
		Persistency persistency = Persistency.get();
		String extension = persistency.getExtension(FileType.PROFILE_TEXT);

		// prepare the dialog
		FileDialog dialog = new FileDialog(shell, SWT.OPEN | SWT.MULTI);
		dialog.setText("Select Profile(s) to Import");
		String filterPath;
		if (StringUtil.isNullOrEmpty(settings.profilesLastImportDir)) {
			filterPath = System.getProperty("user.home");
		} else {
			filterPath = settings.profilesLastImportDir;
		}
		dialog.setFilterPath(filterPath);
		dialog.setFilterExtensions(new String[] { "*" + extension, "*.*" });
		dialog.setFilterNames(new String[] { "Profiles", "All Files" });
		dialog.setFilterIndex(0);
		
		// display file dialog for selection of one of multiple profiles
		String firstPath = dialog.open();
		if (StringUtil.isNullOrEmpty(firstPath))
			return;
		String[] files = dialog.getFileNames();
		if (files == null || files.length == 0)
			return;

		String dir = persistency.getDirectoryName(firstPath);
		settings.profilesLastImportDir = dir;

		// create list of to-be-imported profile names
		HashSet<String> importNames = new HashSet<>();
		for (String file : files) {
			String name = StringUtil.removeSuffix(file, extension, true);
			importNames.add(name.toUpperCase());
		}

		// determine profiles that would be overwritten
		String curProfileName = (curProfile == null) ? null : curProfile.name;
		StringBuilder sbExisting = new StringBuilder();
		int existingCount = 0;
		for (Profile profile : profiles) {
			if (importNames.contains(profile.name.toUpperCase())) {
				if (sbExisting.length() > 0) {
					sbExisting.append(", ");
				}
				sbExisting.append(profile.name);
				++existingCount;
			}
		}
		
		// ask whether profiles shall be overwritten
		if (existingCount > 0) {
			String title = (existingCount == 1) ? "Overwrite profile?" : "Overwrite " + Cult.format(existingCount) + " profiles?";
			String msg = (existingCount == 1) ? "Overwrite existing profile" : "Overwrite " + Cult.format(existingCount) + " existing profiles";
			if (existingCount == 1) {
				title = "Overwrite profile?";
				msg = "Overwrite existing profile '" + sbExisting.toString() + "'?";
			} else {
				title = "Overwrite " + Cult.format(existingCount) + " profiles?";
				msg = "Overwrite " + Cult.format(existingCount) + " existing profiles '" + sbExisting.toString() + "'?";
			}
			if (Message.show(msg, title, SWT.YES | SWT.NO | SWT.CANCEL | SWT.ICON_QUESTION, shell) != SWT.YES) {
				return;
			}
		}

		// import profile(s)
		int importCount = 0;
		String firstImportedProfileName = null;
		boolean wasCurrentProfileReplaced = false;
		for (String importFile : files) {
			String importPath = persistency.combinePaths(dir, importFile);
			String importName = StringUtil.removeSuffix(importFile, extension, true);
			
			Profile importedProfile = null;
			try (ISettingsReader reader = TextSettingsReader.createFromFile(persistency, importPath, Program.TECHNICAL_VERSION)) {
				importedProfile = Profile.createFromSettings(reader, "");
			} catch (IOException ex) {
	   		Message.show(ex.getMessage(), shell);
				continue;
			}

			// remove existing profile of the same name (if any), adding the imported profile in its place
			boolean found = false;
			for (int index = 0; index < profiles.size(); ++ index) {
				if (profiles.get(index).name.equalsIgnoreCase(importName)) {
					found = true;
					profiles.remove(index);
					profiles.add(index, importedProfile);
					break;
				}
			}
			if (!found) {
				profiles.add(importedProfile);
			}
			
			if (firstImportedProfileName == null) {
				firstImportedProfileName = importedProfile.name;
			}
			if (curProfileName != null && importedProfile.name.equalsIgnoreCase(curProfileName))
				wasCurrentProfileReplaced = true;
			++importCount;
		}

		// refresh profile list
		String selectName = firstImportedProfileName; // may be null
		if (wasCurrentProfileReplaced || (selectName == null && curProfileName != null)) 
			selectName = curProfileName;
		refreshProfileList(selectName);

		// show success message (error messages were already displayed above)
		if (importCount > 0) {
			String title;
			String result; 
			if (importCount == 1) {
				title = "Import Profile";
				result = "Profile '" + firstImportedProfileName + "' was imported.";
			} else {
				title = "Import Profiles";
				result = Cult.format(importCount) + " profiles were imported.";
			}
			result += System.lineSeparator() + System.lineSeparator() + "In order to persist the changes, close the window with button '" + btnOK.getText().replace("&", "") + "' when done.";
			Message.show(result, title, shell);
		}
   }

   private void exportProfiles(Profile[] profiles) {
   	// prepare dialog
		DirectoryDialog dirDialog = new DirectoryDialog(shell);
		String filterPath;
		if (StringUtil.isNullOrEmpty(settings.profilesLastExportDir)) {
			filterPath = System.getProperty("user.home");
		} else {
			filterPath = settings.profilesLastExportDir;
		}
		dirDialog.setFilterPath(filterPath);
		String text;
		if (profiles.length == 1)
			text = "Select Destination Folder for Profile '" + profiles[0].getNameWithoutPrefix() + "'";
		else
			text = "Select Destination Folder for " + Cult.format(profiles.length) + " Profiles";
		dirDialog.setText(text);

   	// display dialog for selection of destination folder
		String dir = dirDialog.open();
		if (dir == null || dir.length() == 0)
			return;

		settings.profilesLastExportDir = dir;
		
		// determine destination path
		Persistency persistency = Persistency.get();
		dir = persistency.addDirSep(dir);
		String extension = persistency.getExtension(FileType.PROFILE_TEXT);

		// determine files that would be overwritten
		StringBuilder sbExisting = new StringBuilder();
		int existingCount = 0;
		for (Profile profile : profiles) {
			String name = profile.getNameWithoutPrefix();
			String path = persistency.combinePaths(dir, name + extension);
			if (persistency.fileExists(path)) {
				if (sbExisting.length() > 0)
					sbExisting.append(", ");
				sbExisting.append(name);
				++existingCount;
			}
		}

		// ask whether files shall be overwritten
		if (existingCount > 0) {
			String msg;
			String title;
			if (existingCount == 1) {
				title = "Overwrite file?";
				msg = "Overwrite existing file '" + sbExisting.toString() + "'?";
			} else {
				title = "Overwrite " + Cult.format(existingCount) + " files?";
				msg = "Overwrite " + Cult.format(existingCount) + " existing files '" + sbExisting.toString() + "'?";
			}
			if (Message.show(msg, title, SWT.YES | SWT.NO | SWT.CANCEL | SWT.ICON_QUESTION, shell) != SWT.YES) {
				return;
			}
		}
		

		// export profile(s)
		String firstSuccessProfileName = null;
		int exportCount = 0;
		for (Profile profile : profiles) {
			String name = profile.getNameWithoutPrefix();
			String path = persistency.combinePaths(dir, name + extension);
			try (ISettingsWriter writer = TextSettingsWriter.createForFile(persistency, path, Program.TECHNICAL_VERSION, Profile.REQUIRED_VERSION)) {
	   		profile.save(writer);
	   		++exportCount;
	   		if (exportCount == 1) {
	   			firstSuccessProfileName = name;
	   		}
	   	} catch (IOException ex) {
	   		Message.show(ex.getMessage(), shell);
	   	}
		} 

		// show success message (error messages were already displayed above)
		if (exportCount > 0) {
			String title;
			String result; 
			if (exportCount == 1) {
				title = "Export Profile";
				result = "Profile '" + firstSuccessProfileName + "' was exported.";
			} else {
				title = "Export Profiles";
				result = Cult.format(exportCount) + " profiles were exported.";
			}
			Message.show(result, title, shell);
		}
   }

   private void chkRulesItemCheck(int index) {
      if (suspendItemCheck > 0)
         return;

      boolean profileIsReadOnly = (curProfile != null) && curProfile.isReadOnly;
      
      ++suspendItemCheck;
      try {
         boolean activate = chkRules.getItem(index).getChecked(); 
	      ProfileHighlightItem highlightItem = getHighlightItem();
         if (itemsInChkRules[index] instanceof Rule) {
         	Rule rule = (Rule)itemsInChkRules[index];

         	if (profileIsReadOnly) {
            	// revert any (un)checking if the profile is read-only
         		if (activate != rule.isActive) {
         			Message.show("Profile '" + curProfile.name + "' is read-only!\r\nYou can create a copy of it in your own profile folder in order to change its configuration.", shell);
         			chkRules.getItem(index).setChecked(rule.isActive);
         			chkRules.redraw();
         		}
         	} else { 
	         	rule.isActive = activate;
	            if (highlighter != null) {
	            	highlighter.setRuleHighlight(rule, index, highlightItem);
	            }
	            updateRuleGroupChecked(rule.getGroupID());
         	}
         } else {
	         RuleGroup group = (RuleGroup)itemsInChkRules[index];

         	if (profileIsReadOnly) {
   	         // revert any (un)checking if the profile is read-only
	            updateRuleGroupChecked(group.iD);
         	} else {
		      	// check or uncheck all visible(!) rules in this group 
		         for (int i = 0; i < chkRules.getItemCount(); ++i) {
		            if (itemsInChkRules[i] instanceof Rule) {
		            	Rule rule = (Rule)itemsInChkRules[i];
		               if (rule.getGroupID() == group.iD && rule.isActive != activate) {
		                  rule.isActive = activate;
		                  if (highlighter != null) {
		                  	highlighter.setRuleHighlight(rule, i, highlightItem);
		                  }
		                  chkRules.getItem(i).setChecked(activate);
		               }
		            }
		         }
         	}
         }
      } catch (java.lang.Exception ex) {
      }
      --suspendItemCheck;
      refreshActiveRuleCount();
   }

   private void updateRuleGroupChecked(RuleGroupID ruleGroupID) {
      int groupIndex = findRuleGroupIndex(ruleGroupID);
      if (groupIndex >= 0 && groupIndex < itemsInChkRules.length) {
      	// determine whether any of the visible(!) rules of this group is active
         RuleGroup group = (RuleGroup)itemsInChkRules[groupIndex];
         boolean isAnyRuleInGroupActive = false; // group.isAnyRuleActive() may include filtered-out rules
         for (int i = 0; i < chkRules.getItemCount(); ++i) {
            if (itemsInChkRules[i] instanceof Rule) {
            	Rule ruleInGroup = (Rule)itemsInChkRules[i];
               if (ruleInGroup.getGroupID() == group.iD && ruleInGroup.isActive) {
               	isAnyRuleInGroupActive = true;
               	break;
               }
            }
         }
         // check the group item accordingly
         TableItem groupItem = chkRules.getItem(groupIndex);
         if (groupItem.getChecked() != isAnyRuleInGroupActive) {
         	groupItem.setChecked(isAnyRuleInGroupActive);
         }
      }
   }
   
   private void hideForm(boolean resultSave) {
   	this.resultSave = resultSave;
		
		setRule(null); // dispose of configControls
      setProfile(null);

      shell.dispose();
   }

	@Override
	public void keyPressedInCodeDisplay(KeyEvent e) {
		if (e.keyCode == SWT.F1) {
			ProgramLauncher.showHelp(HelpTopic.PROFILES);
			e.doit = false;
		} else if (((e.stateMask & CONTROL_KEY) != 0) && e.keyCode == 'v') {
			exampleCodeFromClipboard();
			e.doit = false;	
		}
	}

	private boolean exampleCodeFromClipboard() {
		if (!SystemClipboard.containsText()) {
			Message.show("The clipboard is empty!", shell);
			return false;
		} else if (curRule == null) {
			Message.show("Please select a rule first!", shell);
			return false;
		}

		String code = SystemClipboard.getText();
		if (Program.showDevFeatures()) {
			// simplify pasting code from unit tests
			if ((code.contains("\t\tbuildSrc(\"") || code.contains("\t\tbuildExp(\"")) && code.contains("\");")) {
				code = code.replace("\t\tbuildSrc(\"", "").replace("\t\tbuildExp(\"", "").replace("\");", "");
				code = StringUtil.getUnescapedText(code);
			}
		}

		boolean parseSuccess = refreshExample(curRule, code);
      if (!parseSuccess)
      	refreshExample(curRule, curRule.getExample());
		return true;
	}

   private void changeProfileDirs() {
   	String curProfileName = (curProfile == null) ? null : curProfile.name;
   	
   	new FrmProfileDirs(settings).open();

		// after returning from FrmProfileDirs, ensure FrmProfiles appears in the foreground, 
		// even if another window was meanwhile activated and is higher in the Z order
		if (!shell.getMinimized()) {
			shell.forceActive();
		}

		// update the profiles from read-only directories (all other would have been moved along with the own profile directory) 
		Profile.updateReadOnlyProfiles(profiles, settings.readOnlyProfileDirs, null);
		refreshProfileList(curProfileName);
   }
   
   private void obfuscateExampleCode(Obfuscator obfuscator) {
   	Code code;
   	try {
   		code = obfuscator.obfuscate(curExampleCode);
			obfuscator.obfuscate(code);
		} catch (UnexpectedSyntaxAfterChanges | ParseException e) {
			Message.show(e.getMessage(), shell);
			return;
		}
   	
		boolean parseSuccess = refreshExample(curRule, code.toString());
      if (!parseSuccess) {
      	refreshExample(curRule, curRule.getExample());
      }
   }

   /** builds the Java code for a Unit Test from the currently selected code */
	private void generateUnitTestClass(boolean defaultConfigOnly, boolean showMessage) {
		final String LINE_SEP = ABAP.LINE_SEPARATOR;
		final String TAB = "\t";

		if (curRule == null) {
			Message.show("Please select a rule first!", shell);
			return;
		}

		// determine whether this rule has any configuration for the setUp() method
		ConfigValue[] configValues = curRule.getConfigValues();
		boolean hasConfigValues = false;
		if (configValues != null) {
			for (ConfigValue configValue : configValues) {
				if (!(configValue instanceof ConfigInfoValue)) {
					hasConfigValues = true;
					break;
				}
			}
		}
		if (defaultConfigOnly && !hasConfigValues) {
			Message.show("This rule does not have any configuration!", shell);
			return;
		}

		StringBuilder sb = new StringBuilder();
		String ruleClassName = curRule.getClass().getSimpleName();
		String testClassName = StringUtil.removeSuffix(ruleClassName, "Rule", true) + "Test";

		if (!defaultConfigOnly) {
			sb.append("package " + curRule.getClass().getPackageName() + ";" + LINE_SEP);
			sb.append(LINE_SEP);
		
			// imports
			if (hasConfigValues)
				sb.append("import org.junit.jupiter.api.BeforeEach;").append(LINE_SEP);
			sb.append("import org.junit.jupiter.api.Test;").append(LINE_SEP);
			sb.append(LINE_SEP);
			sb.append("import com.sap.adt.abapcleaner.rulebase.RuleID;").append(LINE_SEP);
			sb.append("import com.sap.adt.abapcleaner.rulebase.RuleTestBase;").append(LINE_SEP);
			sb.append(LINE_SEP);
			
			// class with rule attribute
			sb.append("class " + testClassName + " extends RuleTestBase {").append(LINE_SEP);
			if (hasConfigValues) {
				sb.append(TAB + "private " + ruleClassName + " rule;").append(LINE_SEP);
				sb.append(TAB + LINE_SEP);
			}

			// constructor
			sb.append(TAB + testClassName + "() {").append(LINE_SEP);
			sb.append(TAB + TAB + "super(RuleID." + curRule.getID().name() + ");").append(LINE_SEP);
			if (hasConfigValues) {
				sb.append(TAB + TAB + "rule = (" + ruleClassName + ")getRule();").append(LINE_SEP);
			}
			sb.append(TAB + "}").append(LINE_SEP);
			sb.append(TAB + LINE_SEP);

			// begin of setUp() method
			if (hasConfigValues) {
				sb.append(TAB + "@BeforeEach").append(LINE_SEP);
				sb.append(TAB + "void setUp() {").append(LINE_SEP);
			}
		}

		// default configuration
		sb.append(TAB + TAB + "// setup default test configuration (may be modified in the individual test methods)").append(LINE_SEP);
		for (ConfigValue configValue : configValues) {
			if (configValue instanceof ConfigInfoValue)
				continue;
			sb.append(TAB + TAB + "rule.config" + configValue.settingName);
			sb.append((configValue instanceof ConfigSelectionValue) ? ".setEnumValue(" : ".setValue(");
			sb.append(configValue.getDefaultValueAsCode() + ");").append(LINE_SEP);
		}

		if (!defaultConfigOnly) {
			if (hasConfigValues) {
				// end of setUp() method
				sb.append(TAB + "}").append(LINE_SEP);
				sb.append(TAB + LINE_SEP);
			}

			// sample test method
			sb.append(TAB + "@Test").append(LINE_SEP);
			sb.append(TAB + "void test() {").append(LINE_SEP);
			sb.append(TAB + TAB + "// TODO").append(LINE_SEP);
			sb.append(TAB + "}").append(LINE_SEP);
			sb.append("}").append(LINE_SEP);
		}
		
		// copy the generated code to the clipboard and show a success message
		SystemClipboard.setText(sb.toString());
		if (showMessage) {
			String message = "The generated code for the unit test class '" + testClassName + "' was copied to the clipboard.";
			Message.show(message, "Generate Unit Test Class from Code Selection", shell);
		}
	
	}
}
