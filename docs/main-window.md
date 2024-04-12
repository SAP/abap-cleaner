# Main Window

When cleaning ABAP code interactively with the ABAP cleaner UI,
you may navigate, search, and copy the cleaned code as follows:

![ABAP cleaner interactive cleanup - main window](images/main-window.png "ABAP cleaner interactive cleanup - main window")

## Keyboard Shortcuts

### Navigation

Key                     | Effect
----------------------- | --------------------------------------------------------------------------------------
_Up_, _Down_            | normal navigation
_PageUp_, _PageDown_    | normal navigation
_Ctrl + Home / End_     | move to start / end of code 
_Home_ / _End_          | navigate to first / last line on the current screen
_Shift + Up / Down_     | select lines
_Shift_ + click etc.    | select lines
_Left_ / _Right_        | navigate to previous/next finding (considering current highlight settings)
_Ctrl + Left_ / _Right_ | navigate to previous/next page with findings (considering current highlight settings)
_Ctrl + Up_ / _Down_    | scroll without changing selection
_type any number_       | navigate to a line number of the original code (shown on the left-hand side)
_a ... z_               | activate/deactivate item in list 'Rules Used in Current Selection'

### Incremental Search

Key                    | Effect
---------------------- | -------------------------------------------------------------------
_Ctrl + F_             | start search mode; afterwards, simply start typing the search text
_F3_ / _Enter_         | navigate to next finding
_Shift + F3_ / _Enter_ | navigate to previous finding
_Esc_                  | exit search mode

### Copy & Paste Code

Key                | Effect
------------------ | ------------------------------------------------------------------------
_Ctrl + V_         | paste code from clipboard and immediately process it with ABAP cleaner (stand-alone App only)
_Ctrl + A_         | select all code
_Ctrl + C_         | copy selected lines of cleaned code (right-hand side) to the clipboard
_Ctrl + Shift + C_ | copy selected lines of original code (left-hand side) to the clipboard

The Copy operations will always copy complete statements, 
even if only the first or last few lines of a multi-line statement are selected in the display. 

## Cleanup Settings

The 'Cleanup Settings' allow you to select a profile, a default cleanup range, and a restriction to an ABAP release 
(see below). These settings are immediately applied to the code shown in interactive cleanup. 

![ABAP cleaner main window - cleanup settings](images/main-window_cleanup-settings.png "ABAP cleaner main window - cleanup settings")

Additionally, these settings are saved and used for all subsequent _automated_ cleanups (shortcut _Ctrl + 4_), 
in which the interactive ABAP cleaner UI is not shown. Therefore, even if you mainly use the automated cleanup, 
you may occasionally open the interactive ABAP cleaner UI to change these cleanup settings.

If you use different Eclipse workspaces, please note that cleanup profiles and most ABAP cleaner settings are shared 
across all workspaces, whereas the 'Cleanup Settings' are workspace-specific. This means that if you switch to a different 
Eclipse workspace, ABAP cleaner switches to the cleanup settings (profile name, cleanup range, syntax restriction) that 
were last used in this workspace. The same is true if you switch between the ADT plug-in and the stand-alone version of 
ABAP cleaner. 

### Selecting a Profile

In order to process the current code with a different profile (i.e. different cleanup rules and settings), 
simply select that profile from the 'Profile' list. 
Note that changes made in the 'Rules Used in Current Selection' list will be reset.

To configure an existing profile or create a new one, click on the 'Configure...' button 
to open the ['Profiles and Rules'](profiles.md) window.
When returning from this window with the 'Save Profiles and Exit' button, 
the code will automatically be reprocessed with the changed settings. 

### Default cleanup range

If you open the interactive ABAP cleaner from ADT, you can enter the 'Default cleanup range' to be 
* the 'Current command' (at cursor position), 
* the 'Current method' / declaration section / FORM etc., 
* the 'Current class' (i.e. either the whole definition or the whole implementation from ```CLASS``` to ```ENDCLASS```), or even 
* the 'Entire code document' (which may contain multiple classes etc.). 

Note that this default only applies if ABAP cleaner is called while no code is selected in the editor: 
If you select any amount of code, ABAP cleaner will always only clean the selected statements. 

### Restricting Rules to an ABAP Release

If you need to restrict code changes performed by the cleanup rules to the syntax of a specific ABAP Release 
(e.g. if your code must be downported to older releases), use the setting 
'Restrict rules to syntax of ...'. E.g., if you select 'ABAP 7.53' or lower, 
cleanup rules that introduce calculation assignment operators such as ```+=``` will be automatically skipped. 
Changes to this setting trigger a reprocessing of the current cleanup. 

When called from inside ADT, ABAP cleaner automatically determines the ABAP release of the current system 
and restricts rules to this release; with the setting on the UI, this can be restricted even further. 
The ABAP release of the current system is shown in the window title:

![ABAP cleaner main window - ABAP release in title](images/main-window_title.png "ABAP cleaner main window - ABAP release in title")

## Display Settings

If you want to focus on certain types of changes, activate or deactivate the options 
'Highlight indent changes', 
'Highlight inner space changes', 
'Highlight upper/lower case changes' and 
'Highlight text and line changes' (where 'line changes' refer to added, deleted, or moved lines). 

![ABAP cleaner main window - display settings](images/main-window_display-settings.png "ABAP cleaner main window - display settings")

Both the highlighting in the display (with yellow / orange background color) 
and the navigation to the previous / next findings (with the _Left / Right_ keys) 
will then be restricted to the selected types of changes.

Further display options can be found in the 'View' menu: 'Zoom In', 'Zoom Default' and 'Zoom Out' for changing the 
font size; 'Highlight Declarations' to display declaration keywords such as ```DATA``` or ```FIELD-SYMBOLS``` in a 
different color; and 'Highlight Write Positions' to show variables in write positions in a different color.


## Incremental Search

In order to perform a text search, press _Ctrl + F_ and directly start typing the search string. 
Press _(Shift +) F3_ or _(Shift +) Enter_ to navigate to the previous / next finding. 

![ABAP cleaner main window - incremental search](images/main-window_incremental-search.png "ABAP cleaner main window - incremental search")

With the options in the 'Incremental Search' section, you can influence 
whether the left-hand and/or right-hand display is searched, 
and whether the search shall be limited to 'changed lines only' 
(which refers to the lines currently highlighted in yellow / orange, depending on the 'Display' settings). 


## Deactivating Rules for a Selection of Statements

If you navigate to (any line within) a statement that was changed by ABAP cleaner, or if you select multiple statements, 
the 'Rules Used in Current Selection' list will show 
which ABAP cleaner rules were used to clean the selected ABAP statement(s). 

![ABAP cleaner main window - deactivating rules for a selection of statements](images/main-window_rules-used.png "ABAP cleaner main window - deactivating rules for a selection of statements")

Deactivate the checkbox(es) of one or several rule(s) in order to deactivate the rule for the current selection.
ABAP cleaner will then immediately reprocess the section accordingly. 
Deactivated rules will still appear as deactivated checkboxes to allow you to reactivate them. 

Note that this serves to deactivate rules in _specific_ code places, 
while the profile configuration should be used to _generally_ activate or deactivate rules. 
Deactivation of rules in specific code places will be lost when the code is processed again 
(e.g. with a different profile or by opening ABAP cleaner again).

You can use _Ctrl_ + click on one of the rules in the list to directly open its configuration in the current profile. 

### No pragmas or pseudo-comments for ABAP cleaner

ABAP cleaner strives to automatically determine where cleanup is possible and beneficial with the various cleanup rules. 
Therefore, there is no set of pragmas or pseudo-comments that could be written into the code 
in order to locally deactivate ABAP cleaner rules (and we don't think anyone would want them). 

If you feel that a cleanup rule worsens certain code places, rather than improving them, 
and this cannot be changed with the options of this cleanup rule, please open an issue and provide some code examples. 
We could then check whether the rule can be improved to automatically determine these cases 
and create a better cleanup result for them (or keep them unchanged). 

**Continue reading**: [Profiles and rules](profiles.md)
