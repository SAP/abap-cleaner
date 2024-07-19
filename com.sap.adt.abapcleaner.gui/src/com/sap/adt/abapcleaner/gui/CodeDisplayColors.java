package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.graphics.Color;

/***
 * Encapsulates the Colors used by the {@link CodeDisplay}. 
 * Depending on the method background color supplied to the constructor, a bright or a dark theme is selected.
 */
public class CodeDisplayColors {
	private static final boolean USE_DARK_BY_DEFAULT = false;
	
	// text fore colors
	public final Color textIdentifier;
	public final Color textIdentifierWritePos;
	public final Color textComment;
	public final Color textKeyword;
	public final Color textDeclarationKeyword;
	public final Color textNumber;
	public final Color textOperator;
	public final Color textStringLiteral;
	public final Color textTokenOperator;
	public final Color textDdlKeyword;
	public final Color textDdlAnnotation;
	public final Color textNonAbap;

	// line numbers
	public final Color lineNumber;
	public final Color lineNumberBack;

	// search matches
	public final Color highlightSearchMatch;
	public final Color normalSearchMatch;

	// miscellaneous
	public final Color verticalLine;
   public final Color textLink;

	// background color
	public final Color background;

	// background colors for code lines 
	public final Color lineSkip;
	public final Color lineDeleted;
	public final Color lineAdded;
	public final Color lineChanged;
	public final Color lineHighlight;
	public final Color lineOutsideCleanupRange;
	
	// background colors for selected code lines 
	public final Color selLine;
	public final Color selLineSkip;
	public final Color selLineDeleted;
	public final Color selLineAdded;
	public final Color selLineChanged;
	public final Color selLineHighlight;
	public final Color selBorder;

   // highlighting new features in FrmProfile (using color for "added" or "changed") 
	public final Color ruleAddedBackground;
   public final Color ruleChangedBackground;
   public final Color ruleActivatedBackground;
   public final Color ruleDeactivatedBackground;
   
   // configuration labels in FrmProfile
   public final Color configAddedBackground;
   public final Color configChangedBackground;
   
	/**
	 * Creates a set of Colors used in the {@link CodeDisplay} 
	 * 
	 * @param adt - whether ABAP Development Tools-inspired colors shall be used 
	 */
	public static CodeDisplayColors createDefault(ColorProfile colorProfile) {
		return new CodeDisplayColors(colorProfile, null, null, null, null, null, null); 
	}
	
	/**
	 * Creates a set of Colors used in the {@link CodeDisplay}; all parameters may be null. 
	 * 
	 * @param colorProfile - the color profile to be used 
	 * @param methodBackground - if supplied, it is used to determine whether a bright or dark theme is used
	 * @param identifier - text color of identifiers
	 * @param keyword - text color of ABAP keywords
	 * @param literal - text color of string literals
	 * @param literalNumber - text color of integer literals
	 * @param comment - text color of comments 
	 */
	public CodeDisplayColors(ColorProfile colorProfile, Color methodBackground, Color identifier, Color keyword, Color literal, Color literalNumber, Color comment) {
		// if a method background color was supplied, derive from it whether to use a bright or a dark theme 
		boolean useDark = getUseDarkTheme(methodBackground);
		boolean adt = (colorProfile == ColorProfile.ADT);
		
		//                                  | dark theme  |        | bright theme | existing ADT preference
		// ---------------------------------+-------------+--------+--------------+------------------------
		// text colors
		textIdentifier         = createColor(255, 255, 255, useDark,   0,   0,   0, identifier);
		textIdentifierWritePos = createColor(216,  12,  12, useDark, 216,  12,  12); 
		textOperator           = createColor(104, 151, 187, useDark, 128,   0, 128); // ADT reuses the 'identifier' color here  
		textTokenOperator      = createColor(104, 151, 187, useDark, 159,  52,   0); // ADT reuses the 'identifier' color here
		textKeyword            = createColor( 86, 165, 228, useDark,   0,   0, 255, keyword);
		textDeclarationKeyword = createColor(  0, 185, 185, useDark,   0, 153, 153);
		textStringLiteral      = createColor( 30, 181,  64, useDark,   0, 128,   0, literal); 
		textNumber             = createColor( 91, 233, 254, useDark,  51, 153, 255, literalNumber);
		textComment            = createColor(128, 128, 128, useDark, 128, 128, 128, comment);
		textDdlKeyword         = createColor(204, 120,  50, useDark, 127,   0, 116);
		textDdlAnnotation      = createColor(169, 156, 140, useDark,  51,  92, 162);
		textNonAbap            = createColor(255, 255, 255, useDark,  64,  64,  64); 

		// line numbers
		lineNumber             = createColor(208, 208, 208, useDark,  48,  48,  48);
		lineNumberBack         = createColor( 72,  72,  72, useDark, 220, 220, 220);

		// search matches
		highlightSearchMatch   = createColor(243, 236, 121, useDark, 139,   0,   0);
		normalSearchMatch      = createColor(192, 192, 192, useDark,   0,   0,   0);

		// miscellaneous
		verticalLine           = createColor(255, 182, 193, useDark, 255, 182, 193);
      textLink               = createColor(111, 197, 238, useDark,   0, 102, 204);
		
		background                = adt ? createColor( 40,  40,  40, useDark, 255, 255, 255, methodBackground) : createColor( 47,  47,  47, useDark, 255, 255, 255, methodBackground);

		// line background colors (ADT: colors inspired by ADT text compare view; non-ADT: dark theme colors inspired by code coverage colors in Eclipse; 
		// light-theme colors inspired by Compare plug-in of Notepad++)
		lineSkip                  = adt ? createColor( 20,  20,  20, useDark, 220, 220, 220) : createColor( 20,  20,  20, useDark, 225, 225, 225);
		lineDeleted               = adt ? createColor( 67,  42,  42, useDark, 255, 229, 229) : createColor( 98,   5,   5, useDark, 255, 215, 215); 
		lineAdded                 = adt ? createColor( 37,  63,  37, useDark, 229, 242, 229) : createColor(  6,  74,   6, useDark, 217, 255, 217);  
		lineChanged               = adt ? createColor( 66,  66,  66, useDark, 229, 229, 229) : createColor( 98,  98,   0, useDark, 231, 231, 152);  
		lineHighlight             = adt ? createColor( 85,  85,  85, useDark, 204, 204, 204) : createColor(160,  74,   6, useDark, 240, 191,  94);   
		lineOutsideCleanupRange   = adt ? createColor( 98,  98,  98, useDark, 200, 200, 200) : createColor( 98,  98,  98, useDark, 200, 200, 200);
		
		// background colors for lines within the current selection of lines
		selLine                   = adt ? createColor( 32,  32,  32, useDark, 240, 247, 255) : createColor(  0,   0,   0, useDark, 246, 246, 255);
		selLineSkip               = adt ? createColor( 12,  12,  12, useDark, 231, 231, 231) : createColor(  8,   8,   8, useDark, 227, 227, 236); 
		selLineDeleted            = adt ? createColor( 44,  28,  28, useDark, 255, 238, 238) : createColor( 64,   4,   4, useDark, 246, 221, 230); 
		selLineAdded              = adt ? createColor( 24,  42,  24, useDark, 238, 251, 238) : createColor(  4,  64,   4, useDark, 222, 246, 231);
		selLineChanged            = adt ? createColor( 44,  44,  44, useDark, 238, 238, 238) : createColor( 64,  64,   0, useDark, 231, 231, 191);
		selLineHighlight          = adt ? createColor( 56,  56,  56, useDark, 221, 221, 221) : createColor(106,  50,   4, useDark, 237, 206, 155);
		selBorder                 = adt ? createColor(192, 110,   0, useDark, 255, 165,   0) : createColor(192, 110,   0, useDark, 255, 165,   0);

      // FrmProfile: highlighting new features or differences between profiles
      ruleAddedBackground       = adt ? createColor( 37,  63,  37, useDark, 229, 242, 229) : createColor(  6,  74,   6, useDark, 217, 255, 217); // cp. lineAdded;
      ruleChangedBackground     = adt ? createColor( 66,  66,  66, useDark, 229, 229, 229) : createColor( 98,  98,   0, useDark, 231, 231, 152); // cp. lineChanged;
      ruleActivatedBackground   = ruleAddedBackground;
      ruleDeactivatedBackground = adt ? createColor( 67,  42,  42, useDark, 255, 229, 229) : createColor( 98,   5,   5, useDark, 255, 215, 215); // cp. lineDeleted;

      configAddedBackground     = ruleAddedBackground;
      configChangedBackground   = adt ? createColor( 82,  82,  82, useDark, 222, 222, 222) : ruleChangedBackground;
	}
	
	static boolean getUseDarkTheme(Color background) {
		return (background == null) ? USE_DARK_BY_DEFAULT : (background.getRed() + background.getGreen() + background.getBlue() < 3 * 128);
	}

	static Color createColor(int darkR, int darkG, int darkB, boolean useDarkTheme, int brightR, int brightG, int brightB) {
		return createColor(darkR, darkG, darkB, useDarkTheme, brightR, brightG, brightB, null); 
	}
	
	static Color createColor(int darkR, int darkG, int darkB, boolean useDarkTheme, int brightR, int brightG, int brightB, Color existingColor) {
		if (existingColor != null) {
			return existingColor;
		} else {
			return useDarkTheme ? new Color(darkR, darkG, darkB) : new Color(brightR, brightG, brightB);
		}
	}
}
