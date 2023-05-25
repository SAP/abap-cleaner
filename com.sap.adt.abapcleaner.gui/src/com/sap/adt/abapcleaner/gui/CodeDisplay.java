package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.comparer.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.CompareException;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.rulebase.*;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.events.MouseWheelListener;

public class CodeDisplay extends Composite {
	public static final int DEFAULT_FONT_SIZE = 9;
	private static final int MIN_LINE_NUMBER_DIGITS = 3;
	private static final double LINE_NUMBER_KEY_TIME_SPAN_MAX_MS = 1000;
	private static final int LEFT_MOUSE_BUTTON = 1; // see https://help.eclipse.org/2020-12/topic/org.eclipse.platform.doc.isv/reference/api/org/eclipse/swt/events/MouseEvent.html#button

	private final boolean SHOW_PAINT_TIMER_STATS = Program.showDevFeatures() && false;

	private CodeDisplayColors colors;
	
	private Slider vsbCode;
	private SashForm spcCode;
	private Canvas picCode1;
	private Canvas picCode2;

	private Font codeFont;
	private Font codeFontBold;
	private int visibleLineCount;

	private final DiffNavigator navigator = DiffNavigator.create();

	private Stopwatch lastLineNumberKey = Stopwatch.createAndStart();
	private String typedLineNumber = "";

	private IUsedRulesDisplay usedRulesDisplay;
	private ISearchControls searchControls;
	private IChangeTypeControls changeTypeControls;
	private IFallbackKeyListener fallbackKeyListener;

	private String sourceName;
	private String sourcePath;
	private String sourceCode;
	private String abapRelease;
	private Rule rule;
	private CleanupRange cleanupRange;
	private boolean showVerticalLine = true;
	private int verticalLinePos = 120;
	private boolean highlightDeclarationKeywords;
	private boolean highlightWritePositions;
	
	private boolean isLeftMouseButtonDown = false;

	public final String getSourceName() { return sourceName; }

	public final String getSourcePath() { return sourcePath; }

	public final String getSourceCode() { return sourceCode; }

	public final String getAbapRelease() { return abapRelease; }

	public final CleanupRange getCleanupRange() { return cleanupRange; }

	public final boolean getShowVerticalLine() { return showVerticalLine; }

	public final int getVerticalLinePos() { return verticalLinePos; }

	public final boolean getHighlightDeclarationKeywords() { return highlightDeclarationKeywords; }
	
	public final boolean getHighlightWritePositions() { return highlightWritePositions; }
	
	public final String getCodeToString() { return navigator.getCodeToString(); }

	public final int getTopLineIndex() { return vsbCode.getSelection(); }

	public final int getCurLineIndex() { return navigator.getCurLine(); }

	public final int getSelectionStartLine() { return navigator.getSelStartLine(); }

	public final int getVisibleLineCount() { return visibleLineCount; }

	private class PaintStats {
		final boolean forCompletePaint;
		int drawCount = 0;
		int drawTimeSum_ms = 0;
		
		PaintStats(boolean forCompletePaint) {
			this.forCompletePaint = forCompletePaint;
		}
		
		void add(int time_ms) {
			++drawCount;
			drawTimeSum_ms += time_ms;
		}
		
		String getInfo() {
			if (drawCount == 0)
				return "";
			return (forCompletePaint ? "all: " : "part: ") + Cult.format(drawTimeSum_ms / (double)drawCount, 1) + " ms (" + Cult.format(drawCount) + " x)";
		}
	}
	private final PaintStats completePaintStats = new PaintStats(true);
	private final PaintStats partPaintStats = new PaintStats(false);
	
	public CodeDisplay(Composite parent, int style) {
		super(parent, style);
		
		addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent arg0) {
				if (codeFont != null) {
					codeFont.dispose();
					codeFont = null;
				}
				if (codeFontBold != null) {
					codeFontBold.dispose();
					codeFontBold = null;
				}
			}
		});
		addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				codeDisplayKeyPressed(e);
			}
		});
		GridLayout gridLayout = new GridLayout(2, false);
		gridLayout.verticalSpacing = 0;
		gridLayout.horizontalSpacing = 0;
		gridLayout.marginWidth = 0;
		gridLayout.marginHeight = 0;
		setLayout(gridLayout);

		spcCode = new SashForm(this, SWT.NONE);
		spcCode.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
		spcCode.setSashWidth(8);

		picCode1 = new Canvas(spcCode, SWT.NO_BACKGROUND); // see https://www.eclipse.org/articles/Article-SWT-graphics/SWT_graphics.html
		picCode1.addMouseWheelListener(new MouseWheelListener() {
			public void mouseScrolled(MouseEvent arg0) {
				scrollBy(arg0.count);
			}
		});
		picCode1.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				codeDisplayKeyPressed(e);
			}
		});
		picCode1.addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(ControlEvent e) {
				refreshScrollbarLargeChange();
				picCode1.redraw();
			}
		});
		picCode1.addPaintListener(new PaintListener() {
			public void paintControl(PaintEvent arg0) {
				paintCode(picCode1, arg0, true);
				// ensure that the other display part is updated, too (note that this has no effect if no paint requests are outstanding)
				picCode2.update();
			}
		});
		picCode1.addMouseMoveListener(new MouseMoveListener() {
			public void mouseMove(MouseEvent arg0) {
				picCodeMouseMove(arg0, DisplaySide.LEFT);
			}
		});
		picCode1.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDown(MouseEvent e) {
				picCodeMouseDown(e, DisplaySide.LEFT);
			}

			@Override
			public void mouseUp(MouseEvent e) {
				picCodeMouseUp(e);
			}
		});

		picCode2 = new Canvas(spcCode, SWT.NO_BACKGROUND);
		picCode2.addMouseWheelListener(new MouseWheelListener() {
			public void mouseScrolled(MouseEvent arg0) {
				scrollBy(arg0.count);
			}
		});
		picCode2.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				codeDisplayKeyPressed(e);
			}
		});
		picCode2.addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(ControlEvent e) {
				picCode2.redraw();
			}
		});
		picCode2.addPaintListener(new PaintListener() {
			public void paintControl(PaintEvent arg0) {
				paintCode(picCode2, arg0, false);
				// ensure that the other display part is updated, too (note that this has no effect if no paint requests are outstanding)
				picCode1.update();   
			}
		});
		picCode2.addMouseMoveListener(new MouseMoveListener() {
			public void mouseMove(MouseEvent arg0) {
				picCodeMouseMove(arg0, DisplaySide.RIGHT);
			}
		});
		picCode2.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDown(MouseEvent e) {
				picCodeMouseDown(e, DisplaySide.RIGHT);
			}

			@Override
			public void mouseUp(MouseEvent e) {
				picCodeMouseUp(e);
			}
		});
		spcCode.setWeights(new int[] { 1, 1 });

		vsbCode = new Slider(this, SWT.VERTICAL);
		vsbCode.setLayoutData(new GridData(SWT.LEFT, SWT.FILL, false, true, 1, 1));
		vsbCode.addMouseWheelListener(new MouseWheelListener() {
			public void mouseScrolled(MouseEvent arg0) {
				scrollBy(arg0.count);
			}
		});
		vsbCode.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				codeDisplayKeyPressed(e);
			}
		});
		vsbCode.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				invalidateDisplay();
			}
		});

		// the JRE maps logical fonts to physical fonts, see https://docs.oracle.com/javase/8/docs/api/java/awt/Font.html
		final String logicalMonospacedFontName = "Monospaced";

		// find a monospaced font that is available on the system; if none is found, fall back to the logical "Monospaced" 
		// font guaranteed by Java. // TODO: use configurable Program.codeFont as first option in fontNames?
		String[] fontNames = new String[] { "Consolas", "Menlo", "Courier New", "Monaco", "Ubuntu Mono", "DejaVu Sans Mono", logicalMonospacedFontName };
		for (String fontName : fontNames) {
			// check whether a physical font with this name can be found on the system
			FontData[] fontList = Display.getCurrent().getFontList(fontName,  true);
			boolean isPhysicalFontAvailable = (fontList != null && fontList.length > 0);
			if (isPhysicalFontAvailable || fontName.equals(logicalMonospacedFontName)) {
				// see https://help.eclipse.org/2020-12/topic/org.eclipse.platform.doc.isv/reference/api/org/eclipse/swt/graphics/Font.html
				codeFont = new Font(Display.getCurrent(), fontName, DEFAULT_FONT_SIZE, SWT.NORMAL); 
				codeFontBold = new Font(Display.getCurrent(), fontName, DEFAULT_FONT_SIZE, SWT.BOLD); 
				break;
			}
		}
		
		searchControls = new DummySearchControls();
		refreshScrollbarLargeChange();
	}

	@Override
	protected void checkSubclass() {
		// Disable the check that prevents subclassing of SWT components
	}

	public final void setColors(CodeDisplayColors colors) { this.colors = colors; }
	
	public final void setUsedRulesDisplay(IUsedRulesDisplay usedRulesDisplay) { this.usedRulesDisplay = usedRulesDisplay; }

	public final void setSearchControls(ISearchControls searchControls) { this.searchControls = searchControls; }

	public final void setChangeTypeControls(IChangeTypeControls changeTypeControls) { this.changeTypeControls = changeTypeControls; }

	public final void setFallbackKeyListener(IFallbackKeyListener fallbackKeyListener) { this.fallbackKeyListener = fallbackKeyListener; }

	public final void prepareShow() {
		refreshScrollbarLargeChange();
	}

	public final void clear() {
		setInfo("", "", "", "", null);
		Code code = null;
		try {
			code = Code.parse(null, ParseParams.createForWholeCode("", "", ABAP.NEWEST_RELEASE));
		} catch (ParseException e) {
		}
		CompareDoc comp1 = CompareDoc.createFromText("", ABAP.LINE_SEPARATOR);
		CompareDoc comp2 = CompareDoc.createFromText("", ABAP.LINE_SEPARATOR);
		DiffDoc diffDoc = null;
		try {
			diffDoc = comp1.compareTo(comp2, null);
		} catch (CompareException e) {
		}
		refreshCode(code, diffDoc, 0, 0, 0, null);
		invalidateDisplay();
	}

	public final void setInfo(String sourceName, String sourcePath, String sourceCode, String abapRelease, Rule rule) {
		this.sourceName = sourceName;
		this.sourcePath = sourcePath;
		this.sourceCode = sourceCode;
		this.abapRelease = abapRelease;
		this.rule = rule;
	}

	public final void refreshCode() {
		refreshCode(null, null, -1, -1, -1, null);
	}

	/**
	 * @param code
	 * @param diffDoc
	 * @param setToTopLineIndex       -1 to keep current position, 0 to set to start
	 * @param setToCurLineIndex       -1 to keep current position, 0 to set to start
	 * @param setSelectionToStartLine -1 to keep current position, 0 to set to start
	 */
	public final void refreshCode(Code code, DiffDoc diffDoc, int setToTopLineIndex, int setToCurLineIndex, int setSelectionToStartLine, CleanupRange cleanupRange) {
		navigator.refreshCode(code, diffDoc, setToTopLineIndex, setToCurLineIndex, setSelectionToStartLine);
		this.cleanupRange = cleanupRange;
		
		int lineCount = navigator.getLineCount();
		vsbCode.setMaximum(lineCount); 
		refreshScrollbarLargeChange();
		if ((setToTopLineIndex >= 0 && vsbCode.getSelection() != setToTopLineIndex) || vsbCode.getSelection() >= lineCount || lineCount <= visibleLineCount)
			vsbCode.setSelection(Math.min(Math.max(setToTopLineIndex, 0), Math.max(lineCount - visibleLineCount + 1, 0)));

		if (changeTypeControls != null)
			changeTypeControls.updateChangedLineCount(navigator.getChangeStats());

		invalidateDisplay();
		if (usedRulesDisplay != null)
			usedRulesDisplay.selectionChanged();
	}

	private void invalidateDisplay() {
		// invalidate the right-hand display first
		picCode2.redraw();
		picCode1.redraw(); 
	}

	public final void reprocessSelection(Profile profile, int releaseRestriction) {
		String errorMessage = navigator.reprocessSelection(profile, releaseRestriction, sourceName);
		if (errorMessage == null) {
			refreshCode();
		} else {
			Message.show(errorMessage);
		}
	}

	public final void formActivated() {
		focusDisplay();
		setSearchMode(false);
	}

	private void refreshScrollbarLargeChange() {
		boolean needsScrollbar = (navigator.getLineCount() > visibleLineCount);
		if (codeFont != null) {
			visibleLineCount = picCode1.getClientArea().height / getCodeFontHeight();
			vsbCode.setPageIncrement(visibleLineCount);
			vsbCode.setThumb(visibleLineCount);
			vsbCode.setVisible(needsScrollbar);
			if (!needsScrollbar)
				vsbCode.setSelection(0);
		}
	}

	private void paintCode(Canvas pic, PaintEvent e, boolean isLeft) {
		Stopwatch stopwatch = SHOW_PAINT_TIMER_STATS ? Stopwatch.createAndStart() : null;
		
		int canvasWidth = pic.getClientArea().width; // e.width does not always work
		int canvasHeight = pic.getClientArea().height; // e.height does not always work

		Rectangle clipRect = e.gc.getClipping();

		// paint to an Image, then in the end paint the Image to the GC in one command (double buffering) 
		Image image = new Image(pic.getDisplay(), canvasWidth, canvasHeight);
		GC g = new GC(image); 
		g.setClipping(clipRect);

		// clear background
		if (colors != null)	
			g.setBackground(colors.background);
		g.fillRectangle(clipRect);

		if (navigator != null && !navigator.isEmpty() && colors != null) {
			paintCode(g, isLeft, canvasWidth, clipRect);
		}

		g.dispose();

		e.gc.drawImage(image, 0, 0);
		image.dispose();
		
		if (SHOW_PAINT_TIMER_STATS && searchControls != null) {
			// update paint statistics (this requires e.g. 25 ms per code display side for a full redraw, 5 ms for 3 code lines when pressing arrow up/down)
			PaintStats paintStats = (clipRect.height < canvasHeight) ? partPaintStats : completePaintStats;
			paintStats.add(stopwatch.getElapsedTimeMs());
			searchControls.searchTextChanged(completePaintStats.getInfo() + ", " + partPaintStats.getInfo(), true);
		}
		
		e.gc.dispose();
	}

	private void paintCode(GC g, boolean isLeft, int canvasWidth, Rectangle clipRect) {
		final int selBorderHeight = 2;

		int clipRectBottom = clipRect.y + clipRect.height;

		int startLine = vsbCode.getSelection();
		int lineNumberDigits = Math.max(navigator.getLineNumberDigits(), MIN_LINE_NUMBER_DIGITS);

		// search settings
		boolean searchSide = isLeft ? searchControls.searchLeftDisplay() : searchControls.searchRightDisplay();
		boolean searchChangedLinesOnly = searchControls.searchChangedLines();
		boolean ignoreCase = !searchControls.matchCase();
		boolean wholeWord = searchControls.matchWholeWord();

		// text measuring
		g.setFont(codeFont);
		int lineHeight = g.stringExtent("M").y; // g.getFontMetrics().getHeight(); // or getCodeFontHeight();
		int lineWidth = canvasWidth;
			
		// measure the width of a single char; since g.stringExtent(...).x is a rounded integer value, we use a longer String
		final String measureText = "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM";   
		int widthOfMeasureText = g.stringExtent(measureText).x;
		float charWidth = widthOfMeasureText / (float)measureText.length();
		
		// measure padding; this doesn't seem to be necessary in Java, but it doesn't harm either
		int widthOf2MeasureText = g.stringExtent(measureText + measureText).x;
		float padding = (2 * widthOfMeasureText - widthOf2MeasureText) / 2F;

		int lineNumberWidth = (int) ((lineNumberDigits + 1) * charWidth);
		int codeWidth = lineWidth - lineNumberWidth;

		int selectionLineMin = navigator.getSelectionLineMin();
		int selectionLineMax = navigator.getSelectionLineMax();

		// draw area for line numbers
		g.setBackground(colors.lineNumberBack);
		g.fillRectangle(0, clipRect.y, lineNumberWidth, clipRect.height);

		// draw background areas
		int lineIndexFromTop = 0;
		for (int lineIndex = startLine; lineIndexFromTop <= visibleLineCount; ++lineIndex) {
			if (lineIndex >= navigator.getLineCount())
				break;
			DiffLine diffLine = navigator.getLine(lineIndex);
			DisplayLine line = isLeft ? diffLine.leftLine : diffLine.rightLine;
			if (!navigator.showAddedAndDeletedLines() && line == null) {
				// skip this line WITHOUT increasing lineIndexFromTop (used for FrmProfile)
				continue;
			}
			int xOffset = lineNumberWidth;
			int yOffset = lineHeight * lineIndexFromTop;
			boolean isSelected = (lineIndex >= selectionLineMin && lineIndex <= selectionLineMax);
			++lineIndexFromTop;

			if (yOffset + lineHeight < clipRect.y || yOffset >= clipRectBottom)
				continue;

			// visualize line status
			LineStatus lineStatus = diffLine.status;
			boolean highlightChange = (lineStatus == LineStatus.CHANGED) && navigator.isLineHighlighted(diffLine);
			Color lineBackColor = getLineBackground(lineStatus, isLeft, isSelected, highlightChange, navigator.showAddedAndDeletedLines());
			if (line != null && (!line.isAbapCommand() || !line.isCommandInCleanupRange()))
				lineBackColor = colors.lineOutsideCleanupRange;
			
			if (lineBackColor != null && (navigator.showAnyChanges() || lineBackColor == colors.lineSkip || lineBackColor == colors.selLineSkip || lineBackColor == colors.lineOutsideCleanupRange)) {
				g.setBackground(lineBackColor);
				g.fillRectangle(xOffset, yOffset, codeWidth, lineHeight);
			}

			if (line == null)
				continue;

			// show text highlight
			if (highlightChange) {
				Color highlightBrush = (isSelected ? colors.selLineHighlight : colors.lineHighlight);
				for (HighlightBit bit : line.getHighlightBits()) {
					if (navigator.isLineBitHighlighted(bit)) {
						g.setBackground(highlightBrush);
						g.fillRectangle((int) ((float) xOffset + padding + bit.start * charWidth), yOffset, (int) ((float) bit.length * charWidth), lineHeight);
					}
				}
			}
		}

		if (showVerticalLine && verticalLinePos > 0) {
			float x = lineNumberWidth + padding + getVerticalLinePos() * charWidth;
			g.setForeground(colors.verticalLine);
			g.drawLine((int) x, clipRect.y, (int) x, clipRect.height);
		}

		// draw text and search match rectangles using a separate loop so rectangles are not covered by the next line's background
		lineIndexFromTop = 0;
		for (int lineIndex = startLine; lineIndexFromTop <= visibleLineCount; ++lineIndex) {
			if (lineIndex >= navigator.getLineCount())
				break;
			DiffLine diffLine = navigator.getLine(lineIndex);
			DisplayLine line = isLeft ? diffLine.leftLine : diffLine.rightLine;
			if (!navigator.showAddedAndDeletedLines() && line == null) {
				// skip this line WITHOUT increasing lineIndexFromTop (used for FrmProfile)
				continue;
			}
			int xOffset = lineNumberWidth;
			int yOffset = lineHeight * lineIndexFromTop;
			++lineIndexFromTop;

			// show selection border
			if (lineIndex == selectionLineMin) {
				g.setBackground(colors.selBorder);
				g.fillRectangle(xOffset, yOffset - selBorderHeight / 2, codeWidth, selBorderHeight);
			}
			if (lineIndex == selectionLineMax) {
				g.setBackground(colors.selBorder);
				g.fillRectangle(xOffset, yOffset + lineHeight - selBorderHeight / 2, codeWidth, selBorderHeight);
			}

			if (line == null) // only after incrementing lineIndexFromTop
				continue;

			if (yOffset + lineHeight + 1 < clipRect.y || yOffset - 1 >= clipRectBottom)
				continue;

			// draw line number (right-aligned)
			String lineNumber = String.valueOf(line.indexInDoc + 1);
			g.setForeground(colors.lineNumber);
			g.setFont(codeFont);
			g.drawText(lineNumber, (int) (charWidth * (lineNumberDigits - lineNumber.length() + 0.5F) - padding), yOffset, true);

			// draw text
			String text = line.getText();
			if (StringUtil.isNullOrEmpty(text) || line.getTextBits() == null)
				continue;
			for (TextBit textBit : line.getTextBits()) {
				g.setForeground(getTextForeground(textBit));
				g.setFont(getTextFont(textBit));
				g.drawText(text.substring(textBit.start, textBit.start + textBit.length), (int) (xOffset + charWidth * textBit.start), yOffset, true);
			}

			// show all search matches
			boolean lineStatusMatch = searchChangedLinesOnly ? navigator.isLineHighlighted(diffLine) : true;
			String searchText = navigator.getSearchText();
			if (navigator.isInSearchMode() && !StringUtil.isNullOrEmpty(searchText) && searchSide && lineStatusMatch) {
				int pos = -1;
				while (pos + 1 < text.length()) {
					pos = AbapCult.indexOf(text, searchText, pos + 1, ignoreCase);
					if (pos < 0)
						break;
					if (!wholeWord || navigator.isStartOfWholeWord(text, pos)) {
						DisplaySide displaySide = isLeft ? DisplaySide.LEFT : DisplaySide.RIGHT;
						boolean highlight = navigator.isSearchMatchHighlighted(lineIndex, pos, displaySide);
						Color searchMatchPen = highlight ? colors.highlightSearchMatch : colors.normalSearchMatch;
						g.setForeground(searchMatchPen);
						int x = (int) (xOffset + padding + charWidth * pos);
						int width = (int) (charWidth * searchText.length());
						g.drawRectangle(x, yOffset, width, lineHeight);
						if (highlight)
							g.drawRectangle(x - 1, yOffset - 1, width + 2, lineHeight + 2);
					}
				}
			}
		}
	}

	private Color getTextForeground(TextBit textBit) {
		switch (textBit.type) {
			case KEYWORD:
				return colors.textKeyword;
			case DECLARATION_KEYWORD:
				return highlightDeclarationKeywords ? colors.textDeclarationKeyword : colors.textKeyword;
			case IDENTIFIER:
				return colors.textIdentifier;
			case IDENTIFIER_WRITE_POS: 
				return highlightWritePositions ? colors.textIdentifierWritePos : colors.textIdentifier; 
			case USUAL_OPERATOR:
				return colors.textOperator;
			case TOKEN_OPERATOR:
				return colors.textTokenOperator;
			case NUMBER:
				return colors.textNumber;
			case STRING_LITERAL:
				return colors.textStringLiteral;
			case COMMENT:
				return colors.textComment;
			case NON_ABAP:
				return colors.textNonAbap;
			default:
				return colors.textIdentifier;
		}
	}

	private Font getTextFont(TextBit textBit) {
		switch (textBit.type) {
			case DECLARATION_KEYWORD:
				return highlightDeclarationKeywords ? codeFontBold : codeFont;
			default:
				return codeFont;
		}
	}

	private Color getLineBackground(LineStatus lineStatus, boolean left, boolean isSelected, boolean highlightChange, boolean highlightLineChange) {
		switch (lineStatus) {
			case EQUAL:
				return (isSelected ? colors.selLine : null);

			case LEFT_DELETED:
				if (!highlightLineChange)
					return null;
				return left ? (isSelected ? colors.selLineDeleted : colors.lineDeleted) 
								: (isSelected ? colors.selLineSkip : colors.lineSkip);
			
			case RIGHT_ADDED:
				if (!highlightLineChange)
					return null;
				return left ? (isSelected ? colors.selLineSkip : colors.lineSkip) 
								: (isSelected ? colors.selLineAdded : colors.lineAdded);

			case LEFT_DELETED_RIGHT_ADDED:
				if (!highlightLineChange)
					return null;
				return left ? (isSelected ? colors.selLineDeleted : colors.lineDeleted) 
								: (isSelected ? colors.selLineAdded : colors.lineAdded);

			case CHANGED:
				return highlightChange ? (isSelected ? colors.selLineChanged : colors.lineChanged) 
											  : (isSelected ? colors.selLine : null);
			
			default:
				return null;
		}
	}

	public final void codeDisplayKeyPressed(KeyEvent e) {
		if (processKeyPressedEvent(e))
			e.doit = false;
		else if (fallbackKeyListener != null)
			fallbackKeyListener.keyPressedInCodeDisplay(e);
	}
	
	private boolean processKeyPressedEvent(KeyEvent e) {
		if (navigator.isEmpty())
			return false;

		boolean controlPressed = ((e.stateMask & SWT.CONTROL) != 0);
		boolean shiftPressed = ((e.stateMask & SWT.SHIFT) != 0);

		if (e.character == '\u0006') { // Ctrl + F
			setSearchMode(true);
			return true;
		}
		if (navigator.isInSearchMode()) {
			if (e.keyCode == SWT.BS || e.character >= 32) {
				if (e.keyCode == SWT.BS)
					navigator.removeLastCharFromSearchText();
				else
					navigator.addCharToSearchText(e.character);
				boolean found = search(true, true);
				searchControls.searchTextChanged(navigator.getSearchText(), found);
				return true;
			}
			if (e.keyCode == SWT.ESC) {
				setSearchMode(false);
				return true;
			}
			if (e.keyCode == '\r') {
				// process like F3, but only if search mode is active
				search(!shiftPressed);
				return true;
			}
			switch (e.keyCode) {
				case SWT.ARROW_UP:
				case SWT.ARROW_DOWN:
				case SWT.ARROW_LEFT:
				case SWT.ARROW_RIGHT:
				case SWT.PAGE_UP:
				case SWT.PAGE_DOWN:
				case SWT.HOME:
				case SWT.END:
				case SWT.DEL:
					setSearchMode(false);
					navigator.clearSearchPos();
					// but NOT e.doit = false;
					break;
			}
		}
		if (e.keyCode == SWT.F3) {
			search(!shiftPressed); 
			return true;
		}

		
		if (e.keyCode == SWT.F10 && Program.showDevFeatures()) {
			// generate unit test from code selection
			generateUnitTest(true, !shiftPressed);
		}			
		if (e.keyCode == SWT.F11 && Program.showDevFeatures()) {
			// generate unit test (build commands only)
			generateUnitTest(false, !shiftPressed);
		}

		if (navigator.isInSearchMode())
			return false;

		if (controlPressed) {
			switch (e.keyCode) {
				case SWT.SPACE:
				case SWT.PAGE_DOWN:
				case SWT.PAGE_UP:
				case SWT.ARROW_RIGHT:
				case SWT.ARROW_LEFT: {
					if (shiftPressed) 
						break;
					resetLastLineNumberKey();
					if (e.keyCode == SWT.ARROW_RIGHT || e.keyCode == SWT.PAGE_DOWN || (e.keyCode == SWT.SPACE && !shiftPressed)) {
						int lastVisibleLine = vsbCode.getSelection() + visibleLineCount - 1;
						if (navigator.moveToNextScreenWithChanges(lastVisibleLine)) {
							scrollToLine(navigator.getCurLine() - 2);
							if (usedRulesDisplay != null)
								usedRulesDisplay.selectionChanged();
						}
					} else {
						int firstVisibleLine = vsbCode.getSelection();
						if (navigator.moveToPrevScreenWithChanges(firstVisibleLine)) {
							scrollToLine(navigator.getCurLine() - visibleLineCount + 2);
							if (usedRulesDisplay != null)
								usedRulesDisplay.selectionChanged();
						}
					}
					return true;
				}
				case SWT.ARROW_DOWN: {
					if (shiftPressed) 
						break;
					resetLastLineNumberKey();
					scrollToLine(vsbCode.getSelection() + 1);
					return true;
				}
				case SWT.ARROW_UP: {
					if (shiftPressed) 
						break;
					resetLastLineNumberKey();
					scrollToLine(vsbCode.getSelection() - 1);
					return true;
				}

				case SWT.HOME: {
					resetLastLineNumberKey();
					navigator.moveToFirstLine(!shiftPressed);
					scrollToLine(navigator.getCurLine());
					if (usedRulesDisplay != null)
						usedRulesDisplay.selectionChanged();
					return true;
				}
				case SWT.END: {
					resetLastLineNumberKey();
					navigator.moveToLastLine(!shiftPressed);
					scrollToLine(navigator.getCurLine() - visibleLineCount + 1);
					if (usedRulesDisplay != null)
						usedRulesDisplay.selectionChanged();
					return true;
				}

				case 'c': {
					copySelectionToClipboard(shiftPressed ? DisplaySide.LEFT : DisplaySide.RIGHT);
					return true;
				}

				case 'a': {
					navigator.selectAll();
					invalidateDisplay();
					if (usedRulesDisplay != null)
						usedRulesDisplay.selectionChanged();
					return true;
				}
				case '+': {
					zoomIn();
					return true;
				}
				case '-': {
					zoomOut();
					return true;
				}
			}

		} else {
			switch (e.keyCode) {
				case SWT.SPACE:
				case SWT.ARROW_RIGHT:
				case SWT.ARROW_LEFT: {
					resetLastLineNumberKey();
					boolean moved;
					if (e.keyCode == SWT.ARROW_RIGHT || (e.keyCode == SWT.SPACE && !shiftPressed))
						moved = navigator.moveToNextChange();
					else // (e.keyCode == SWT.Left || (e.keyCode == SWT.Space && shiftPressed))
						moved = navigator.moveToPrevChange();
					if (moved) {
						scrollToLine(navigator.getCurLine() - visibleLineCount / 2);
						if (usedRulesDisplay != null)
							usedRulesDisplay.selectionChanged();
					}
					return true;
				}

				case SWT.PAGE_DOWN: {
					resetLastLineNumberKey();
					navigator.moveToNextPage(visibleLineCount, !shiftPressed);
					scrollToLine(vsbCode.getSelection() + vsbCode.getPageIncrement());
					if (usedRulesDisplay != null)
						usedRulesDisplay.selectionChanged();
					return true;
				}
				case SWT.PAGE_UP: {
					resetLastLineNumberKey();
					navigator.moveToPrevPage(visibleLineCount, !shiftPressed);
					scrollToLine(vsbCode.getSelection() - vsbCode.getPageIncrement());
					if (usedRulesDisplay != null)
						usedRulesDisplay.selectionChanged();
					return true;
				}

				case SWT.ARROW_DOWN: {
					resetLastLineNumberKey();
					boolean invalidateAll = navigator.areLinesSelected() || !navigator.showAddedAndDeletedLines();
					navigator.moveToNextLine(!shiftPressed);
					ensureCurLineIsVisible(invalidateAll);
					if (usedRulesDisplay != null)
						usedRulesDisplay.selectionChanged();
					return true;
				}
				case SWT.ARROW_UP: {
					resetLastLineNumberKey();
					boolean invalidateAll = navigator.areLinesSelected() || !navigator.showAddedAndDeletedLines();
					navigator.moveToPrevLine(!shiftPressed);
					ensureCurLineIsVisible(invalidateAll);
					if (usedRulesDisplay != null)
						usedRulesDisplay.selectionChanged();
					return true;
				}
				case SWT.HOME: {
					resetLastLineNumberKey();
					navigator.moveToLine(vsbCode.getSelection(), !shiftPressed);
					invalidateDisplay();
					if (usedRulesDisplay != null)
						usedRulesDisplay.selectionChanged();
					return true;
				}
				case SWT.END: {
					resetLastLineNumberKey();
					navigator.moveToLine(vsbCode.getSelection() + visibleLineCount - 1, !shiftPressed);
					invalidateDisplay();
					if (usedRulesDisplay != null)
						usedRulesDisplay.selectionChanged();
					return true;
				}
			}
			if (e.character >= '0' && e.character <= '9') {
				String addedNum = String.valueOf(((char) e.character));
				if (lastLineNumberKey.isRunning() && lastLineNumberKey.getElapsedTimeMs() <= LINE_NUMBER_KEY_TIME_SPAN_MAX_MS)
					typedLineNumber += addedNum;
				else
					typedLineNumber = addedNum;

				if (navigator.moveToLineInDoc(Integer.parseInt(typedLineNumber) - 1, DisplaySide.LEFT)) {
					scrollToLine(navigator.getCurLine() - visibleLineCount / 2);
					if (usedRulesDisplay != null)
						usedRulesDisplay.selectionChanged();
				}
				lastLineNumberKey.resetAndStart();
				return true;
			}
		}
		return false;
	}

	public final void copySelectionToClipboard() {
		copySelectionToClipboard(DisplaySide.RIGHT);
	}

	public final void copySelectionToClipboard(DisplaySide displaySide) {
		String text = navigator.getSelectedText(displaySide, ABAP.LINE_SEPARATOR);
		if (!StringUtil.isNullOrEmpty(text))
			SystemClipboard.setText(text);
	}

	public final String getSelectedText(DisplaySide displaySide) {
		return navigator.getSelectedText(displaySide, ABAP.LINE_SEPARATOR);
	}

	private boolean search(boolean findNext) {
		return search(findNext, false);
	}

	private boolean search(boolean findNext, boolean mayExtendCurrentSelection) {
		if (navigator.search(findNext, mayExtendCurrentSelection, searchControls.searchLeftDisplay(), searchControls.searchRightDisplay(), searchControls.searchChangedLines(),
				searchControls.matchCase(), searchControls.matchWholeWord())) {
			scrollToLine(navigator.getCurLine() - visibleLineCount / 2); // ensureCurLineIsVisible(true);
			return true;
		} else {
			return false;
		}
	}

	private void setSearchMode(boolean searchMode) {
		navigator.setSearchMode(searchMode);
		if (searchControls != null)
			searchControls.searchModeChanged(searchMode);

		invalidateDisplay();
	}

	private void ensureCurLineIsVisible(boolean invalidateAll) {
		final int padding = 1;
		if (navigator.getCurLine() - padding < vsbCode.getSelection())
			scrollToLine(navigator.getCurLine() - padding);
		else if (navigator.getCurLine() + padding >= vsbCode.getSelection() + visibleLineCount)
			scrollToLine(navigator.getCurLine() + padding - visibleLineCount + 1);
		else if (invalidateAll) {
			// as there may have been a larger selection before the cursor move, all must be invalidated
			invalidateDisplay();
		} else {
			// invalidate the new "curLine", the line above it and the line below it (plus a few pixel rows for the selection border)
			final int borderHeight = 2;
			int lineHeight = getCodeFontHeight();
			int yOffset = (navigator.getCurLine() - vsbCode.getSelection()) * lineHeight;
			picCode1.redraw(0, yOffset - lineHeight - borderHeight, picCode1.getClientArea().width, 3 * lineHeight + 2 * borderHeight, false);
			picCode2.redraw(0, yOffset - lineHeight - borderHeight, picCode2.getClientArea().width, 3 * lineHeight + 2 * borderHeight, false);
		}
	}

	private void scrollBy(int diff) {
		scrollToLine(vsbCode.getSelection() - diff);
	}

	private void scrollToLine(int lineIndex) {
		vsbCode.setSelection(Math.min(Math.max(lineIndex, 0), vsbCode.getMaximum() - vsbCode.getPageIncrement() + 1));
		invalidateDisplay();
	}

	private void picCodeMouseDown(MouseEvent e, DisplaySide displaySide) {
		if (navigator.isEmpty())
			return;

		if (e.button == LEFT_MOUSE_BUTTON)
			isLeftMouseButtonDown = true;

		boolean shiftPressed = ((e.stateMask & SWT.SHIFT) != 0);
		navigator.setCurLine(getLineIndexOfMousePos(e.y, displaySide), !shiftPressed);

		invalidateDisplay();
		if (usedRulesDisplay != null)
			usedRulesDisplay.selectionChanged();

		focusDisplay();
	}

	private void picCodeMouseUp(MouseEvent e) {
		if (e.button == LEFT_MOUSE_BUTTON)
			isLeftMouseButtonDown = false;
	}

	private int getLineIndexOfMousePos(int mouseY, DisplaySide displaySide) {
		if (navigator.showAddedAndDeletedLines()) {
			return vsbCode.getSelection() + (int) (mouseY / getCodeFontHeight());
		} else {
			int lineIndex = vsbCode.getSelection();
			int visibleLinesToSkip = (int) (mouseY / getCodeFontHeight());
			while (lineIndex < navigator.getLineCount()) {
				DiffLine diffLine = navigator.getLine(lineIndex);
				if (diffLine == null)
					break;
				DisplayLine displayLine = (displaySide == DisplaySide.LEFT) ? diffLine.leftLine : diffLine.rightLine;
				if (displayLine != null) {
					if (visibleLinesToSkip <= 0)
						break;
					--visibleLinesToSkip;
				}
				++lineIndex;
			}
			return lineIndex;
		}
	}

	/**
	 * Returns the height in pixels.
	 * 
	 * @return
	 */
	private final int getCodeFontHeight() {
		GC gc = new GC(picCode1);
		gc.setFont(codeFont);
		int height = gc.stringExtent("M").y; // codeFont.getFontData()[0].getHeight() does NOT work
		gc.dispose();
		return height;
	}

	/**
	 * Returns the font size in points.
	 * 
	 * @return
	 */
	public final float getCodeFontSize() {
		float result = 0F;
		FontData[] fontData = codeFont.getFontData();
		if (fontData.length > 0) 
			result = fontData[0].getHeight();
		return (result == 0F) ? DEFAULT_FONT_SIZE : result;
	}

	public final void zoomIn() {
		float oldSize = getCodeFontSize();
		float step = (oldSize < 12F) ? 1F : 2F;
		float newSize = Math.min(oldSize + step, 24F);
		setCodeFontSize(newSize);
	}

	
	public final void zoomOut() {
		float oldSize = getCodeFontSize();
		float step = (oldSize > 12F) ? 2F : 1F;
		float newSize = Math.max(oldSize - step, 1F);
		setCodeFontSize(newSize);
	}

	public final void zoomDefault() {
		setCodeFontSize(DEFAULT_FONT_SIZE);
	}

	/**
	 * Sets the font size in points.
	 * 
	 * @param value
	 */
	public final void setCodeFontSize(float value) {
		if (getCodeFontSize() == value)
			return;

		codeFont = changeSize(codeFont, value);
		codeFontBold = changeSize(codeFontBold, value);
		
		refreshScrollbarLargeChange();
		invalidateDisplay();
	}

	private final Font changeSize(Font font, float value) {
		FontData[] fontData = font.getFontData();
		for (int i = 0; i < fontData.length; ++i)
			fontData[i].setHeight((int) value);
		font.dispose();
		return new Font(getDisplay(), fontData);
	}
	
	private void picCodeMouseMove(MouseEvent e, DisplaySide displaySide) {
		if (navigator.isEmpty())
			return;

		if (isLeftMouseButtonDown) { // e.button == LEFT_MOUSE_BUTTON doesn't work
			navigator.setCurLine(getLineIndexOfMousePos(e.y, displaySide), false);
			ensureCurLineIsVisible(true);
			if (usedRulesDisplay != null)
				usedRulesDisplay.selectionChanged();
		}
	}

	public final RuleStats[] getRuleStats(Profile profile) {
		return navigator.getRuleStats(profile);
	}

	public final void setBlockRuleInSelection(RuleID ruleID, boolean blocked) {
		navigator.setBlockRuleInSelection(ruleID, blocked);
	}

	public final void setHighlight(ChangeTypes highlight) {
		navigator.setHighlight(highlight);
		invalidateDisplay();
	}

	public final void focusDisplay() {
		vsbCode.setFocus();
	}

	private void resetLastLineNumberKey() {
		lastLineNumberKey.stop();
		typedLineNumber = "";
	}

	public final void setVerticalLine(boolean showVerticalLine, int verticalLinePos) {
		this.showVerticalLine = showVerticalLine;
		this.verticalLinePos = verticalLinePos;
		invalidateDisplay();
	}
	
	public final void setHighlightDeclarationKeywords(boolean highlightDeclarationKeywords) {
		this.highlightDeclarationKeywords = highlightDeclarationKeywords; 
		invalidateDisplay();
	}

	public final void setHighlightWritePositions(boolean highlightWritePositions) {
		this.highlightWritePositions = highlightWritePositions; 
		invalidateDisplay();
	}

	final Command getCommandAt(int lineIndex) {
		return navigator.getCommandAt(lineIndex);
	}

	/** builds the Java code for a Unit Test from the currently selected code */
	final void generateUnitTest(boolean fullMethod, boolean showMessage) {
		final String LINE_SEP = ABAP.LINE_SEPARATOR;
		
		// get the left and right side of the currently selected code section
		String srcText = StringUtil.trimEnd(getSelectedText(DisplaySide.LEFT), ABAP.LINE_SEPARATOR);
		String expText = StringUtil.trimEnd(getSelectedText(DisplaySide.RIGHT), ABAP.LINE_SEPARATOR);
		if (StringUtil.isNullOrEmpty(srcText)) {
			Message.show("Please select some source code first!");
			return;
		}

		// build the test method start
		StringBuilder sb = new StringBuilder();
		if (fullMethod) {
			sb.append(LINE_SEP);
			sb.append("\t").append("@Test").append(LINE_SEP);
			sb.append("\t").append("void test() {").append(LINE_SEP);
		}
		
		// build special setting of configuration options
		if (rule != null) {
			boolean found = false;
			ConfigValue[] configValues = rule.getConfigValues();
			for (ConfigValue configValue : configValues) {
				if (!configValue.isDefault()) {
					sb.append("\t\t").append("rule.config" + configValue.settingName);
					if (configValue instanceof ConfigSelectionValue) {
						sb.append(".setEnumValue(" + configValue.settingName + ".forValue(");
						sb.append(String.valueOf(((ConfigSelectionValue) configValue).getValue()) + ")");
					} else {
						sb.append(".setValue(");
						if (configValue instanceof ConfigBoolValue)
							sb.append(((ConfigBoolValue) configValue).getValue() ? "true" : "false");
						else if (configValue instanceof ConfigIntValue)
							sb.append(String.valueOf(((ConfigIntValue) configValue).getValue()));
						else if (configValue instanceof ConfigTextValue)
							sb.append("\"" + ((ConfigTextValue) configValue).getValue() + "\"");
					}
					sb.append(");").append(LINE_SEP);
					found = true;
				}
			}
			if (found)
				sb.append(LINE_SEP);
		}

		// build the source code (= the input code)
		String[] srcLines = StringUtil.split(srcText, ABAP.LINE_SEPARATOR, false);
		for (String srcLine : srcLines) {
			sb.append("\t\t").append("buildSrc(\"").append(StringUtil.getEscapeText(srcLine)).append("\");").append(LINE_SEP);
		}
		sb.append(LINE_SEP);
		
		// build the expected code
		if (srcText.equals(expText)) {
			sb.append("\t\t").append("copyExpFromSrc();").append(LINE_SEP);
		} else {
			String[] expLines = StringUtil.split(expText, ABAP.LINE_SEPARATOR, false);
			for (String expLine : expLines) {
				sb.append("\t\t").append("buildExp(\"").append(StringUtil.getEscapeText(expLine)).append("\");").append(LINE_SEP);
			}
		}

		if (fullMethod) {
			// if the section is inside a method or a class definition, generate respective calls
			Command command = getCommandAt(getCurLineIndex());
			while (command != null) {
				command = command.getParent();
				if (command == null)
					break;
				if (command.isMethodStart()) {
					sb.append(LINE_SEP);
					sb.append("\t\t").append("putAnyMethodAroundSrcAndExp();").append(LINE_SEP);
					break;
				} else if (command.isDeclarationSectionStart()) {
					sb.append(LINE_SEP);
					sb.append("\t\t").append("putAnyClassDefAroundSrcAndExp();").append(LINE_SEP);
					break;
				}
			}
	
			// build the test method end
			sb.append(LINE_SEP);
			sb.append("\t\t").append("testRule();").append(LINE_SEP);
			sb.append("\t").append("}").append(LINE_SEP);
		}
		
		// copy the generated code to the clipboard and show a success message
		SystemClipboard.setText(sb.toString());
		if (showMessage) {
			String message = "The generated unit test code was copied to the clipboard.";
			if (rule == null) {
				message += "\r\n" + "WARNING: Code was processed with more than 1 rule!";
			}
			Message.show(message, "Generate Unit Test from Code Selection");
		}
	}

	/** builds the Java code for the overridden method {@link Rule#getExample()} from the currently selected code */
	final void generateExample(boolean fullStatement, boolean showMessage) {
		final String LINE_SEP = ABAP.LINE_SEPARATOR;
		
		// get the left and right side of the currently selected code section
		String srcText = StringUtil.trimEnd(getSelectedText(DisplaySide.LEFT), ABAP.LINE_SEPARATOR);
		if (StringUtil.isNullOrEmpty(srcText)) {
			Message.show("Please select some source code first!");
			return;
		}

		// build the example start
		StringBuilder sb = new StringBuilder();
		if (fullStatement) {
			sb.append("\t\t").append("return \"\"");
		}
		
		// build the source code (= the input code)
		String[] srcLines = StringUtil.split(srcText, ABAP.LINE_SEPARATOR, false);
		for (String srcLine : srcLines) {
			sb.append(LINE_SEP);
			sb.append("\t\t").append("+ LINE_SEP + \"").append(StringUtil.getEscapeText(srcLine)).append("\"");
		}
		if (fullStatement) {
			sb.append(";");
		}
		sb.append(LINE_SEP);
		
		// copy the generated example to the clipboard and show a success message
		SystemClipboard.setText(sb.toString());
		if (showMessage) {
			String message = "The generated example code was copied to the clipboard.";
			Message.show(message, "Generate Example from Code Selection");
		}
	}

	public void moveToFirstChange() {
		if (navigator == null)
			return;
		navigator.moveToFirstLine(true);
		boolean moved = navigator.moveToNextChange();
		if (moved) {
			scrollToLine(navigator.getCurLine() - visibleLineCount / 2);
			if (usedRulesDisplay != null)
				usedRulesDisplay.selectionChanged();
		}
	}
}
