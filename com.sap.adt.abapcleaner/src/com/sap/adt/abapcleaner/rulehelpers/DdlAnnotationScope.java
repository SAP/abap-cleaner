package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.Rule;

public class DdlAnnotationScope {
	private ArrayList<DdlAnnotation> annotations = new ArrayList<>();
	
	private final boolean allowComments;
	
	private Command firstCommand;
	private Command lastCommand;
	
	private ArrayList<DdlAnnotationNestingRange> nestingRanges;

	public ArrayList<DdlAnnotation> getAnnotations() { return annotations; }
	public boolean isEmpty() { return annotations.isEmpty(); }
	public Command getFirstCommand() { return firstCommand; }
	public Command getLastCommand() { return lastCommand; }
	
	public DdlAnnotationScope(boolean allowComments) {
		this.allowComments = allowComments;
	}
	
	public void add(Command command) throws UnexpectedSyntaxBeforeChanges {
		if (firstCommand == null)
			firstCommand = command;
		lastCommand = command;
		
		ArrayList<DdlAnnotationElement> elements = new ArrayList<>();
		boolean hasContentToAdd = false;
		boolean expectingValue = false;

		// 'parse' the Command, note down the annotation elements, and build this.annotations from them
		Token token = command.getFirstToken();
		int lineBreaks = token.lineBreaks;
		while (token != null) {
			if (token.isComment()) {
				if (allowComments) {
					// simply skip the comment (only possible if annotation layout is not changed)
					token = token.getNext();
					continue;
				} else {
					// 
					throw new UnexpectedSyntaxBeforeChanges(null, token, "annotations with comments not yet supported");
				}
			}
				
			if (token.textEqualsAny(DDL.BRACE_OPEN_STRING)) {
				expectingValue = false;
				DdlAnnotationElement lastElement = elements.get(elements.size() - 1);
				if (lastElement.startsArray) {
					// start an anonymous object with the next free number
					++lastElement.anonymousObjectCount;
					elements.add(DdlAnnotationElement.createAnonymousObject(lastElement.anonymousObjectCount));
				}
				elements.get(elements.size() - 1).startedNestingInOriginalCode = true;
			
			} else if (token.textEquals(DDL.DOT_SIGN_STRING)) {
				expectingValue = false;

			} else if (token.textEqualsAny(DDL.COLON_SIGN_STRING)) {
				expectingValue = true; 
			
			} else if (token.textEqualsAny(DDL.BRACKET_OPEN_STRING)) {
				expectingValue = true;
				DdlAnnotationElement lastElement = elements.get(elements.size() - 1);
				if (lastElement.startsArray) {
					// start an anonymous object with the next free number
					++lastElement.anonymousObjectCount;
					elements.add(DdlAnnotationElement.createAnonymousObject(lastElement.anonymousObjectCount));
				}
				elements.get(elements.size() - 1).startsArray = true;
			
			} else if (token.textEqualsAny(DDL.COMMA_SIGN_STRING, DDL.BRACE_CLOSE_STRING, DDL.BRACKET_CLOSE_STRING)) {
				if (hasContentToAdd)
					annotations.add(DdlAnnotation.create(annotations.size(), elements, expectingValue, token.getPrevCodeToken(), lineBreaks));

				// if a comma was found, return to the beginning of the last { or [;
				// if } or ] was found, return to the beginning of the { or [ before the last one
				boolean passBraceOrBracket = !token.textEquals(DDL.COMMA_SIGN_STRING);
				while (elements.size() > 0) {
					DdlAnnotationElement lastElement = elements.get(elements.size() - 1);
					if (lastElement.startsArray || lastElement.startedNestingInOriginalCode) {
						if (!passBraceOrBracket)
							break;
						passBraceOrBracket = false;
					}
					elements.remove(elements.size() - 1);
				}
				
				hasContentToAdd = false;
				expectingValue = (elements.size() > 0 && elements.get(elements.size() - 1).startsArray);
				if (token.textEquals(DDL.COMMA_SIGN_STRING)) {
					lineBreaks = (token.getNextCodeToken() == null) ? 0 : token.getNextCodeToken().lineBreaks;
				}
			} else if (expectingValue) {
				String valueText = token.getText();

				// consider the special case of unchecked expressions #(...)
				if (token.textEquals("#") && token.getNextCodeToken().getOpensLevel()) {
					StringBuilder sbExpr = new StringBuilder(token.getText());
					Token lastOfExpr = token.getNextCodeToken().getNextCodeSibling();
					do {
						token = token.getNext();
						// include spaces, but put everything to one line 
						int spaces = (token.lineBreaks > 0) ? 1 : token.spacesLeft;
						if (spaces > 0)
							sbExpr.append(StringUtil.repeatChar(' ', spaces));
						sbExpr.append(token.getText()); 
					} while (token != lastOfExpr);
					valueText = sbExpr.toString();
					// token is now at the end of the unchecked expression
				}
				
				elements.add(DdlAnnotationElement.create(valueText));
				hasContentToAdd = true;
				
			} else {
				String[] newElements = StringUtil.split(token.getText(), DDL.DOT_SIGN, true);
				for (String newElement : newElements) {
					newElement = newElement.trim();
					if (newElement.startsWith(DDL.ANNOTATION_SIGN_STRING))
						newElement = newElement.substring(DDL.ANNOTATION_SIGN_STRING.length());
					if (!StringUtil.isNullOrEmpty(newElement)) {
						elements.add(DdlAnnotationElement.create(newElement));
					}
				}
				hasContentToAdd = true; // in case this is a Boolean annotation that implicitly uses the default value
			}
			token = token.getNext();
		}
		
		if (hasContentToAdd) {
			annotations.add(DdlAnnotation.create(annotations.size(), elements, expectingValue, command.getLastCodeToken(), lineBreaks));
		}
	}

	public boolean belongsToMainAnnotations() {
		return firstCommand == null || firstCommand.getParent() == null;
	}
	
	public void finishBuild() {
		int digits = String.valueOf(annotations.size()).length();
		for (DdlAnnotation annotation : annotations) {
			annotation.initializeSortKeys(digits);
		}
	}

	public int getMaxLevelCount() {
		int maxLevel = 0;
		for (DdlAnnotation anno : annotations) {
			maxLevel = Math.max(maxLevel, anno.getLevelCount());
		}
		return maxLevel;
	}
	
	public void useExistingNesting() {
		nestingRanges = new ArrayList<>();
		
		// determine possible nesting
		for (int annoIndex = 0; annoIndex < annotations.size(); ++annoIndex) {
			checkNestingFor(annoIndex, DdlAnnotationNestingDepth.KEEP_AS_IS, null, null);
		}
		
		// determine the (maximum) column width, if column alignment is possible, 
		// as well as the maximum inner element depth and maximum inner nesting depth
		for (DdlAnnotationNestingRange nestingRange : nestingRanges) {
			nestingRange.finishBuild();
		}
	}

	public void adjustNesting(DdlAnnotationNestingDepth nestingDepth, String allowList, String blockList, DdlAnnotationSortOrder sortOrder) {
		String[] allowAnnotations = StringUtil.split(allowList, new char[] { ',', ' ' }, true);
		String[] blockAnnotations = StringUtil.split(blockList, new char[] { ',', ' ' }, true);
		
		nestingRanges = new ArrayList<>();
		
		// temporarily sort annotations in alphabetical order 
		annotations.sort(new DdlAnnotation.AlphabeticComparator());
		
		// determine possible nesting
		for (int annoIndex = 0; annoIndex < annotations.size(); ++annoIndex) {
			checkNestingFor(annoIndex, nestingDepth, allowAnnotations, blockAnnotations);
		}
		
		// determine the (maximum) column width, if column alignment is possible, 
		// as well as the maximum inner element depth and maximum inner nesting depth
		for (DdlAnnotationNestingRange nestingRange : nestingRanges) {
			nestingRange.finishBuild();
		}
		
		// if configured, keep alphabetical sort order by all elements
		if (sortOrder == DdlAnnotationSortOrder.BY_ALL_ELEMS)
			return;
		
		// define sort sequence to preserve original order where possible
		int digits = String.valueOf(annotations.size()).length();
		nestingRanges.sort(new DdlAnnotationNestingRange.NestingRangeComparator());
		for (DdlAnnotationNestingRange nestingRange : nestingRanges) {
			// determine the annotation with the lowest original position
			// (e.g. "05" if the nesting range contains annotations from original position "12", "05" and "07")
			int posMin = Integer.MAX_VALUE;
			for (DdlAnnotation annotation : nestingRange.annotations) {
				posMin = Math.min(posMin, annotation.getPosInSequence());
			}
			// use the position of this annotation as a prefix for all annotations in the nesting range
			// (so the annotations get seqSortKey = "05.12", "05.05", and "05.07"
			String prefix = StringUtil.getNumberWithLeadingZeros(posMin, digits) + ".";
			for (DdlAnnotation annotation : nestingRange.annotations) {
				annotation.addPrefixToSeqSortKey(prefix); 
			}
		}
		
		if (sortOrder == DdlAnnotationSortOrder.BY_FIRST_ELEM || sortOrder == DdlAnnotationSortOrder.BY_TWO_ELEMS) {
			// group by first name element; after that, keep the original order, or the order required for the nesting ranges
			for (DdlAnnotation annotation : annotations) {
				String prefix = annotation.getElementName(0) + ".";
				if (sortOrder == DdlAnnotationSortOrder.BY_TWO_ELEMS)
					prefix += annotation.getElementName(1) + ".";
				annotation.addPrefixToSeqSortKey(prefix);
			}
		}
		
		// re-sort annotations to their new order according to .seqSortKey
		annotations.sort(new DdlAnnotation.SequenceComparator());
	}
	
	private void checkNestingFor(int annoIndex, DdlAnnotationNestingDepth nestingDepth, String[] allowAnnotations, String[] blockAnnotations) {
		DdlAnnotation anno = annotations.get(annoIndex);
		int levelOfFirstDiff = (annoIndex == 0) ? -1 : anno.getLevelOfFirstDiff(annotations.get(annoIndex - 1));

		for (int level = 0; level < anno.getLevelCount(); ++level) {
			checkNestingFor(annoIndex, levelOfFirstDiff, level, nestingDepth, allowAnnotations, blockAnnotations);
		}
	}

	private void checkNestingFor(int annoIndex, int levelOfFirstDiff, int nestAfterLevel, DdlAnnotationNestingDepth nestingDepth, String[] allowAnnotations, String[] blockAnnotations) {
		DdlAnnotation anno = annotations.get(annoIndex);

		// was nesting already planned when processing an earlier annotation?
		if (anno.startsNestingAfterLevel(nestAfterLevel)) {
			return;
		}
		
		// check 'mandatory' and 'impossible' cases
		if (anno.hasArrayStartAt(nestAfterLevel)) {
			// array elements must always be 'nested' 
			nestingRanges.add(createNestingRange(annotations, annoIndex, nestAfterLevel, true, true));
			return;

		} else if (anno.hasAnonymousArrayObjectAt(nestAfterLevel)) {
			// anonymous objects inside an array must always be 'nested'
			nestingRanges.add(createNestingRange(annotations, annoIndex, nestAfterLevel, true, false));
			return;

		} else if (nestingDepth.getUseExisting()) {
			// nesting enforced if (and only if) the original code used it here
			if (anno.originalCodeStartedNestingAfterLevel(nestAfterLevel))
				nestingRanges.add(createNestingRange(annotations, annoIndex, nestAfterLevel, nestingDepth.getFillUpExisting(), false));
			return;
			
		} else if (levelOfFirstDiff == anno.getLevelCount()) {
			// this only happens in case of duplicate annotations
			return;
		}
		
		// optional cases (non-array, non-anonymous, independent of original code): 
		// determine whether nesting should be done at this level
		boolean allowed = false;
		if (nestAfterLevel != levelOfFirstDiff - 1) {
			// no nesting needed at this point
			return;
			
		} else if (levelOfFirstDiff >= nestingDepth.getMinDepth()) {
			// nesting allowed according to minNestingDepth
			allowed = true; 

		} // do NOT attach with else if!
		
		if (!allowed && allowAnnotations != null) {
			// check whether the annotation matches the allowList
			int allowMatchElemCountMin = anno.getMatchElemCountMin(allowAnnotations);
			if (levelOfFirstDiff >= allowMatchElemCountMin && allowMatchElemCountMin >= 0) {
				allowed = true; 
			}
		} // do NOT attach with else if!

		// block nesting if a match is found on the blockList (possibly overriding the allowList)
		if (allowed && blockAnnotations != null) {
			int blockMatchElemCountMin = anno.getMatchElemCountMin(blockAnnotations);
			if (levelOfFirstDiff >= blockMatchElemCountMin && blockMatchElemCountMin >= 0) {
				allowed = false; 
			}
		}
		
		if (allowed) {
			nestingRanges.add(createNestingRange(annotations, annoIndex, nestAfterLevel, nestingDepth.getFillUpExisting(), false));
		}
	}
	
	/** creates a nesting range for all surrounding annotations with matching element names up to the supplied level */
	private DdlAnnotationNestingRange createNestingRange(ArrayList<DdlAnnotation> annotations, int annoIndex, int nestAfterLevel, boolean fillUpExisting, boolean isArray) {
		DdlAnnotation annotation = annotations.get(annoIndex);

		// determine the index range of the annotations that start with the same elements 
		int startIndex = annoIndex - 1;
		while (startIndex >= 0 && annotation.canNestingInclude(annotations.get(startIndex), nestAfterLevel, fillUpExisting)) {
			--startIndex;
		}
		++startIndex;

		int endIndex = annoIndex + 1;
		while (endIndex < annotations.size() && annotation.canNestingInclude(annotations.get(endIndex), nestAfterLevel, fillUpExisting)) {
			++endIndex;
		}

		// create add all annotations (in sorted order) to the nesting range
		DdlAnnotationNestingRange nestingRange = new DdlAnnotationNestingRange(nestAfterLevel, isArray);
		for (int addIndex = startIndex; addIndex < endIndex; ++addIndex) {
			nestingRange.addAnnotation(annotations.get(addIndex), addIndex);
		}
		return nestingRange;
	}

	public boolean determineOneLiners(DdlAnnotationLayout layout) throws UnexpectedSyntaxException, ParseException  {
		int maxOneLinerElemCount = belongsToMainAnnotations() ? layout.maxOneLinerElemCountMain : layout.maxOneLinerElemCountList; 
		if (maxOneLinerElemCount < 2)
			return false;
		
		// set .writeAsOneLiner = true for all applicable nesting range candidates
		boolean foundCandidate = false;
		for (DdlAnnotationNestingRange nestingRange : nestingRanges) {
			nestingRange.writeAsOneLiner = nestingRange.isOneLinerCandidate(maxOneLinerElemCount); 
			if (nestingRange.writeAsOneLiner) {
				foundCandidate = true;
			} 
		}
		if (!foundCandidate)
			return false;
		
		// simulate writing the annotations
		DdlAnnotationSimWriter simWriter = new DdlAnnotationSimWriter(layout, getMaxLevelCount());
		writeTo(simWriter);
		
		// determine which nesting ranges can keep .writeAsOneLiner = true
		boolean keptCandidate = false;
		for (DdlAnnotationNestingRange nestingRange : nestingRanges) {
			if (nestingRange.writeAsOneLiner) {
				int nestedLevel = nestingRange.nestAfterLevel + 1;
				int maxEndIndex = 0;
				for (DdlAnnotation annotation : nestingRange.annotations) {
					maxEndIndex = Math.max(maxEndIndex, annotation.getEndIndexInLine(nestedLevel));
					maxEndIndex = Math.max(maxEndIndex, annotation.getValueEndIndexInLine());
				}
				if (maxEndIndex > layout.maxLineLength) {
					nestingRange.writeAsOneLiner = false;
				} else {
					keptCandidate = true;
				}
			}
		}
		
		return keptCandidate;
	}
	
	public void determineTablesInArrays() {
		for (DdlAnnotationNestingRange nestingRange : nestingRanges) {
			if (nestingRange.isArray && nestingRange.maxInnerElemDepth <= 2 && nestingRange.maxInnerNestingDepth == 1) {
				determineTablesInArrays(nestingRange);
			}
		}
	}
	
	private boolean determineTablesInArrays(DdlAnnotationNestingRange nestingRange) {
		// determines whether 'tabular' alignment is possible inside arrays that contain nested objects with the same elements:
		// @Annotation: [ { name = 'any'       value = '1' }
		//                { name = 'other'     value = '2' }
		//                { name = 'long name' value = '3' }

		// to do this, 
		// - check whether the array consists of at least two nested objects (and nothing else);
		// - check whether these nested objects all have the same element names in the same sequence; 
		// - build a list of maximum value widths for each column (the element names anyway have the same width)
		// - store them on the respective DdlAnnotation.valueColumnWidth
		
		int levelInArray = nestingRange.nestAfterLevel + 1;
		String firstAnonymousObjName = null;
		String lastAnonymousObjName = null;
		int index = 0;
		int lastAnonymousObjStart = 0;

		ArrayList<String> columnNames = new ArrayList<>();
		ArrayList<Integer> maxValueWidths = new ArrayList<>();
		
		for (DdlAnnotation annotation : nestingRange.annotations) {
			if (!annotation.hasAnonymousArrayObjectAt(levelInArray) || !annotation.continueOnSameLine(levelInArray + 1)) 
				return false;
			else if (annotation.getValue() == null)
				return false;

			String anonymousObjName = annotation.getElementName(levelInArray);
			String columnName = annotation.getElementName(levelInArray + 1);
			int valueWidth = annotation.getValue().length();
			
			if (firstAnonymousObjName == null || firstAnonymousObjName.equals(anonymousObjName)) {
				// create a list of element names from the first anonymous object
				firstAnonymousObjName = anonymousObjName;
				lastAnonymousObjName = anonymousObjName;
				columnNames.add(columnName);
				maxValueWidths.add(valueWidth);
			
			} else {
				// for all following anonymous objects, check the element names and their sequence 
				// against those of the first anonymous object; allow following anonymous objects to have fewer elements
				if (lastAnonymousObjName == null || !lastAnonymousObjName.equals(anonymousObjName)) {
					lastAnonymousObjName = anonymousObjName;
					lastAnonymousObjStart = index;
				}
				int indexInObj = index - lastAnonymousObjStart;
				if (indexInObj >= columnNames.size() || !columnName.equals(columnNames.get(indexInObj))) 
					return false;

				// determine the maximum value width for each 'column'
				int maxValueWidth = Math.max(maxValueWidths.get(indexInObj), valueWidth);
				maxValueWidths.set(indexInObj, maxValueWidth);
			}
			++index;
		}
		if (columnNames.size() < 2) 
			return false;

		// store the maximum widths that were determined above on all involved annotations
		lastAnonymousObjName = null;
		int indexInObj = 0;
		for (DdlAnnotation annotation : nestingRange.annotations) {
			String anonymousObjName = annotation.getElementName(levelInArray);
			if (lastAnonymousObjName == null || !lastAnonymousObjName.equals(anonymousObjName)) {
				lastAnonymousObjName = anonymousObjName;
				indexInObj = 0;
			} else {
				++indexInObj;
			}
			annotation.valueColumnWidth = maxValueWidths.get(indexInObj);
		}
		return true;
	}

	public void writeTo(DdlAnnotationWriter writer) throws UnexpectedSyntaxException, ParseException {
		// write the changed annotations (in different sequence, with newly determined nesting etc.)
		for (int annoIndex = 0; annoIndex <= annotations.size(); ++annoIndex) {
			writeTo(writer, annoIndex, false);
		}
	}
	
	private void writeTo(DdlAnnotationWriter writer, int annoIndex, boolean ignorePrevAnno) throws UnexpectedSyntaxException, ParseException {
		boolean isAtEnd = (annoIndex == annotations.size());
		DdlAnnotation prevAnno = (ignorePrevAnno || annoIndex == 0) ? null : annotations.get(annoIndex - 1);
		DdlAnnotation annotation = isAtEnd ? null : annotations.get(annoIndex);

		// close brackets and braces from the previous annotation, if needed
		int nestingStartLevel = 0;
		boolean startsNewMainElem = false;
		if (prevAnno != null) {
			int firstDiffLevel = (isAtEnd ? 0 : annotation.getLevelOfFirstDiff(prevAnno));
			startsNewMainElem = (firstDiffLevel == 0);

			// depending on configuration, nesting might start earlier than the firstDiffLevel
			// (e.g. if nesting is switched off, it might only start at an anonymous object in an array, or for values directly in the array; 
			// if DdlAnnotationNestingDepth.KEEP_AS_IS was selected, even existing nesting may not be filled up with all possible annotations)
			// this already includes the condition "while (nestingStartLevel > 0 && !prevAnno.startsNestingAfterLevel(nestingStartLevel - 1) && !prevAnno.hasArrayStartAt(nestingStartLevel - 1)) ..."
			nestingStartLevel = firstDiffLevel;
			while (nestingStartLevel > 0 && !prevAnno.isNestedWith(annotation, nestingStartLevel - 1)) {
				--nestingStartLevel;
			}
			
			// close the brackets and braces down to the nestingStartLevel
			int level = prevAnno.getLevelCount() - 1;
			while (level >= nestingStartLevel) {
				if (prevAnno.hasArrayStartAt(level)) {
					writer.closeBracket();
				} else if (prevAnno.hasAnonymousArrayObjectAt(level)) {
					writer.closeBrace();
				} else if (prevAnno.startsNestingAfterLevel(level)) {
					writer.closeBrace();
				}
				--level;
			}

			// start a new line or even a new command
			writer.endAnnotation(prevAnno.getValueToken().getParentCommand());
			if (!prevAnno.startsNestingOrArrayBefore(nestingStartLevel)) {
				writer.finishCommand();
				nestingStartLevel = 0;
			} else {
				boolean continueOnLine = prevAnno.continueOnSameLine(nestingStartLevel);
				writer.addComma(nestingStartLevel, continueOnLine);
			}
		}

		// stop here if the end was reached
		if (isAtEnd) 
			return;

		if (nestingStartLevel == 0) {
			writer.startCommand(willCommandBeMultiLine(annoIndex));
		}
		
		// unless merely an extra value must be added to an array, add required annotation levels 
		boolean addDot = false;
		writer.startAnnotation(annotation.getValueToken(), annotation.getOriginalLineBreaks(), startsNewMainElem);
		for (int level = nestingStartLevel; level < annotation.getLevelCount(); ++level) {
			writer.startLevel(level);
			
			if (!annotation.hasAnonymousArrayObjectAt(level)) {
				if (level == 0)
					writer.addAnnotationSign();
				else if (addDot)
					writer.addDot();
				int endIndex = writer.addAnnoElemName(annotation.getElementName(level), annotation.getColumnWidth(level));
				annotation.setEndIndexInLine(level, endIndex);
			}

			// open bracket or brace
			addDot = false;
			if (annotation.startsNestingAfterLevel(level)) {
				if (!annotation.hasAnonymousArrayObjectAt(level)) {
					writer.addColon();
				}
			
				if (annotation.hasArrayStartAt(level)) {
					writer.openBracket();
				} else {
					writer.openBrace();
				}
			} else {
				addDot = true;
			}
		}
		
		// write the value
		if (annotation.getValue() != null) {
			if (nestingStartLevel >= 0 && !annotation.isValueInArray()) {
				writer.addColon();
			}
			writer.startLevel(annotation.getLevelCount());
			int endIndex = writer.addValue(annotation.getValue(), annotation.getValueColumnWidth()); 
			annotation.setValueEndIndexInLine(endIndex);
		}
	}

	private boolean willCommandBeMultiLine(int startAnnoIndex) {
		for (int annoIndex = startAnnoIndex + 1; annoIndex < annotations.size(); ++annoIndex) {
			DdlAnnotation prevAnno = annotations.get(annoIndex - 1);
			DdlAnnotation annotation = annotations.get(annoIndex);
			
			int firstDiffLevel = annotation.getLevelOfFirstDiff(prevAnno);

			// depending on configuration, nesting might start earlier than the firstDiffLevel
			int nestingStartLevel = firstDiffLevel;
			while (nestingStartLevel > 0 && !prevAnno.isNestedWith(annotation, nestingStartLevel - 1)) {
				--nestingStartLevel;
			}

			if (!prevAnno.startsNestingOrArrayBefore(nestingStartLevel)) {
				return false;
			} else if (!prevAnno.continueOnSameLine(nestingStartLevel)) {
				return true;
			} // otherwise, continue the loop
		}
		return false;
	}
	
	public void transferResults(DdlAnnotationCommandWriter writer, Code code, Rule rule) throws UnexpectedSyntaxAfterChanges {
		ArrayList<Command> newCommands = writer.getNewCommands();

		// check the changed Commands against the existing ones and insert changes
		Command insertBefore = firstCommand;
		Command offsetCommand = firstCommand.getPrev(); // inserting must not happen before the offsetCommand 
		Command endCommand = lastCommand.getNextNonCommentCommand(); // remember this now, before scope.lastCommand may get detached from the context
		for (Command newCommand : newCommands) {
			HashSet<Command> originalCommands = writer.getOriginalCommandsOf(newCommand);

			// keep the existing Command if it is similar to the new Command, with possible differences only in whitespace 
			if (newCommand.toStringWithoutWhitespace().equals(insertBefore.toStringWithoutWhitespace())) {
				if (transferWhitespace(newCommand, insertBefore, rule))
					code.addRuleUse(rule, insertBefore);
				insertBefore = insertBefore.getNextNonCommentCommand();
				continue;
			} else {
				// remove all original Commands that were (partly) moved to newCommand, because they will anyway be removed; 
				// maybe the Command after them can then be preserved if it matches the next newCommand
				while (originalCommands.contains(insertBefore) && insertBefore.getNextNonCommentCommand() != null) {
					insertBefore = deleteOldCommandAndReturnNext(code, insertBefore, rule);
				}
			}
			Command insertBeforeComment = (insertBefore.getPrev() == offsetCommand) ? insertBefore : insertBefore.getStartOfAttachedComments();
			// if newCommand originates from the very first line in the code document, it could start with 0 line breaks
			if (insertBeforeComment.getPrev() != null && newCommand.getFirstTokenLineBreaks() == 0)
				newCommand.getFirstToken().setLineBreaks(1);
			
			// insert the new Command 
			insertBeforeComment.insertLeftSibling(newCommand);
			code.addRuleUse(rule, newCommand);
		}

		// delete the remaining Commands that were neither matched nor already deleted 
		while (insertBefore != endCommand) {
			insertBefore = deleteOldCommandAndReturnNext(code, insertBefore, rule);
		}
	}

	private Command deleteOldCommandAndReturnNext(Code code, Command obsoleteCommand, Rule rule) throws UnexpectedSyntaxAfterChanges {
		Command nextCommand = obsoleteCommand.getNextNonCommentCommand();
		try {
			code.addRuleUse(rule, obsoleteCommand);
			obsoleteCommand.removeFromCode();
		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxAfterChanges(rule, e);
		}
		return nextCommand;
	}

	private boolean transferWhitespace(Command sourceCommand, Command targetCommand, Rule rule) throws UnexpectedSyntaxAfterChanges {
		boolean changed = false;
		Token source = sourceCommand.getFirstCodeToken();
		Token target = targetCommand.getFirstCodeToken();

		while (source != null && target != null) {
			changed |= target.copyWhitespaceFrom(source);

			if (source.getTextLength() > target.getTextLength()) {
				// the targetCommand may have "@ Annotation . subAnno . subSubAnno" in multiple Tokens from the original code,
				// which the rearranged code in sourceCommand will have as "@Annotation.subAnno.subsubAnno"
				String sourceTextToMatch = source.getText();
				StringBuilder sbTarget = new StringBuilder();
				do {
					sbTarget.append(target.getText());
					if (!StringUtil.startsWith(sourceTextToMatch, target.getText(), true))
						throw new UnexpectedSyntaxAfterChanges(rule, targetCommand, "Cannot transfer whitespace: text mismatch between '" + source.getText() + "' and '" + sbTarget.toString() + "'.");
					sourceTextToMatch = sourceTextToMatch.substring(target.getTextLength());
					if (sourceTextToMatch.length() == 0)
						break;
					target = target.getNext();
					target.setWhitespace(0, 0); // condense to "@Annotation.subAnno.subsubAnno", as sourceCommand has it
				} while (true);
				
			} else if (!source.textEquals(target.getText())) {
				throw new UnexpectedSyntaxAfterChanges(rule, targetCommand, "Cannot transfer whitespace: text mismatch between '" + source.getText() + "' and '" + target.getText() + "'.");
			}

			source = source.getNext();
			target = target.getNext();
		}
		return changed;
	}
	
	public DdlAnnotation findAnnotation(String path) {
		for (DdlAnnotation annotation : annotations) {
			if (annotation.getPath().equals(path)) {
				return annotation;
			}
		}
		return null;
	}
}
