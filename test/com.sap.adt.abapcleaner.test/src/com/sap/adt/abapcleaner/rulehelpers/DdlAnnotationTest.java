package com.sap.adt.abapcleaner.rulehelpers;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.ArrayList;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.ParseParams;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;

public class DdlAnnotationTest {
	private ArrayList<DdlAnnotation> annotations;

	DdlAnnotation getFirstAnnotation(String sourceCode) {
		// parse the source code
		Code code;
		try {
			code = Code.parse(null, ParseParams.createForTest(sourceCode, ABAP.NEWEST_RELEASE));
		} catch (ParseException e) {
			fail(e.getMessage());
			return null;
		}

		// test the referential integrity of the parse result
		try {
			code.testReferentialIntegrity(true);
		} catch (IntegrityBrokenException e1) {
			fail("Error after parsing source code:" + e1.getMessage());
			return null;
		} 
		
		// create the DdlAnnotationScope
		DdlAnnotationScope scope = new DdlAnnotationScope(true);
		Command command = code.firstCommand;
		while (command != null) {
			try {
				scope.add(command);
			} catch (UnexpectedSyntaxBeforeChanges e) {
				fail();
			}
			command = command.getNext();
		}
		scope.finishBuild();
		scope.useExistingNesting();
		
		annotations = scope.getAnnotations();
		return annotations.get(0);
	}
	
	@Test
	void testAnnotationWithArray() {
		DdlAnnotation annotation = getFirstAnnotation("@ ObjectModel . text. element :['AnyText' ]");

		assertEquals(3, annotation.getLevelCount());
		assertEquals("'AnyText'", annotation.getValue());
		assertEquals("'AnyText'", annotation.getValueToken().getText());
		assertEquals(0, annotation.getOriginalLineBreaks());
		
		assertEquals("0: @ObjectModel.text.element: [ 'AnyText'", annotation.toString());
	}
	
	@Test
	void testAnnotationWithBraces() {
		DdlAnnotation annotation = getFirstAnnotation("@ObjectModel: { usageType: { sizeCategory: #XXL } }");

		assertEquals(3, annotation.getLevelCount());
		assertEquals("#XXL", annotation.getValue());
		assertEquals("#XXL", annotation.getValueToken().getText());
		assertEquals(0, annotation.getOriginalLineBreaks());
		
		assertEquals("0: @ObjectModel: { usageType: { sizeCategory: #XXL", annotation.toString());
	}
	
	@Test
	void testBooleanAnnotationWithDefaultValue() {
		DdlAnnotation annotation = getFirstAnnotation("@Anno.subAnno");

		assertEquals(2, annotation.getLevelCount());
		assertNull(annotation.getValue());
		assertEquals("@Anno.subAnno", annotation.getValueToken().getText());
		assertEquals(0, annotation.getOriginalLineBreaks());
		
		assertEquals("0: @Anno.subAnno", annotation.toString());
	}
	
	@Test
	void testOutOfRangeCalls() {
		DdlAnnotation annotation = getFirstAnnotation("@ObjectModel.delegatedAction: [ { enabled: true } ]");
		
		assertFalse(annotation.startsNestingAfterLevel(-1));
		assertFalse(annotation.startsNestingAfterLevel(4));
		
		assertFalse(annotation.originalCodeStartedNestingAfterLevel(-1));
		assertFalse(annotation.originalCodeStartedNestingAfterLevel(4));

		assertFalse(annotation.hasAnonymousArrayObjectAt(-1));
		assertFalse(annotation.hasAnonymousArrayObjectAt(4));

		assertEquals("", annotation.getElementName(-1));
		assertEquals("", annotation.getElementName(4));
	}
}
