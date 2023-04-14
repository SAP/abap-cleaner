package com.sap.adt.abapcleaner.comparer;

import com.sap.adt.abapcleaner.parser.*;

/**
 * <p>Represents a section of code text inside a {@link DisplayLine} which can be printed with the same {@link ColorType}. 
 * The {@link #start} and {@link #length} information refers to {@link DisplayLine#getText()}.</p>
 */
public class TextBit {
   public final int start;
   public int length;
   public final ColorType type;

   public final int getEnd() {
      return start + length;
   }

   public static TextBit create(int start, int length, ColorType type) {
   	return new TextBit(start, length, type);
   }
   
   public static TextBit createFromModel(TextBit model) {
      if (model == null)
         throw new NullPointerException("model");

      return new TextBit(model);
   }
   
   private TextBit(int start, int length, ColorType type) {
      this.start = start;
      this.length = length;
      this.type = type;
   }

   private TextBit(TextBit model) {
      start = model.start;
      length = model.length;
      type = model.type;
   }
}