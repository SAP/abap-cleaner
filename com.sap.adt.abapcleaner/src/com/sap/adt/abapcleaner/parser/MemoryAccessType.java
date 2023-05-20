package com.sap.adt.abapcleaner.parser;

public enum MemoryAccessType {
	/** the MemoryAccessType was not yet determined or was invalidated */
	UNKNOWN(false, false),
	/** MemoryAccessType is not applicable to this Token */
	NONE(false, false),
	/** the Token is in a read position */
	READ(false, false),
	/** the Token is in a write position */
	WRITE(true, true),
	/** the Token is in a read and write position (esp. CHANGING parameters) */
	READ_WRITE(true, true),
	/** the Token is in a read position, but write is not prevented by the syntax check (PERFORM ... USING ...) */
	READ_WRITE_POSSIBLE(true, false),
	/** the Token is either in a read position, or MemoryAccessType is not applicable */
	READ_OR_NONE(false, false),
	/** the Token is either a field-symbol or a data reference in a position where it is declared, assigned to a memory area, or unassigned */
	ASSIGN_TO_FS_OR_DREF(true, true);
	
	public final boolean mayWrite;
	public final boolean displayAsWritePos;
	
	MemoryAccessType(boolean mayWrite, boolean displayAsWritePos) {
		this.mayWrite = mayWrite;
		this.displayAsWritePos = displayAsWritePos;
	}
}
