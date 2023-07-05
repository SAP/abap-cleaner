package com.sap.adt.abapcleaner.parser;

public enum MemoryAccessType {
	/** the MemoryAccessType was not yet determined or was invalidated */
	UNKNOWN(false, false, false),
	/** MemoryAccessType is not applicable to this Token */
	NONE(false, false, false),
	/** the Token is in a read position */
	READ(true, false, false),
	/** the Token is in a write position */
	WRITE(false, true, true),
	/** the Token is in a read and write position (esp. CHANGING parameters) */
	READ_WRITE(true, true, true),
	/** the Token is in a read position, but write is not prevented by the syntax check (PERFORM ... USING ...) */
	READ_WRITE_POSSIBLE(true, true, false),
	/** the Token is either in a read position, or MemoryAccessType is not applicable */
	READ_OR_NONE(true, false, false),
	/** the Token is either a field-symbol or a data reference in a position where it is declared, assigned to a memory area, or unassigned */
	ASSIGN_TO_FS_OR_DREF(false, true, true);
	
	public final boolean mayRead;
	public final boolean mayWrite;
	public final boolean displayAsWritePos;
	
	MemoryAccessType(boolean mayRead, boolean mayWrite, boolean displayAsWritePos) {
		this.mayRead = mayRead;
		this.mayWrite = mayWrite;
		this.displayAsWritePos = displayAsWritePos;
	}
}
