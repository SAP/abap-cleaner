package com.sap.adt.abapcleaner.parser;

public enum MemoryAccessType {
	/** the MemoryAccessType was not yet determined or was invalidated */
	UNKNOWN,
	/** MemoryAccessType is not applicable to this Token */
	NONE,
	/** the Token is in a read position */
	READ,
	/** the Token is in a write position */
	WRITE,
	/** the Token is in a read and write position (esp. CHANGING parameters) */
	READ_WRITE,
	/** the Token is either in a read position, or MemoryAccessType is not applicable */
	READ_OR_NONE,
	/** the Token is either a field-symbol or a data reference in a position where it is declared, assigned to a memory area, or unassigned */
	ASSIGN_TO_FS_OR_DREF,
}
