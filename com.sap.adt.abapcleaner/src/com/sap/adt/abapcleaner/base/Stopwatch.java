package com.sap.adt.abapcleaner.base;

import java.time.*;

/**
 * Provides stopwatch functionality for measuring elapsed time. 
 */
public class Stopwatch {
	private LocalDateTime start = LocalDateTime.MIN;
	private boolean running;

	public final boolean isRunning() { return running; }

	public static Stopwatch createAndStart() {
		return new Stopwatch();
	}
	
	private Stopwatch() {
		resetAndStart();
	}

	public final void resetAndStart() {
		start = LocalDateTime.now();
		running = true;
	}

	public final void stop() {
		running = false;
	}

	public final int getElapsedTimeMs() { 
		return (int) Duration.between(start, LocalDateTime.now()).toMillis(); 
	}
}