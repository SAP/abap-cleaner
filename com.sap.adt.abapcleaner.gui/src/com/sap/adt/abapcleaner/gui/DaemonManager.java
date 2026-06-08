package com.sap.adt.abapcleaner.gui;

import java.io.*;
import java.net.*;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.CommandLineArgs;
import com.sap.adt.abapcleaner.programbase.Program;

/**
 * Handles daemonization of the application: finds a free port, prints it to stdout,
 * and starts listening for commands on that port.
 */
public class DaemonManager {
	private ServerSocket serverSocket;
	private Thread listenerThread;
	private volatile boolean running = false;

	/** idle time before auto-shutdown; 0 = no timeout */
	private int idleTimeOut_s;

	private LocalDateTime startTime;
	private LocalDateTime lastCommandTime;

	/**
	 * the number of currently active command handlers; used to prevent auto-shutdown while long-running commands
	 * (esp. interactive cleanup) are still being processed
	 */
	private int activeHandlerCount = 0;

	/**
	 * called when --daemonize is sent via CLI (see {@link CommandLineArgs})
	 */
	public void startDaemon(PrintStream out, int idleTimeOut_s) throws IOException {
		// bind to port 0; the OS assigns a free port automatically
		serverSocket = new ServerSocket(0);
		int assignedPort = serverSocket.getLocalPort();

		this.idleTimeOut_s = idleTimeOut_s;
		startTime = LocalDateTime.now();
		lastCommandTime = startTime;

		listenerThread = new Thread(this::acceptLoop, Program.PRODUCT_NAME + " daemon-acceptor");
		listenerThread.setDaemon(true);
		listenerThread.start();
		running = true;

		// print the port, so the caller can read it
		out.println(assignedPort);
		out.flush();
	}

	private void acceptLoop() {
		while (running) {
			try {
				Socket clientSocket = serverSocket.accept();

				// handle each client in its own thread
				Thread clientThread = new Thread(() -> handleClient(clientSocket), Program.PRODUCT_NAME + " daemon-client-" + clientSocket.getPort());
				clientThread.setDaemon(true);
				clientThread.start();

			} catch (IOException e) {
				if (running) {
					printErr("Accept error: " + e.getMessage());
				}
			}
		}
	}

	private void handleClient(Socket clientSocket) {
		try (BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
				// PrintWriter out = new PrintWriter(new OutputStreamWriter(clientSocket.getOutputStream()), true)
				PrintStream out = new PrintStream(clientSocket.getOutputStream(), true)) {

			// printInfo("Client connected: " + clientSocket.getRemoteSocketAddress());

			String line;
			while ((line = in.readLine()) != null) {
				++activeHandlerCount;
				processCommand(line, out);
				--activeHandlerCount;
			}

		} catch (IOException e) {
			printErr("Client error: " + e.getMessage());

		} finally {
			try {
				clientSocket.close();
			} catch (IOException ignored) {
			}
			printInfo("Client disconnected.");
		}
	}

	/**
	 * Dispatch commands received over the socket.
	 */
	private void processCommand(String command, PrintStream out) {
		if (command == null) {
			printErr("ERROR: null command");
			return;
		}
		updateLastCommandTime();

		String[] args = StringUtil.splitArgs(command);
		
		// extract the request ID, if any; the request ID will be added to the end marker of the response,
		// so the caller can map the response to the request
		List<String> argsWithoutRequestId = new ArrayList<>();
		String requestId = null;
		for (int i = 0; i < args.length; i++) {
			if (requestId == null && args[i].equals(CommandLineArgs.OPT_DAEMON_REQUEST_ID) && i + 1 < args.length) {
				requestId = args[i + 1];
				++i; // skip the request ID value in argsWithoutRequestId
			} else {
				argsWithoutRequestId.add(args[i]);
			}
		}
		if (requestId != null) {
			args = argsWithoutRequestId.toArray(new String[0]);
		}

		// for testing: 
		// String requestIdInfo = (requestId != null) ? "#" + requestId : "";
		// printInfo("Received " + requestIdInfo + ": " + command); 

		// if the command (without request ID) is a single argument, it might be a daemon control command;
		// otherwise, treat it as a CLI command
		String onlyArg = (args.length == 1) ? args[0] : "";
		String response = null;
		switch (onlyArg) {
			case CommandLineArgs.OPT_DAEMON_PING:
				response = "pong";
				break;

			case CommandLineArgs.OPT_DAEMON_STATUS:
				// do not call updateLastCommandTime(), as --status should be a neutral observation without side effects
				String status = "Status: daemon running since " + startTime;
				status += ", idle for " + java.time.Duration.between(lastCommandTime, LocalDateTime.now()).getSeconds() + " seconds";
				status += (idleTimeOut_s > 0) ? " (idle timeout: " + idleTimeOut_s + " seconds)" : " (no idle timeout)";
				response = status;
				break;
				
			case CommandLineArgs.OPT_DAEMON_KEEPALIVE:
				String idleInfo = (idleTimeOut_s > 0) ? " (idle timeout: " + idleTimeOut_s + " seconds)" : " (no idle timeout)";
				response = "OK: idle timer reset" + idleInfo;
				break;

			case CommandLineArgs.OPT_DAEMON_STOP:
				stopDaemon();
				response = "OK: stopped daemon";
				break;

			default: // treat as a CLI command
				FrmMain.handleCLI(args, true, out, System.err);
				// a final out.println(); should already be ensured by FrmMain.handleCLI()
				response = null;
				break;
		}
		printResponse(response, out, requestId);
		updateLastCommandTime();

		// for testing: 
		// printInfo("Processed " + requestIdInfo + ": " + command);
	}

	private void updateLastCommandTime() {
		lastCommandTime = LocalDateTime.now();
	}

	/** returns true while the daemon is running; checks for idle timeout */
	public boolean isRunning() {
		if (running && idleTimeOut_s > 0 && activeHandlerCount == 0) {
			// check for timeout - isRunning() is regularly polled by FrmMain.handleCLI() while the daemon is running
			LocalDateTime now = LocalDateTime.now();
			if (lastCommandTime.plusSeconds(idleTimeOut_s).isBefore(now)) {
				printInfo("Daemon timed out after " + idleTimeOut_s + " seconds inactivity.");
				stopDaemon();
			}
		}

		return running;
	}

	public void stopDaemon() {
		running = false;
		try {
			if (serverSocket != null && !serverSocket.isClosed()) {
				serverSocket.close();
			}
		} catch (IOException e) {
			printErr("Error closing server socket: " + e.getMessage());
		}
	}

	private void printResponse(String message, PrintStream out, String requestId) {
		if (!StringUtil.isNullOrEmpty(message)) {
			out.println(message);
		}

		// if a request ID was provided, add it to the end marker, so the caller can map the response to the request
		String endMarker = CommandLineArgs.DAEMON_RESPONSE_END_MARKER;
		if (requestId != null) {
			endMarker += " " + requestId;
		}
		out.println(endMarker);
		out.flush();
	}

	private void printInfo(String message) {
		// reuse System.err for info messages to keep stdout to the actual output of CLI commands
		printErr(message);
	}

	private void printErr(String message) {
		System.err.println(message);
	}
}
