// on-the-fly test function, can be called temporarily from BinaryManager.testDaemon()
import * as path from 'path';
import { CLI } from "./CLI";
import { DaemonClient } from "./DaemonClient";
import { Logger } from "./Logger";

export async function testDaemon(client: DaemonClient): Promise<void> {
   const DIR = path.join(__dirname, '..', 'samples') + path.sep;
   const FILE1_INPUT = "test-1-input.abap";
   const FILE2_INPUT = "test-2-input.abap";
   const FILE2_OUTPUT = "test-2-output.abap";
   const FILE1_LINE_RANGE = "5-20";

   const PROFILE_NAME_STRESS_TEST = "stress test";
   const TEST_TITLE = "Test Title";
   const ARROW = " -> ";

   try {
      // ping the daemon
      const pingResponse = await client.sendCommand(DaemonClient.ARG_PING);
      Logger.info(DaemonClient.ARG_PING + ARROW + pingResponse);

      // query daemon status
      const statusResponse = await client.sendCommand(DaemonClient.ARG_STATUS);
      Logger.info(DaemonClient.ARG_STATUS + ARROW + statusResponse);

      // send keepalive
      const keepAliveResponse = await client.sendCommand(DaemonClient.ARG_KEEP_ALIVE);
      Logger.info(DaemonClient.ARG_KEEP_ALIVE + ARROW + keepAliveResponse);

      // query version
      const versionResponse = await client.sendArgs([CLI.ARG_VERSION]);
      Logger.info(CLI.ARG_VERSION + ARROW + versionResponse);

      // query help
      const helpArg = CLI.ARG_HELP_WINDOWS; // CLI.ARG_HELP_LINUX works the same
      const helpResponse = await client.sendArgs([helpArg]);
      Logger.info(helpArg + ARROW + helpResponse);

      // test starting another daemon (expected to fail)
      const daemonizeResponse = await client.sendArgs([CLI.ARG_DAEMONIZE]);
      Logger.info(CLI.ARG_DAEMONIZE + ARROW + daemonizeResponse);

      // test cleaning source code directly
      const cleanupResponse1 = await client.sendArgs([CLI.ARG_SOURCE_CODE, "REPORT any_report. DATA: a TYPE i. DATA  b  TYPE string."]);
      Logger.info(CLI.ARG_SOURCE_CODE + ARROW + cleanupResponse1);

      // test cleaning a file
      const cleanupResponse2 = await client.sendArgs([CLI.ARG_SOURCE_FILE, `${DIR}${FILE1_INPUT}`]);
      Logger.info(CLI.ARG_SOURCE_FILE + ARROW + cleanupResponse2);

      // test cleaning with additional args
      const cleanupResponse3 = await client.sendArgs([CLI.ARG_SOURCE_FILE, `${DIR}${FILE1_INPUT}`, CLI.ARG_LINE_RANGE, FILE1_LINE_RANGE, CLI.ARG_SCOPE, CLI.VALUE_SCOPE_METHOD, CLI.ARG_PROFILE_NAME, PROFILE_NAME_STRESS_TEST, CLI.ARG_STATS, CLI.ARG_USED_RULES]);
      Logger.info(CLI.ARG_LINE_RANGE + ARROW + cleanupResponse3);

      // test cleaning a file with a target file
      const cleanupResponse4 = await client.sendArgs([CLI.ARG_SOURCE_FILE, `${DIR}${FILE2_INPUT}`, CLI.ARG_TARGET_FILE, `${DIR}${FILE2_OUTPUT}`, CLI.ARG_OVERWRITE, CLI.ARG_PROFILE_NAME, PROFILE_NAME_STRESS_TEST, CLI.ARG_STATS, CLI.ARG_USED_RULES]);
      Logger.info(CLI.ARG_TARGET_FILE + ARROW + cleanupResponse4);

      // test interactive cleanup
      const interactiveCleanup = await client.sendArgs([CLI.ARG_SOURCE_FILE, `${DIR}${FILE1_INPUT}`, CLI.ARG_INTERACTIVE, CLI.ARG_TITLE, TEST_TITLE, CLI.ARG_DARK_THEME]);
      Logger.info(CLI.ARG_INTERACTIVE + ARROW + interactiveCleanup);

      // test readonly interactive cleanup
      const readOnlyCleanup = await client.sendArgs([CLI.ARG_SOURCE_FILE, `${DIR}${FILE1_INPUT}`, CLI.ARG_INTERACTIVE, CLI.ARG_TITLE, TEST_TITLE, CLI.ARG_READ_ONLY]);
      Logger.info(CLI.ARG_READ_ONLY + ARROW + readOnlyCleanup);

      // stop daemon to test restarting it when this test function is called again
      const stopResult = await client.sendArgs([DaemonClient.ARG_STOP]);
      Logger.info(DaemonClient.ARG_STOP + ARROW + stopResult);

   } catch (err) {
      Logger.error(`Daemon communication failed: ${err}`);
   }
}
