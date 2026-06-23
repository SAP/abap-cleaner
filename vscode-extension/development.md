# Notes for developers of the ABAP cleaner extension for VS Code

## Run And Debug

The repository root contains [.vscode/launch.json](../.vscode/launch.json) and [.vscode/tasks.json](../.vscode/tasks.json), 
so you can start the extension directly from VS Code by opening the project root.

Prerequisites:

- Node.js and npm
- Java 21+ on PATH
- A built abap-cleaner product on disk (see _Bundling the binary for development_ below)

Steps:

1. Run `npm install` once in [vscode-extension/](.).
2. Press `F5` and select `ext: Run Extension (watch)`.
3. A new **Extension Development Host** window opens. This is the target VS Code where your extension runs.
4. In that target window, open or create a `.abap` file (ABAP code) or an `.acds` (CDS View).
5. Trigger formatting via `Ctrl + 4` (Format Selection), `Ctrl + 5` / `Shift + Alt + F` (Format Document)
   or the `ABAP Cleaner: ...` commands from the Command Palette.

### Bundling the binary for development

The release workflow extracts the matching abap-cleaner archive into `vscode-extension/binary/` before packaging the VSIX. 
For local development you have two options:

1. **Point at a local Java build** — set the `ABAP_CLEANER_DEV_BINARY_PATH` env var (also configurable 
   in [../.vscode/launch.json](../.vscode/launch.json)) to the absolute path of an `abap-cleaner(.exe)` binary 
   you've built via `mvn package`.
2. **Copy the product into the bundle folder** — extract 
   `com.sap.adt.abapcleaner.app/target/products/com.sap.adt.abapcleaner.app-<platform>.tar.gz` (or `.zip`) 
   into `vscode-extension/binary/` so it ends up at `vscode-extension/binary/abapcleaner/...`. 
   The folder is gitignored.


## Packaging

To build a VSIX manually (single-platform, matching the host you run on):

```bash
npm install
npm run compile
# extract the matching abap-cleaner product into ./binary/abapcleaner/...
npx @vscode/vsce package --skip-license --allow-missing-repository
```

For the official multi-platform release, see [.github/workflows/create-release.yml](../.github/workflows/create-release.yml), 
which builds all four platform VSIX files alongside the standalone abap-cleaner archives in a single workflow run.
