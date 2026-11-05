// Checks a module against its build manifest the way a host does before it
// runs the module: the SHA-256 and the imports and exports must match the
// manifest, and the host of `host.mjs` must provide every import.
//
//   node examples/wasm/verify.mjs lines.wasm [lines.manifest.json]
//
// Without a second argument the manifest next to the module (`<name>.manifest.json`)
// is used if there is one. The exit status is 0 if the module is accepted and 1
// if it is refused; the reason is printed.
import fs from "node:fs";
import { checkModule, createHost, parseManifest } from "./host.mjs";

const [, , modulePath, manifestArgument] = process.argv;
if (!modulePath) {
  console.error("usage: node verify.mjs <module.wasm> [<manifest.json>]");
  process.exit(2);
}
const sibling = modulePath.replace(/\.wasm$/, ".manifest.json");
const manifestPath = manifestArgument ?? (fs.existsSync(sibling) ? sibling : null);

try {
  const manifest = manifestPath === null ? null : parseManifest(fs.readFileSync(manifestPath, "utf8"));
  const module = await checkModule(fs.readFileSync(modulePath), createHost().imports, manifest);
  const imports = WebAssembly.Module.imports(module).length;
  const exports = WebAssembly.Module.exports(module).length;
  console.log(
    `${modulePath}: accepted (${imports} import(s), ${exports} export(s), ` +
      (manifest === null ? "no manifest" : `manifest ${manifestPath}`) +
      ")",
  );
} catch (error) {
  console.error(`${modulePath}: refused: ${error.message}`);
  process.exit(1);
}
