// Runs one test of `fe test --backend wasm` in its own Node.js process.
//
//   node run_test.mjs <module.wasm> <export>
//
// The module is instantiated with the host of `examples/wasm/host.mjs` (the
// file next to this one), the export is called, and the host is checked for
// handles that the test left open. Exit status 0 means the test passed. On a
// failure one line on standard error says what happened, and the status is 1.
// Standard output carries the numbers that the test logged, one per line.
import fs from "node:fs";
import { ContractError, GuestAbort, instantiate } from "./host.mjs";

function describe(error) {
  if (error instanceof WebAssembly.RuntimeError) {
    return `trap: ${error.message} (Fe code traps on a failed assertion, arithmetic overflow, division by zero and an index outside an array)`;
  }
  if (error instanceof GuestAbort) {
    return `the test aborted with code ${error.code}`;
  }
  if (error instanceof ContractError) {
    return `host contract violation: ${error.message}`;
  }
  if (error instanceof RangeError) {
    return `the test ran out of stack: ${error.message}`;
  }
  return String(error?.stack ?? error);
}

const [, , modulePath, entry] = process.argv;
if (!modulePath || !entry) {
  console.error("usage: node run_test.mjs <module.wasm> <export>");
  process.exit(2);
}

let guest = null;
let failure = null;
try {
  guest = await instantiate(fs.readFileSync(modulePath));
  guest.call(entry);
  guest.host.assertNoOpenHandles();
} catch (error) {
  failure = describe(error);
}
for (const entry of guest?.host.logs ?? []) {
  console.log(`log ${entry.type} ${entry.value}`);
}
if (failure !== null) {
  console.error(failure);
  process.exit(1);
}
