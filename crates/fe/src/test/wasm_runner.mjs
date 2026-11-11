// Runs one test of `fe test --backend wasm` in its own Node.js process.
//
//   node run_test.mjs <module.wasm> <export> <output limit in bytes>
//
// The module is instantiated with the host of `examples/wasm/host.mjs` (the
// file next to this one), the export is called, and the host is checked for
// handles that the test left open. A test that logs and writes more than the
// output limit is stopped there, and `fe test` reports the limit. Exit status
// 0 means the test passed. On a
// failure one line on standard error says what happened, and the status is 1.
// A test that panics with a code or reverts with data ends with status 3 and
// the line `revert <data in hex>`: `fe test` decides whether that was expected.
// Standard output carries the numbers that the test logged, one per line, and
// then what it wrote to its standard output and standard error, each under a
// line that names the stream. The test's standard input is empty.
import fs from "node:fs";
import { ContractError, GuestAbort, instantiate } from "./host.mjs";

/** The test panicked with a code or reverted: `data` is the revert data. */
class TestRevert extends Error {
  constructor(data) {
    super("the test reverted");
    this.data = data;
  }
}

function describe(error) {
  if (error instanceof WebAssembly.RuntimeError) {
    return `trap: ${error.message}`;
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

const [, , modulePath, entry, limitArg] = process.argv;
const limit = Number(limitArg);
if (!modulePath || !entry || !Number.isSafeInteger(limit)) {
  console.error("usage: node run_test.mjs <module.wasm> <export> <output limit in bytes>");
  process.exit(2);
}

// The output is printed when the test ends, so the limit is kept as it grows.
let printed = 0;
const print = (length) => {
  printed += length;
  if (printed > limit) throw new Error("the test exceeded its output limit");
};

let guest = null;
let failure = null;
let revert = null;
// What `core::rt` imports in a test: the data of a panic or a revert goes to
// `__fe_revert`, as in a native test, and `abort` ends a test that cannot
// allocate that data.
const env = {
  __fe_revert(address, length) {
    const memory = new Uint8Array(guest.exports.memory.buffer);
    const start = address >>> 0;
    const end = start + (length >>> 0);
    if (end > memory.length) {
      throw new ContractError(`revert data at ${start} of length ${length >>> 0} lies outside the memory`);
    }
    throw new TestRevert(memory.slice(start, end));
  },
  abort() {
    throw new Error("the test ran out of memory while it reverted");
  },
};
try {
  guest = await instantiate(fs.readFileSync(modulePath), {
    imports: { env },
    onLog: ({ type, value }) => print(`log ${type} ${value}\n`.length),
    onWrite: ({ bytes }) => print(bytes.length),
  });
  guest.call(entry);
  guest.host.assertNoOpenHandles();
} catch (error) {
  if (error instanceof TestRevert) {
    revert = Buffer.from(error.data).toString("hex");
  } else {
    failure = describe(error);
  }
}
for (const entry of guest?.host.logs ?? []) {
  console.log(`log ${entry.type} ${entry.value}`);
}
for (const stream of ["stdout", "stderr"]) {
  const text = guest?.host[stream] ?? "";
  if (text !== "") console.log(`${stream}:\n${text.replace(/\n$/, "")}`);
}
if (revert !== null) {
  console.error(`revert ${revert}`);
  process.exitCode = 3;
} else if (failure !== null) {
  console.error(failure);
  // The status is set, not `process.exit` called, so that a long output is
  // written to the end.
  process.exitCode = 1;
}
