// Runs calls against a WebAssembly module built from Fe, through the Node host
// of the files contract (`examples/wasm/host.mjs`), for `tests/wasm_files.rs`.
//
// Standard input is one JSON object:
//   { "wasm": path, "files": { name: contents }, "calls": [ call, ... ] }
// where a call is { "export": name, "args": [...], "reply": bool }. An argument
// is a string (text), a number, or { "u64": "decimal" }. Standard output is one
// JSON object: { "results": [...], "after": [...], "opened", "closed", "open" }.
// A result is { kind: "reply", ... }, { kind: "value", value }, { kind: "none" },
// { kind: "contract", message } when the host refused the module's behaviour, or
// { kind: "trap", message } when the module trapped. `after[i]` is the arena top
// and the number of open handles after call i.
import fs from "node:fs";
import { ContractError, instantiate } from "../../../../examples/wasm/host.mjs";

const request = JSON.parse(fs.readFileSync(0, "utf8"));
const bytes = fs.readFileSync(request.wasm);
const guest = await instantiate(bytes, { files: request.files ?? {} });
const hasAllocator = typeof guest.exports.arena_mark === "function";

const argument = (value) => {
  if (typeof value === "object" && value !== null && "u64" in value) return BigInt(value.u64);
  if (typeof value === "object" && value !== null && "bytes" in value) return Uint8Array.from(value.bytes);
  return value;
};

const results = [];
const after = [];
for (const call of request.calls) {
  const args = (call.args ?? []).map(argument);
  let result;
  try {
    if (call.reply) {
      result = { kind: "reply", ...guest.callReply(call.export, ...args) };
    } else {
      const value = guest.call(call.export, ...args);
      result = value === undefined ? { kind: "none" } : { kind: "value", value: String(value) };
    }
  } catch (error) {
    if (error instanceof ContractError) {
      result = { kind: "contract", message: error.message };
    } else if (error instanceof WebAssembly.RuntimeError) {
      result = { kind: "trap", message: error.message };
    } else {
      throw error;
    }
  }
  results.push(result);
  after.push({ top: hasAllocator ? guest.exports.arena_mark() : null, open: guest.host.openHandles });
}
process.stdout.write(
  JSON.stringify({
    results,
    after,
    opened: guest.host.opened,
    closed: guest.host.closed,
    open: guest.host.openHandles,
  }),
);
