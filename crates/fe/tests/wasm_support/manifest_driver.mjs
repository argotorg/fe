// Instantiates a module with the Node host (`examples/wasm/host.mjs`), with or
// without its manifest, and makes calls, for `tests/wasm_manifest.rs`.
//
// Standard input is one JSON object:
//   { "wasm": path, "manifest": object | null, "files": { name: contents },
//     "calls": [ { "export": name, "args": [...], "reply": bool } ] }
// An argument is a string (text), a number, or { "u64": "decimal" }. Standard
// output is one JSON object, either { "error": { "name", "message" } } when the
// host refused the module, or { "results": [...], "logs": [...] } where a result is
// { "value": string }, { "reply": {...} } or { "error": { "name", "message" } }.
import fs from "node:fs";
import { instantiate } from "../../../../examples/wasm/host.mjs";

const request = JSON.parse(fs.readFileSync(0, "utf8"));
const bytes = fs.readFileSync(request.wasm);
const argument = (value) =>
  typeof value === "object" && value !== null && "u64" in value ? BigInt(value.u64) : value;
const failure = (error) => ({ error: { name: error.name, message: error.message } });

let guest;
try {
  guest = await instantiate(bytes, { manifest: request.manifest ?? null, files: request.files ?? {} });
} catch (error) {
  process.stdout.write(JSON.stringify(failure(error)));
  process.exit(0);
}
const results = [];
for (const call of request.calls ?? []) {
  const args = (call.args ?? []).map(argument);
  try {
    if (call.reply) {
      results.push({ reply: guest.callReply(call.export, ...args) });
    } else {
      const value = guest.call(call.export, ...args);
      results.push({ value: value === undefined ? null : String(value) });
    }
  } catch (error) {
    results.push(failure(error));
  }
}
process.stdout.write(
  JSON.stringify({ results, logs: guest.host.logs.map((entry) => ({ type: entry.type, value: String(entry.value) })) }),
);
