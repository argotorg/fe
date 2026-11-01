// Runs examples/wasm/lines.wasm on a file of the disk.
//
//   node examples/wasm/lines.mjs lines.wasm path/to/file
import fs from "node:fs";
import path from "node:path";
import { instantiate } from "./host.mjs";

const [, , modulePath, filePath] = process.argv;
if (!modulePath || !filePath) {
  console.error("usage: node lines.mjs <module.wasm> <file>");
  process.exit(2);
}
const name = path.basename(filePath);
const guest = await instantiate(fs.readFileSync(modulePath), {
  files: { [name]: fs.readFileSync(filePath) },
});
const reply = guest.callReply("lines", name);
guest.host.assertNoOpenHandles();
if (!reply.ok) {
  console.error(`error ${reply.code}: ${reply.error}`);
  process.exit(1);
}
console.log(reply.value);
