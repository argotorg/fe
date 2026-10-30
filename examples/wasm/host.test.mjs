// Tests of the host checks, with no WebAssembly module of Fe: the imports are
// called directly, as a module that breaks the contract would call them.
//
//   node --test examples/wasm/host.test.mjs
import assert from "node:assert/strict";
import test from "node:test";
import {
  CONTRACT_VERSION,
  ContractError,
  FILE_ERRORS,
  GuestAbort,
  MAX_TEXT_BYTES,
  MANIFEST_VERSION,
  checkModule,
  createHost,
  decodeReply,
  encodeReply,
  instantiate,
  parseManifest,
  sha256Hex,
} from "./host.mjs";

const PAGE = 65536;

/** A host over two files, bound to a memory that holds the names `hello.txt` and `nope` at 100. */
function fixture() {
  const memory = new WebAssembly.Memory({ initial: 1 });
  const host = createHost({ "hello.txt": "Hello, world", empty: "" });
  host.bind({ exports: { memory } });
  const bytes = new Uint8Array(memory.buffer);
  bytes.set(new TextEncoder().encode("hello.txt"), 100);
  bytes.set(new TextEncoder().encode("nope"), 200);
  return { host, env: host.imports.env, memory };
}

const open = (env, address, length) => decodeReply(env.sys_wasm_files_open(address, length));

test("a present file, a missing file and a bad name are replies", () => {
  const { env } = fixture();
  assert.deepEqual(open(env, 100, 9), { ok: true, value: 1 });
  assert.deepEqual(open(env, 200, 4), { ok: false, code: 1, error: "NotFound" });
  assert.deepEqual(open(env, 100, 0), { ok: false, code: 3, error: "BadName" });
});

test("size and read_byte, and a read past the end is OutOfRange", () => {
  const { env } = fixture();
  const handle = open(env, 100, 9).value;
  assert.deepEqual(decodeReply(env.sys_wasm_files_size(handle)), { ok: true, value: 12 });
  assert.deepEqual(decodeReply(env.sys_wasm_files_read_byte(handle, 1)), { ok: true, value: 101 });
  assert.deepEqual(decodeReply(env.sys_wasm_files_read_byte(handle, 11)), { ok: true, value: 100 });
  assert.deepEqual(decodeReply(env.sys_wasm_files_read_byte(handle, 12)), { ok: false, code: 2, error: "OutOfRange" });
  // An index is unsigned: the host receives an i32 and does not read it as negative.
  assert.deepEqual(decodeReply(env.sys_wasm_files_read_byte(handle, -1)), { ok: false, code: 2, error: "OutOfRange" });
});

test("handles count up from 1 and are not reused", () => {
  const { env } = fixture();
  const first = open(env, 100, 9).value;
  env.sys_wasm_files_close(first);
  const second = open(env, 100, 9).value;
  assert.deepEqual([first, second], [1, 2]);
});

test("a forged handle is refused by every operation", () => {
  const { host, env } = fixture();
  open(env, 100, 9);
  for (const forged of [0, 2, 77, -1, 0x7fffffff]) {
    for (const call of [
      () => env.sys_wasm_files_size(forged),
      () => env.sys_wasm_files_read_byte(forged, 0),
      () => env.sys_wasm_files_close(forged),
    ]) {
      assert.throws(call, (error) => error instanceof ContractError && /never issued.*forged handle/.test(error.message));
    }
  }
  assert.equal(host.openHandles, 1, "a refused call changes nothing");
  assert.equal(host.closed, 0);
});

test("a closed handle is refused: double close and use after close", () => {
  const { host, env } = fixture();
  const handle = open(env, 100, 9).value;
  env.sys_wasm_files_close(handle);
  for (const call of [
    () => env.sys_wasm_files_close(handle),
    () => env.sys_wasm_files_size(handle),
    () => env.sys_wasm_files_read_byte(handle, 0),
  ]) {
    assert.throws(call, (error) => error instanceof ContractError && /handle 1 is closed/.test(error.message));
  }
  assert.equal(host.closed, 1, "the second close is not counted");
});

test("text outside the memory, or too long, is refused", () => {
  const { host, env, memory } = fixture();
  const size = memory.buffer.byteLength;
  assert.equal(size, PAGE);
  for (const [address, length] of [
    [size - 4, 5],
    [size, 1],
    [0xffffff00, 100],
    [-1, 2], // the i32 -1 is the address 0xffffffff
    [0xffffffff, 1],
  ]) {
    assert.throws(
      () => env.sys_wasm_files_open(address, length),
      (error) => error instanceof ContractError && /lies outside the memory of 65536 bytes/.test(error.message),
      `${address}+${length}`,
    );
  }
  // A text that ends at the last byte of the memory is inside it, and so is an empty one at the end.
  assert.deepEqual(open(env, size - 4, 4), { ok: false, code: 1, error: "NotFound" });
  assert.deepEqual(open(env, size, 0), { ok: false, code: 3, error: "BadName" });
  assert.throws(
    () => env.sys_wasm_files_open(0, MAX_TEXT_BYTES + 1),
    (error) => error instanceof ContractError && /above the limit of 65536 bytes/.test(error.message),
  );
  // A negative i32 length is a large unsigned one.
  for (const length of [-1, 0xffffffff, 0x80000000]) {
    assert.throws(
      () => env.sys_wasm_files_open(0, length),
      (error) => error instanceof ContractError && /above the limit/.test(error.message),
    );
  }
  assert.equal(host.opened, 0, "a refused call opens nothing");
});

test("an unknown error code is refused when it is made and when it is read", () => {
  for (const code of [4, 9, 255, 0xffffffff]) {
    assert.throws(() => encodeReply(code), (error) => error instanceof ContractError && new RegExp(`unknown error code ${code}\\b`).test(error.message));
    const word = BigInt(code);
    assert.throws(() => decodeReply(word), (error) => error instanceof ContractError && /unknown error code/.test(error.message));
  }
  for (const code of Object.keys(FILE_ERRORS)) {
    assert.deepEqual(decodeReply(encodeReply(Number(code))), { ok: false, code: Number(code), error: FILE_ERRORS[code] });
  }
  // An error carries no payload, and a reply is a bigint.
  assert.throws(() => decodeReply((5n << 32n) | 2n), /carries no payload/);
  assert.throws(() => decodeReply(3), /64-bit word/);
  assert.deepEqual(decodeReply(encodeReply(0, 0xffffffff)), { ok: true, value: 0xffffffff });
});

test("open handles are counted and the leak check names them", () => {
  const { host, env } = fixture();
  assert.equal(host.openHandles, 0);
  host.assertNoOpenHandles();
  const a = open(env, 100, 9).value;
  const b = open(env, 100, 9).value;
  assert.equal(host.openHandles, 2);
  assert.throws(() => host.assertNoOpenHandles(), /2 handle\(s\) were not closed: 1, 2/);
  env.sys_wasm_files_close(a);
  env.sys_wasm_files_close(b);
  assert.equal(host.openHandles, 0);
  assert.deepEqual([host.opened, host.closed], [2, 2]);
  host.assertNoOpenHandles();
});

test("a host that was not bound refuses to read", () => {
  const host = createHost({});
  assert.throws(() => host.imports.env.sys_wasm_files_open(0, 0), /not bound/);
  assert.throws(() => host.bind({ exports: {} }), /does not export its memory/);
});

test("names are compared as bytes", () => {
  const memory = new WebAssembly.Memory({ initial: 1 });
  const host = createHost({ "grüß": "x", [new Uint8Array([0xff, 0xfe]).toString()]: "y" });
  host.bind({ exports: { memory } });
  const bytes = new Uint8Array(memory.buffer);
  bytes.set(new TextEncoder().encode("grüß"), 0);
  bytes.set([0xc3, 0xbc], 20); // "ü" alone
  assert.deepEqual(open(host.imports.env, 0, 6), { ok: true, value: 1 });
  assert.deepEqual(open(host.imports.env, 20, 2), { ok: false, code: 1, error: "NotFound" });
});

// ---------------------------------------------------------------------------
// Modules and manifests. The modules are assembled here, byte by byte, so that
// these tests need no compiler.
// ---------------------------------------------------------------------------

const leb = (n) => {
  const out = [];
  do {
    let byte = n & 0x7f;
    n >>>= 7;
    if (n !== 0) byte |= 0x80;
    out.push(byte);
  } while (n !== 0);
  return out;
};
const text = (s) => [...leb(s.length), ...new TextEncoder().encode(s)];
const section = (id, body) => [id, ...leb(body.length), ...body];
const vec = (items) => [...leb(items.length), ...items.flat()];

/**
 * A module with the given function imports (`env` names, each `() -> ()`, or
 * `(i32) -> ()` when the name ends in `_i32`), a memory exported as `memory`,
 * and two exported functions: `run`, which returns 7, and `twice`, which
 * returns its i32 argument times two.
 */
function assemble(importNames = []) {
  const types = [
    [0x60, 0, 0], // () -> ()
    [0x60, 1, 0x7f, 0], // (i32) -> ()
    [0x60, 0, 1, 0x7f], // () -> i32
    [0x60, 1, 0x7f, 1, 0x7f], // (i32) -> i32
  ];
  const imports = importNames.map((name) => [...text("env"), ...text(name), 0, name.endsWith("_i32") ? 1 : 0]);
  const bytes = [
    0x00, 0x61, 0x73, 0x6d, 1, 0, 0, 0,
    ...section(1, vec(types)),
    ...(imports.length ? section(2, vec(imports)) : []),
    ...section(3, vec([[2], [3]])),
    ...section(5, vec([[0, 1]])),
    ...section(
      7,
      vec([
        [...text("memory"), 2, 0],
        [...text("run"), 0, importNames.length],
        [...text("twice"), 0, importNames.length + 1],
      ]),
    ),
    ...section(10, vec([[...leb(4), 0, 0x41, 7, 0x0b], [...leb(7), 0, 0x20, 0, 0x41, 2, 0x6c, 0x0b]])),
  ];
  return new Uint8Array(bytes);
}

async function manifestFor(bytes, { imports = [], exports = null, ...patch } = {}) {
  return {
    manifest_version: MANIFEST_VERSION,
    module: { file: "m.wasm", sha256: await sha256Hex(bytes), size: bytes.length, initial_memory_pages: 1 },
    contract_version: CONTRACT_VERSION,
    optimization: "1",
    compiler: "fe test",
    imports: imports.map((name) => ({ module: "env", name, kind: "function", type: { params: [], results: [] } })),
    exports: exports ?? [
      { name: "memory", kind: "memory" },
      { name: "run", kind: "function", params: [], result: { type: "i32", wasm: ["i32"] } },
      {
        name: "twice",
        kind: "function",
        params: [{ name: "x", type: "u32", wasm: ["i32"] }],
        result: { type: "u32", wasm: ["i32"] },
      },
    ],
    ...patch,
  };
}

const refuses = (promise, pattern) =>
  assert.rejects(promise, (error) => error instanceof ContractError && pattern.test(error.message));

test("sha256Hex is the SHA-256 of the bytes", async () => {
  assert.equal(await sha256Hex(new Uint8Array()), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
  assert.equal(
    await sha256Hex(new TextEncoder().encode("abc")),
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
  );
});

test("a module runs when the host provides its imports", async () => {
  const bytes = assemble(["sys_wasm_log_i32", "sys_wasm_now_ms_unused"]);
  const guest = await instantiate(bytes, { imports: { env: { sys_wasm_now_ms_unused() {} } } });
  assert.equal(guest.call("run"), 7);
});

test("a module is refused, and the import named, when the host does not provide it", async () => {
  const bytes = assemble(["sys_wasm_log_i32", "custom_import"]);
  await refuses(instantiate(bytes), /the module imports env\.custom_import, which the host does not provide/);
  const two = assemble(["custom_a", "custom_b"]);
  await refuses(instantiate(two), /2 functions that the host does not provide: env\.custom_a, env\.custom_b/);
});

test("a manifest lets the host refuse before it compiles the module", async () => {
  const bytes = assemble(["custom_import"]);
  const manifest = await manifestFor(bytes, { imports: ["custom_import"] });
  await refuses(instantiate(bytes, { manifest }), /the manifest imports env\.custom_import, which the host does not provide/);
  // Garbage that is not a module fails on the hash first when the manifest says otherwise.
  const garbage = new Uint8Array([1, 2, 3]);
  await refuses(instantiate(garbage, { manifest }), /has SHA-256 .* but the manifest describes/);
});

test("a module that matches its manifest runs, and the call is checked against the manifest", async () => {
  const bytes = assemble(["sys_wasm_log_i32"]);
  const manifest = await manifestFor(bytes, { imports: ["sys_wasm_log_i32"] });
  const guest = await instantiate(bytes, { manifest });
  assert.equal(guest.call("run"), 7);
  assert.throws(() => guest.call("run", 1), /`run` takes 0 argument\(s\), but 1 were given/);
  assert.throws(() => guest.call("memory"), /does not export a function/);
  assert.equal(guest.call("twice", 21), 42);
  assert.throws(() => guest.call("twice"), /`twice` takes 1 argument\(s\) \(x: u32\), but 0 were given/);
  assert.throws(() => guest.call("twice", "text"), /argument `x` of `twice` must be a number of type u32/);
  // Without a manifest the host cannot tell, and passes the arguments as they are.
  const bare = await instantiate(bytes);
  assert.equal(bare.call("twice", 4), 8);
});

test("a module that differs from its manifest is refused", async () => {
  const bytes = assemble(["sys_wasm_log_i32"]);
  const good = await manifestFor(bytes, { imports: ["sys_wasm_log_i32"] });

  const changed = Uint8Array.from(bytes);
  changed[changed.length - 2] = 8; // the constant the function returns
  await refuses(instantiate(changed, { manifest: good }), /SHA-256/);

  const wrongHash = structuredClone(good);
  wrongHash.module.sha256 = "0".repeat(64);
  await refuses(instantiate(bytes, { manifest: wrongHash }), /SHA-256/);

  const unlisted = await manifestFor(bytes, { imports: [] });
  await refuses(instantiate(bytes, { manifest: unlisted }), /imports env\.sys_wasm_log_i32, which the manifest does not list/);

  const extra = await manifestFor(bytes, { imports: ["sys_wasm_log_i32", "sys_wasm_now_ms"] });
  await refuses(instantiate(bytes, { manifest: extra }), /lists the import env\.sys_wasm_now_ms, which the module does not have/);

  const lacking = await manifestFor(bytes, { imports: ["sys_wasm_log_i32"], exports: [{ name: "memory", kind: "memory" }] });
  await refuses(instantiate(bytes, { manifest: lacking }), /exports `run`, which the manifest does not list/);

  const phantom = await manifestFor(bytes, {
    imports: ["sys_wasm_log_i32"],
    exports: [
      { name: "memory", kind: "memory" },
      { name: "run", kind: "function" },
      { name: "twice", kind: "function" },
      { name: "ghost", kind: "function" },
    ],
  });
  await refuses(instantiate(bytes, { manifest: phantom }), /lists the function export `ghost`, which the module does not have/);
});

test("a manifest of another version is refused", async () => {
  const bytes = assemble();
  const manifest = await manifestFor(bytes);
  await refuses(instantiate(bytes, { manifest: { ...manifest, manifest_version: 2 } }), /format version 2; this host reads version 1/);
  await refuses(instantiate(bytes, { manifest: { ...manifest, contract_version: 2 } }), /host contract version 2; this host implements version 1/);
  await refuses(instantiate(bytes, { manifest: { manifest_version: 1, contract_version: 1 } }), /lacks the module hash/);
  assert.throws(() => parseManifest("{"), (error) => error instanceof ContractError && /not valid JSON/.test(error.message));
  assert.throws(() => parseManifest("[]"), /format version undefined/);
  assert.deepEqual(parseManifest(JSON.stringify(manifest)), manifest);
});

test("checkModule returns the compiled module", async () => {
  const bytes = assemble();
  const module = await checkModule(bytes, {}, await manifestFor(bytes));
  assert.ok(module instanceof WebAssembly.Module);
});

test("log, clock and abort are supplied", () => {
  const seen = [];
  const host = createHost({}, { now: () => 1234567890123, onLog: (entry) => seen.push(entry) });
  const { env } = host.imports;
  env.sys_wasm_log_i32(-5);
  env.sys_wasm_log_i32(0xffffffff); // an i32 arrives signed from the engine, but is masked here as well
  env.sys_wasm_log_u64(-1n); // the i64 of all ones is the largest u64
  assert.deepEqual(host.logs, [
    { type: "i32", value: -5 },
    { type: "i32", value: -1 },
    { type: "u64", value: 18446744073709551615n },
  ]);
  assert.deepEqual(seen, host.logs);
  assert.equal(env.sys_wasm_now_ms(), 1234567890123n);
  assert.throws(() => env.sys_wasm_abort(7), (error) => error instanceof GuestAbort && error.code === 7);
  assert.throws(() => env.sys_wasm_abort(-1), (error) => error instanceof GuestAbort && error.code === 0xffffffff);
});
