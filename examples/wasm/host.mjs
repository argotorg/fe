// A host for WebAssembly modules built from Fe with `--backend wasm`.
//
// It supplies the imports of `std::wasm` (the four scalar operations and the
// files contract), places text in the module's memory before a call, decodes
// the reply of an export, counts open handles, and checks a module against its
// build manifest before running it. It checks everything a module hands it
// before acting on it, and ends the call with a `ContractError` that names the
// problem when the module breaks the contract. The file is a plain ES module
// with no dependencies; it needs `WebAssembly`, `BigInt`, `TextEncoder` and
// `crypto.subtle` (Node.js 20 or later, and current browsers).
//
//   import { instantiate } from "./host.mjs";
//   const manifest = JSON.parse(fs.readFileSync("lines.manifest.json", "utf8"));
//   const guest = await instantiate(fs.readFileSync("lines.wasm"), {
//     manifest,
//     files: { "hello.txt": "Hello" },
//   });
//   guest.callReply("lines", "hello.txt");   // { ok: true, value: 1 }
//   guest.host.assertNoOpenHandles();
//
// The contract is described in `ingots/std/src/wasm.fe`,
// `ingots/std/src/wasm/boundary.fe` and `ingots/std/src/wasm/files.fe`, and
// in `examples/wasm/README.md`; the numbers below repeat it.

/** The version of the host contract that this host implements. */
export const CONTRACT_VERSION = 1;

/** The version of the manifest format that this host reads. */
export const MANIFEST_VERSION = 1;

/** The errors of the contract, by number. A number outside this table is a fault. */
export const FILE_ERRORS = Object.freeze({
  1: "NotFound",
  2: "OutOfRange",
  3: "BadName",
});

/** The longest text, in bytes, that the host reads from the module or places in it. */
export const MAX_TEXT_BYTES = 65536;

/** The module, or the code that drives it, broke the contract. */
export class ContractError extends Error {
  constructor(message) {
    super(message);
    this.name = "ContractError";
  }
}

/** The module called `abort`. `code` is the number it passed (an unsigned 32-bit integer). */
export class GuestAbort extends Error {
  constructor(code) {
    super(`the module aborted with code ${code}`);
    this.name = "GuestAbort";
    this.code = code;
  }
}

const MASK32 = 0xffffffffn;
const encoder = new TextEncoder();

/** The reply word of the contract: the tag in the low half, the payload in the high half. */
export function encodeReply(tag, payload = 0) {
  if (tag !== 0 && !(tag in FILE_ERRORS)) {
    throw new ContractError(`unknown error code ${tag}: the contract defines ${Object.keys(FILE_ERRORS).join(", ")}`);
  }
  if (tag !== 0 && payload !== 0) {
    throw new ContractError(`an error reply carries no payload, but error ${tag} has payload ${payload}`);
  }
  return (BigInt(payload >>> 0) << 32n) | BigInt(tag >>> 0);
}

/** `{ ok: true, value }` or `{ ok: false, code, error }` for the word an export returned. */
export function decodeReply(word) {
  if (typeof word !== "bigint") {
    throw new ContractError(`a reply is a 64-bit word, but the export returned ${typeof word}`);
  }
  const bits = BigInt.asUintN(64, word);
  const tag = Number(bits & MASK32);
  const payload = Number(bits >> 32n);
  if (tag === 0) return { ok: true, value: payload };
  if (!(tag in FILE_ERRORS)) {
    throw new ContractError(`unknown error code ${tag} in a reply: the contract defines ${Object.keys(FILE_ERRORS).join(", ")}`);
  }
  if (payload !== 0) {
    throw new ContractError(`an error reply carries no payload, but error ${tag} has payload ${payload}`);
  }
  return { ok: false, code: tag, error: FILE_ERRORS[tag] };
}

/** The bytes of a file or a name: a string is UTF-8, a Uint8Array is taken as it is. */
function toBytes(value, what) {
  if (typeof value === "string") return encoder.encode(value);
  if (value instanceof Uint8Array) return value;
  throw new TypeError(`${what} must be a string or a Uint8Array`);
}

/** A name is compared as bytes, so each byte becomes one character of the key. */
function keyOf(bytes) {
  let key = "";
  for (const byte of bytes) key += String.fromCharCode(byte);
  return key;
}

/**
 * The imports of `std::wasm` (module `env`), over an in-memory set of files.
 *
 * `files` maps a name to its contents. Handle numbers count up from 1 and are
 * never reused, so a closed handle is told from one that was never issued.
 * Call `bind` with the instance before the module runs.
 *
 * Options:
 * - `imports`: further imports, as an object of modules; entries of `env`
 *   replace the standard ones;
 * - `now`: the clock, a function returning milliseconds since the Unix epoch
 *   (`Date.now` by default);
 * - `onLog`: called with `{ type: "i32", value }` or `{ type: "u64", value }`
 *   for each logged number; the entries are also kept in `host.logs`.
 *
 * `abort` throws a `GuestAbort`.
 */
export function createHost(files = {}, { imports = {}, now = Date.now, onLog = null } = {}) {
  const table = new Map();
  for (const [name, contents] of Object.entries(files)) {
    table.set(keyOf(toBytes(name, "a file name")), toBytes(contents, `the contents of ${name}`));
  }
  const open = new Map(); // handle -> contents
  const logs = [];
  let issued = 0;
  let closed = 0;
  let memory = null;
  const log = (entry) => {
    logs.push(entry);
    if (onLog !== null) onLog(entry);
  };

  const bytesOfMemory = () => {
    if (memory === null) throw new ContractError("the host is not bound to an instance");
    return new Uint8Array(memory.buffer);
  };

  /** The text at `address` of `length` bytes, copied out of the module's memory. */
  const readText = (address, length, what) => {
    const addr = address >>> 0;
    const len = length >>> 0;
    if (len > MAX_TEXT_BYTES) {
      throw new ContractError(`${what}: text length ${len} is above the limit of ${MAX_TEXT_BYTES} bytes`);
    }
    const view = bytesOfMemory();
    if (addr + len > view.length) {
      throw new ContractError(`${what}: text at ${addr} of length ${len} lies outside the memory of ${view.length} bytes`);
    }
    return view.slice(addr, addr + len);
  };

  const lookup = (handle, what) => {
    const id = handle >>> 0;
    const contents = open.get(id);
    if (contents !== undefined) return contents;
    if (id >= 1 && id <= issued) {
      throw new ContractError(`${what}: handle ${id} is closed (closed twice, or used after it was closed)`);
    }
    throw new ContractError(`${what}: handle ${id} was never issued by this host (forged handle)`);
  };

  const env = {
    sys_wasm_files_open(address, length) {
      const name = readText(address, length, "open");
      if (name.length === 0) return encodeReply(3);
      const contents = table.get(keyOf(name));
      if (contents === undefined) return encodeReply(1);
      issued += 1;
      open.set(issued, contents);
      return encodeReply(0, issued);
    },
    sys_wasm_files_size(handle) {
      return encodeReply(0, lookup(handle, "size").length);
    },
    sys_wasm_files_read_byte(handle, index) {
      const contents = lookup(handle, "read_byte");
      const at = index >>> 0;
      if (at >= contents.length) return encodeReply(2);
      return encodeReply(0, contents[at]);
    },
    sys_wasm_files_close(handle) {
      lookup(handle, "close");
      open.delete(handle >>> 0);
      closed += 1;
    },
    sys_wasm_log_i32(value) {
      log({ type: "i32", value: value | 0 });
    },
    sys_wasm_log_u64(value) {
      log({ type: "u64", value: BigInt.asUintN(64, value) });
    },
    sys_wasm_now_ms() {
      return BigInt.asIntN(64, BigInt(Math.trunc(now())));
    },
    sys_wasm_abort(code) {
      throw new GuestAbort(code >>> 0);
    },
    ...(imports.env ?? {}),
  };

  return {
    imports: { ...imports, env },
    /** The numbers the module logged, in order. */
    logs,
    /** Gives the host the instance whose memory the module's text lives in. */
    bind(instance) {
      memory = instance.exports.memory;
      if (!(memory instanceof WebAssembly.Memory)) {
        throw new ContractError("the module does not export its memory as `memory`");
      }
    },
    get opened() { return issued; },
    get closed() { return closed; },
    /** Handles that were opened and not closed. */
    get openHandles() { return open.size; },
    assertNoOpenHandles() {
      if (open.size !== 0) {
        throw new ContractError(`${open.size} handle(s) were not closed: ${[...open.keys()].join(", ")}`);
      }
    },
  };
}

/**
 * A running module and its host.
 *
 * An argument of a call is a number or bigint, passed as it is, or a string or
 * Uint8Array, which is text: it is placed in the module's memory and passed as
 * an address and a length. The memory is released after the call. When the
 * guest was made with a manifest, a call must also match the function that the
 * manifest describes: its name, the number of arguments, and which of them are
 * text.
 */
export class Guest {
  constructor(instance, host, manifest = null) {
    this.instance = instance;
    this.host = host;
    this.manifest = manifest;
    this.exports = instance.exports;
  }

  #requireAllocator() {
    for (const name of ["alloc", "arena_mark", "arena_release"]) {
      if (typeof this.exports[name] !== "function") {
        throw new ContractError(`the module does not export \`${name}\`, so it takes no text`);
      }
    }
  }

  #checkAgainstManifest(name, args) {
    if (this.manifest === null) return;
    const entry = this.manifest.exports.find((e) => e.name === name && e.kind === "function");
    if (entry === undefined) {
      throw new ContractError(`the manifest does not describe a function \`${name}\``);
    }
    const params = entry.params ?? [];
    if (args.length !== params.length) {
      const list = params.map((p) => `${p.name}: ${p.type}`).join(", ");
      throw new ContractError(
        `\`${name}\` takes ${params.length} argument(s)${list ? ` (${list})` : ""}, but ${args.length} were given`,
      );
    }
    params.forEach((param, index) => {
      const arg = args[index];
      const isText = typeof arg === "string" || arg instanceof Uint8Array;
      if ((param.type === "HostText") !== isText) {
        const wanted = param.type === "HostText" ? "text (a string or a Uint8Array)" : `a number of type ${param.type}`;
        throw new ContractError(`argument \`${param.name}\` of \`${name}\` must be ${wanted}`);
      }
    });
  }

  /** Calls an export and returns what it returned. */
  call(name, ...args) {
    const func = this.exports[name];
    if (typeof func !== "function") throw new ContractError(`the module does not export a function \`${name}\``);
    this.#checkAgainstManifest(name, args);
    if (!args.some((arg) => typeof arg === "string" || arg instanceof Uint8Array)) {
      return func(...args);
    }
    this.#requireAllocator();
    const mark = this.exports.arena_mark();
    try {
      const spread = [];
      for (const arg of args) {
        if (typeof arg === "string" || arg instanceof Uint8Array) {
          const bytes = toBytes(arg, "a text argument");
          if (bytes.length > MAX_TEXT_BYTES) {
            throw new ContractError(`text of ${bytes.length} bytes is above the limit of ${MAX_TEXT_BYTES} bytes`);
          }
          const address = this.exports.alloc(bytes.length) >>> 0;
          const memory = new Uint8Array(this.exports.memory.buffer);
          if (address + bytes.length > memory.length) {
            throw new ContractError(`alloc returned ${address}, which cannot hold ${bytes.length} bytes in a memory of ${memory.length}`);
          }
          memory.set(bytes, address);
          spread.push(address, bytes.length);
        } else {
          spread.push(arg);
        }
      }
      return func(...spread);
    } finally {
      this.exports.arena_release(mark);
    }
  }

  /** Calls an export that returns a reply and decodes it. */
  callReply(name, ...args) {
    return decodeReply(this.call(name, ...args));
  }
}

/** The SHA-256 of `bytes` as 64 lowercase hexadecimal digits. */
export async function sha256Hex(bytes) {
  const digest = new Uint8Array(await globalThis.crypto.subtle.digest("SHA-256", bytes));
  return Array.from(digest, (byte) => byte.toString(16).padStart(2, "0")).join("");
}

/** The manifest in `text` (JSON), after a check of its version and shape. */
export function parseManifest(text) {
  let manifest;
  try {
    manifest = JSON.parse(text);
  } catch (error) {
    throw new ContractError(`the manifest is not valid JSON: ${error.message}`);
  }
  return checkManifestShape(manifest);
}

/** Throws a `ContractError` unless `manifest` is a manifest this host reads. */
export function checkManifestShape(manifest) {
  if (typeof manifest !== "object" || manifest === null) {
    throw new ContractError("the manifest is not an object");
  }
  if (manifest.manifest_version !== MANIFEST_VERSION) {
    throw new ContractError(
      `the manifest has format version ${manifest.manifest_version}; this host reads version ${MANIFEST_VERSION}`,
    );
  }
  if (manifest.contract_version !== CONTRACT_VERSION) {
    throw new ContractError(
      `the module was built for host contract version ${manifest.contract_version}; this host implements version ${CONTRACT_VERSION}`,
    );
  }
  if (typeof manifest.module?.sha256 !== "string" || !Array.isArray(manifest.imports) || !Array.isArray(manifest.exports)) {
    throw new ContractError("the manifest lacks the module hash, the imports or the exports");
  }
  return manifest;
}

/** `module.name` of an import or export entry. */
const qualified = (entry) => `${entry.module}.${entry.name}`;

/**
 * Throws a `ContractError` that names every import in `wanted` (entries with
 * `module` and `name`) which `imports` does not provide as a function.
 */
function requireImports(wanted, imports, what) {
  const missing = wanted.filter((entry) => typeof imports?.[entry.module]?.[entry.name] !== "function");
  if (missing.length === 0) return;
  const names = missing.map(qualified).join(", ");
  throw new ContractError(
    missing.length === 1
      ? `${what} imports ${names}, which the host does not provide`
      : `${what} imports ${missing.length} functions that the host does not provide: ${names}`,
  );
}

/**
 * Compiles a module and refuses it unless the host can run it. The host must
 * provide every import of the module, and, with a manifest, the module must be
 * the one the manifest describes (same SHA-256, same imports, same exports).
 * Returns the compiled module.
 */
export async function checkModule(bytes, imports, manifest = null) {
  if (manifest !== null) {
    checkManifestShape(manifest);
    const hash = await sha256Hex(bytes);
    if (hash !== manifest.module.sha256) {
      throw new ContractError(
        `the module has SHA-256 ${hash}, but the manifest describes ${manifest.module.sha256}`,
      );
    }
    // Name what is missing from the manifest before the module is compiled.
    requireImports(manifest.imports, imports, "the manifest");
  }
  const module = await WebAssembly.compile(bytes);
  const actual = WebAssembly.Module.imports(module);
  requireImports(
    actual.filter((entry) => entry.kind === "function"),
    imports,
    "the module",
  );
  const unsupported = actual.find((entry) => entry.kind !== "function");
  if (unsupported !== undefined) {
    throw new ContractError(`the module imports ${qualified(unsupported)}, a ${unsupported.kind}; the host supplies functions only`);
  }
  if (manifest !== null) {
    const key = (entry) => `${entry.module}\u0000${entry.name}`;
    const listed = new Set(manifest.imports.map(key));
    const present = new Set(actual.map(key));
    for (const entry of actual) {
      if (!listed.has(key(entry))) {
        throw new ContractError(`the module imports ${qualified(entry)}, which the manifest does not list`);
      }
    }
    for (const entry of manifest.imports) {
      if (!present.has(key(entry))) {
        throw new ContractError(`the manifest lists the import ${qualified(entry)}, which the module does not have`);
      }
    }
    const exported = WebAssembly.Module.exports(module);
    const kinds = new Map(exported.map((entry) => [entry.name, entry.kind]));
    for (const entry of manifest.exports) {
      if (kinds.get(entry.name) !== entry.kind) {
        throw new ContractError(`the manifest lists the ${entry.kind} export \`${entry.name}\`, which the module does not have`);
      }
    }
    const listedExports = new Set(manifest.exports.map((entry) => entry.name));
    for (const entry of exported) {
      if (!listedExports.has(entry.name)) {
        throw new ContractError(`the module exports \`${entry.name}\`, which the manifest does not list`);
      }
    }
  }
  return module;
}

/**
 * Instantiates a module with the standard imports and returns it with its host.
 *
 * Options: `manifest` (the parsed manifest; the module is then checked against
 * it), `files` (the files the module can open) and the options of `createHost`.
 * The host refuses a module whose imports it does not provide, before it runs.
 */
export async function instantiate(bytes, { manifest = null, files = {}, ...options } = {}) {
  const host = createHost(files, options);
  const module = await checkModule(bytes, host.imports, manifest);
  const instance = await WebAssembly.instantiate(module, host.imports);
  host.bind(instance);
  return new Guest(instance, host, manifest);
}
