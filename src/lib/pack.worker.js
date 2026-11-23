import brotli from "brotli-wasm";

self.onmessage = async ({ data: project }) => {
  try {
    const raw = new TextEncoder().encode(JSON.stringify({ name: project.name, files: project.files }));
    if (raw.length > 32 * 1024 * 1024) throw new Error("The project exceeds the 32 MiB link limit.");
    const codec = await brotli;
    const bytes = codec.compress(raw, { quality: 11 });
    if (Math.ceil(bytes.length * 4 / 3) > 2 * 1024 * 1024) throw new Error("The compressed project is too large for an Issie link.");
    const chunks = [];
    for (let i = 0; i < bytes.length; i += 0x8000) {
      chunks.push(String.fromCharCode(...bytes.subarray(i, i + 0x8000)));
    }
    const payload = btoa(chunks.join("")).replaceAll("+", "-").replaceAll("/", "_").replace(/=+$/, "");
    self.postMessage({ payload, bytes: bytes.length });
  } catch (error) {
    self.postMessage({ error: error.message || String(error) });
  }
};
