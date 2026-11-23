export const issieSite = import.meta.env.VITE_ISSIE_WEB_URL || "https://lwzsama.github.io/issie-web/";

export async function packProject(project, signal) {
  signal.throwIfAborted();
  const worker = new Worker(new URL("./pack.worker.js", import.meta.url), { type: "module" });
  let abort;
  try {
    const result = await new Promise((resolve, reject) => {
      abort = () => reject(signal.reason);
      signal.addEventListener("abort", abort, { once: true });
      worker.onmessage = ({ data }) => data.error ? reject(new Error(data.error)) : resolve(data);
      worker.onerror = event => reject(new Error(event.message || "Could not load the Brotli encoder."));
      worker.postMessage(project);
    });
    const url = new URL(issieSite, location.href);
    url.hash = "project=" + result.payload;
    return { url: url.href, bytes: result.bytes };
  } finally {
    signal.removeEventListener("abort", abort);
    worker.terminate();
  }
}
