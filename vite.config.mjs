import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  base: "./",
  plugins: [tailwindcss()],
  // Keep the WASM URL relative to the encoder module when Vite serves the Worker in dev.
  optimizeDeps: { exclude: ["brotli-wasm"] },
  esbuild: { jsx: "automatic" },
  worker: { format: "es" },
});
