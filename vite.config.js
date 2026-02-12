import { defineConfig } from "vite";

export default defineConfig({
  resolve: {
    alias: {
      "fable-library-js": "@fable-org/fable-library-js",
    },
  },
});
