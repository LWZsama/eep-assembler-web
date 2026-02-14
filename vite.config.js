import { defineConfig } from "vite";

export default defineConfig({
  base: "/eep-assembler-web/",
  resolve: {
    alias: {
      "fable-library-js": "@fable-org/fable-library-js",
    },
  },
});
