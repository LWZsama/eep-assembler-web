import assert from "node:assert/strict";
import test from "node:test";
import { createServer } from "vite";

test("each bundled CPU loads the sparse assembler output into its CODEMEM source file", async () => {
  const server = await createServer({ server: { middlewareMode: true, hmr: false }, appType: "custom" });
  try {
    const { cpuProject, processors } = await server.ssrLoadModule("/src/lib/cpu-project.js");
    const ram = "0x00 0x0103\n0x20 0x1234";
    for (const cpu of Object.keys(processors)) {
      const { files } = await cpuProject(cpu, ram);
      const sheets = Object.entries(files).filter(([name]) => name.endsWith(".dgm"));
      const memories = [];
      for (const [, text] of sheets) {
        const [canvas] = Object.values(JSON.parse(text))[0];
        for (const component of canvas[0]) {
          if (component.Label === "CODEMEM") memories.push(component.Type.AsyncROM1.Init.FromFile + ".ram");
          const custom = component.Type.Custom;
          if (custom) assert.ok(Object.hasOwn(files, custom.Name + ".dgm"), cpu + " needs " + custom.Name);
        }
      }
      assert.equal(memories.length, 1, cpu);
      assert.equal(files[memories[0]], ram, cpu);
    }
  } finally {
    await server.close();
  }
});
