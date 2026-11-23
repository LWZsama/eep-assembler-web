// Standard is Issie desktop v6.3.12 (26cbdec48). The two extended circuits are from
// issie-web f22619020. Each CODEMEM component reads the RAM file named here.
export const processors = {
  standard: { label: "Standard EEP1", ram: "testloop.ram", load: () => import("../../templates/standard.mjs") },
  pipeline: { label: "Pipelined EEP1", ram: "init.ram", load: () => import("../../templates/pipeline.mjs") },
  interrupt: { label: "EEP1 with interrupts", ram: "init.ram", load: () => import("../../templates/interrupt.mjs") },
};

export async function cpuProject(processor, ram) {
  const { label, load, ram: filename } = processors[processor];
  const { default: sheets } = await load();
  const files = Object.fromEntries(Object.entries(sheets).map(([path, text]) => [path.split("/").pop(), text]));
  files[filename] = ram;
  return { name: label, files };
}
