import { useCallback, useLayoutEffect, useRef, useState } from "react";
import { createRoot } from "react-dom/client";
import { Binary, Code2, Download, FileUp, Play } from "lucide-react";
import { SiteHeader } from "./components/SiteHeader";
import { ProjectLink } from "./components/ProjectLink";
import { Button } from "./components/ui/button";
import { NativeSelect, NativeSelectOption } from "./components/ui/native-select";
import { processors, cpuProject } from "./lib/cpu-project";
import { mountEditor } from "../tools.js";
import "./theme.css";
import "./editor.css";

const sample = "MOV R0, #3\nMOV R1, R0\nADD R1, #1\nADD R3, R0, R1";
const panel = "flex flex-col rounded-xl border bg-card shadow-sm";
const caption = "flex h-12 items-center gap-2 border-b px-4 text-sm font-medium";

function Workspace({ host }) {
  return (
    <div className="workspace" ref={host}>
      <section aria-labelledby="source-heading" className={panel}>
        <h2 className={caption} id="source-heading"><Code2 className="size-4 text-muted-foreground" />Assembly source</h2>
        <div className="editor-shell">
          <pre id="line-numbers" className="line-numbers" aria-hidden="true" />
          <div className="editor-stage">
            <div id="active-line" aria-hidden="true" />
            <pre id="highlight" className="editor-layer" aria-hidden="true" />
            <textarea id="src" className="editor-layer" aria-label="Assembly source" aria-description="Tab indents. Escape leaves the editor."
              spellCheck={false} wrap="off" defaultValue={sample} />
          </div>
        </div>
        <div className="flex flex-wrap justify-between gap-2 border-t px-4 py-2 font-mono text-xs text-muted-foreground">
          <span id="cursor-status" /><span id="document-status" />
        </div>
      </section>
      <div className="workspace-divider" role="separator" tabIndex={0} aria-label="Resize editor and output panels"
        aria-orientation="vertical" aria-valuemin="0" aria-valuemax="100" aria-valuenow="62" />
      <section aria-labelledby="output-heading" className={panel}>
        <h2 className={caption} id="output-heading"><Binary className="size-4 text-muted-foreground" />Machine code</h2>
        <div id="out" className="output" role="table" aria-label="Machine code output" />
      </section>
    </div>
  );
}

function App() {
  const host = useRef(null);
  const editor = useRef(null);
  const picker = useRef(null);
  const [ram, setRam] = useState("");
  const [processor, setProcessor] = useState("standard");
  const [error, setError] = useState("");
  const prepare = useCallback(() => cpuProject(processor, ram), [processor, ram]);
  useLayoutEffect(() => {
    editor.current = mountEditor(host.current, setRam);
    return () => editor.current.dispose();
  }, []);
  async function save() {
    setError("");
    try { await editor.current.saveRam(); }
    catch (err) { if (err.name !== "AbortError") setError(err.message); }
  }

  return (
    <>
      <SiteHeader name="EEP1 Assembler" />
      <main className="mx-auto max-w-6xl space-y-6 px-5 pb-16 pt-10 sm:px-8 sm:pt-14">
        <div className="space-y-2">
          <h1 className="text-3xl font-semibold tracking-tight sm:text-4xl">Assembly workspace</h1>
          <p className="text-muted-foreground">Assemble an EEP1 program and run it on the CPU of your choice.</p>
        </div>
        <div className="flex flex-wrap items-center gap-2">
          <Button onClick={() => editor.current.assemble()}><Play />Assemble</Button>
          <Button variant="outline" onClick={() => picker.current.click()}><FileUp />Import source</Button>
          <Button variant="outline" disabled={!ram} onClick={save}><Download />Export .ram</Button>
          <span className="ml-auto hidden items-center gap-1 text-xs text-muted-foreground sm:flex">
            <kbd className="rounded border px-1.5 py-0.5">{/mac/i.test(navigator.platform) ? "⌘" : "Ctrl"}</kbd> + <kbd className="rounded border px-1.5 py-0.5">Enter</kbd> to assemble
          </span>
          <input hidden ref={picker} type="file" accept=".asm,.txt,text/plain" aria-label="Import assembly source"
            onChange={async event => {
              const file = event.target.files[0]; event.target.value = "";
              if (!file) return;
              try { editor.current.setSource(await file.text()); setError(""); }
              catch (err) { setError(err.message); }
            }} />
        </div>
        {error && <p role="alert" className="text-sm text-destructive">{error}</p>}
        <Workspace host={host} />
        <ProjectLink prepare={ram ? prepare : null} empty="Assemble a program to generate a link">
          <label className="flex items-center gap-2 text-sm text-muted-foreground">CPU
            <NativeSelect aria-label="CPU model" value={processor} onChange={event => setProcessor(event.target.value)}>
              {Object.entries(processors).map(([id, cpu]) => <NativeSelectOption key={id} value={id}>{cpu.label}</NativeSelectOption>)}
            </NativeSelect>
          </label>
        </ProjectLink>
      </main>
    </>
  );
}

createRoot(document.getElementById("root")).render(<App />);
