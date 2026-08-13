import { assembleText } from "./assembler.js";

const src = document.getElementById("src");
const highlight = document.getElementById("highlight");
const lineNumbers = document.getElementById("line-numbers");
const editorShell = document.querySelector(".editor-shell");
const workspace = document.querySelector(".workspace");
const workspaceDivider = document.querySelector(".workspace-divider");
const activeLine = document.getElementById("active-line");
const cursorStatus = document.getElementById("cursor-status");
const documentStatus = document.getElementById("document-status");
const out = document.getElementById("out");
const runBtn = document.getElementById("run");
const importBtn = document.getElementById("import");
const downloadBtn = document.getElementById("download");
const openIssieBtn = document.getElementById("open-issie");
const issieLinkInput = document.getElementById("issie-link");
const issieLinkAnchor = document.getElementById("issie-link-anchor");
const issieDialog = document.getElementById("issie-dialog");
const closeIssieDialogBtn = document.getElementById("close-issie-dialog");

let lastMachineCodeOutput = "";
let caretRefreshFrame = 0;
let editorBaseMinHeight = 0;
let workspaceSplitRatio = null;
let workspaceResizeFrame = 0;

const DEFAULT_WORKSPACE_SPLIT = 1.45 / (1.45 + 0.9);
const WORKSPACE_DIVIDER_WIDTH = 16;
const WORKSPACE_MIN_PANE_WIDTH = 320;

const OPCODES = new Set([
  "ADC", "ADD", "AND", "CALL", "CMP", "DEC", "DIV", "HALT", "INC",
  "JC", "JGE", "JG", "JLE", "JL", "JMP", "JNC", "JNE", "JNZ", "JE",
  "JZ", "LD", "MOV", "MUL", "NOP", "NOT", "OR", "POP", "PUSH", "RET",
  "SBB", "SHL", "SHR", "ST", "SUB", "TEST", "XOR"
]);

const DIRECTIVES = new Set([
  "DB", "DS", "DW", "END", "EQU", "INCLUDE", "MACRO", "ENDM", "ORG"
]);

const ASSEMBLY_INSTRUCTIONS = new Set([
  ...OPCODES,
  ...DIRECTIVES,
  "ASR", "CLRI", "DCW", "EXT", "JEQ", "JCC", "JCS", "JGT", "JHI", "JLS",
  "JMI", "JPL", "JSR", "LDR", "LSL", "LSR", "RETINT", "SBC", "SETI", "STR", "XSR"
]);

const TOKEN_REGEX = /(?:[A-Za-z_][A-Za-z0-9_]*:)|(?:\.[A-Za-z_][A-Za-z0-9_]*)|(?:#-?(?:0x[\da-fA-F]+|\d+))|(?:\b0x[\da-fA-F]+\b|\b\d+\b)|(?:\bR(?:1[0-5]|[0-9])\b)|(?:"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*')|(?:\b[A-Za-z_][A-Za-z0-9_]*\b)|(?:[,()[\]{}:+\-*/])/y;

const escapeHtml = (text) =>
  text
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;");

function findCommentStart(line) {
  let quote = null;

  for (let index = 0; index < line.length; index += 1) {
    const char = line[index];
    const next = line[index + 1] || "";
    const prev = index > 0 ? line[index - 1] : "";

    if ((char === '"' || char === "'") && prev !== "\\") {
      if (quote === char) {
        quote = null;
      } else if (!quote) {
        quote = char;
      }
      continue;
    }

    if (!quote && (char === ";" || (char === "/" && next === "/"))) {
      return index;
    }
  }

  return -1;
}

function wrapToken(token, className) {
  return `<span class="${className}">${escapeHtml(token)}</span>`;
}

function renderToken(token) {
  if (/^[A-Za-z_][A-Za-z0-9_]*:$/.test(token)) {
    return wrapToken(token, "token-label");
  }

  if (/^\.[A-Za-z_][A-Za-z0-9_]*$/.test(token)) {
    return wrapToken(token, "token-directive");
  }

  if (/^#-?(?:0x[\da-fA-F]+|\d+)$/.test(token) || /^(?:0x[\da-fA-F]+|\d+)$/.test(token)) {
    return wrapToken(token, "token-num");
  }

  if (/^R(?:1[0-5]|[0-9])$/i.test(token)) {
    return wrapToken(token, "token-reg");
  }

  if (/^"(?:[^"\\]|\\.)*"$/.test(token) || /^'(?:[^'\\]|\\.)*'$/.test(token)) {
    return wrapToken(token, "token-string");
  }

  if (/^[,()[\]{}:+\-*/]$/.test(token)) {
    return wrapToken(token, "token-punct");
  }

  const upper = token.toUpperCase();

  if (OPCODES.has(upper)) {
    return wrapToken(token, "token-op");
  }

  if (DIRECTIVES.has(upper)) {
    return wrapToken(token, "token-directive");
  }

  return wrapToken(token, "token-ident");
}

function renderCode(code) {
  let html = "";
  let index = 0;

  while (index < code.length) {
    TOKEN_REGEX.lastIndex = index;
    const match = TOKEN_REGEX.exec(code);

    if (!match) {
      html += escapeHtml(code[index]);
      index += 1;
      continue;
    }

    if (match.index > index) {
      html += escapeHtml(code.slice(index, match.index));
    }

    const token = match[0];
    html += renderToken(token);
    index = TOKEN_REGEX.lastIndex;
  }

  return html;
}

function highlightAsm(source) {
  return source
    .split("\n")
    .map((line) => {
      const commentStart = findCommentStart(line);
      const code = commentStart >= 0 ? line.slice(0, commentStart) : line;
      const comment = commentStart >= 0 ? line.slice(commentStart) : "";
      const codeHtml = renderCode(code);
      const commentHtml = comment ? wrapToken(comment, "token-comment") : "";
      return `${codeHtml}${commentHtml}`;
    })
    .join("\n");
}

function getLineCount() {
  return Math.max(1, src.value.split("\n").length);
}

function updateLineNumbers() {
  const lineCount = getLineCount();
  lineNumbers.textContent = Array.from({ length: lineCount }, (_, index) => String(index + 1)).join("\n");
}

function getCursorPosition() {
  const position = src.selectionStart ?? 0;
  const beforeCursor = src.value.slice(0, position).split("\n");
  const line = beforeCursor.length;
  const column = beforeCursor[beforeCursor.length - 1].length + 1;
  return { line, column };
}

function updateCursorStatus() {
  const { line, column } = getCursorPosition();
  cursorStatus.innerHTML = `<strong>Ln ${line}, Col ${column}</strong>`;
}

function updateDocumentStatus() {
  const lineCount = getLineCount();
  const charCount = src.value.length;
  documentStatus.textContent = `${lineCount} lines | ${charCount} chars | UTF-8`;
}

function updateEditorHeight() {
  if (!editorShell) {
    return;
  }

  const styles = window.getComputedStyle(src);
  const lineHeight = Number.parseFloat(styles.lineHeight) || 24;
  const paddingTop = Number.parseFloat(styles.paddingTop) || 0;
  const paddingBottom = Number.parseFloat(styles.paddingBottom) || 0;

  if (!editorBaseMinHeight) {
    editorBaseMinHeight = editorShell.getBoundingClientRect().height;
  }

  const contentHeight = paddingTop + paddingBottom + getLineCount() * lineHeight;
  const minHeight = Math.max(editorBaseMinHeight, contentHeight);
  editorShell.style.minHeight = `${minHeight}px`;
}

function isWorkspaceStacked() {
  return window.matchMedia("(max-width: 1024px)").matches;
}

function clampWorkspaceSplit(ratio, availableWidth) {
  const minRatio = WORKSPACE_MIN_PANE_WIDTH / availableWidth;
  const maxRatio = 1 - minRatio;
  return Math.min(Math.max(ratio, minRatio), maxRatio);
}

function applyWorkspaceSplit() {
  if (!workspace || !workspaceDivider) {
    return;
  }

  if (isWorkspaceStacked()) {
    workspace.style.gridTemplateColumns = "";
    return;
  }

  if (workspaceSplitRatio === null) {
    return;
  }

  const workspaceWidth = workspace.getBoundingClientRect().width;
  const availableWidth = workspaceWidth - WORKSPACE_DIVIDER_WIDTH;
  const ratio = clampWorkspaceSplit(workspaceSplitRatio, availableWidth);
  const editorWidth = availableWidth * ratio;

  workspaceSplitRatio = ratio;
  workspace.style.gridTemplateColumns = `${editorWidth}px ${WORKSPACE_DIVIDER_WIDTH}px minmax(${WORKSPACE_MIN_PANE_WIDTH}px, 1fr)`;
  workspaceDivider.setAttribute("aria-valuenow", String(Math.round(ratio * 100)));
  workspaceDivider.setAttribute("aria-valuetext", `${Math.round(ratio * 100)}% editor width`);
}

function setWorkspaceSplitFromPointer(clientX, pointerOffset) {
  if (!workspace) {
    return;
  }

  const workspaceRect = workspace.getBoundingClientRect();
  const availableWidth = workspaceRect.width - WORKSPACE_DIVIDER_WIDTH;
  const editorWidth = clientX - workspaceRect.left - pointerOffset;

  workspaceSplitRatio = clampWorkspaceSplit(editorWidth / availableWidth, availableWidth);
  applyWorkspaceSplit();
}

function handleWorkspaceDividerPointerDown(event) {
  if (!workspace || !workspaceDivider || isWorkspaceStacked() || event.button !== 0) {
    return;
  }

  event.preventDefault();

  const dividerRect = workspaceDivider.getBoundingClientRect();
  const pointerOffset = event.clientX - dividerRect.left;

  workspace.classList.add("is-resizing");
  document.body.classList.add("is-resizing");
  workspaceDivider.setPointerCapture?.(event.pointerId);

  const handlePointerMove = (moveEvent) => {
    setWorkspaceSplitFromPointer(moveEvent.clientX, pointerOffset);
  };

  const stopDragging = () => {
    workspace.classList.remove("is-resizing");
    document.body.classList.remove("is-resizing");
    window.removeEventListener("pointermove", handlePointerMove);
    window.removeEventListener("pointerup", stopDragging);
    window.removeEventListener("pointercancel", stopDragging);
    workspaceDivider.releasePointerCapture?.(event.pointerId);
  };

  window.addEventListener("pointermove", handlePointerMove);
  window.addEventListener("pointerup", stopDragging);
  window.addEventListener("pointercancel", stopDragging);
  setWorkspaceSplitFromPointer(event.clientX, pointerOffset);
}

function handleWorkspaceDividerKeydown(event) {
  if (!workspaceDivider || isWorkspaceStacked()) {
    return;
  }

  if (event.key !== "ArrowLeft" && event.key !== "ArrowRight") {
    return;
  }

  event.preventDefault();

  if (workspaceSplitRatio === null) {
    workspaceSplitRatio = DEFAULT_WORKSPACE_SPLIT;
  }

  const amount = event.shiftKey ? 0.05 : 0.02;
  workspaceSplitRatio += event.key === "ArrowRight" ? amount : -amount;
  applyWorkspaceSplit();
}

function updateActiveLine() {
  const { line } = getCursorPosition();
  const styles = window.getComputedStyle(src);
  const lineHeight = Number.parseFloat(styles.lineHeight) || 24;
  const paddingTop = Number.parseFloat(styles.paddingTop) || 0;
  const top = paddingTop + (line - 1) * lineHeight - src.scrollTop;

  activeLine.style.top = `${top}px`;
  activeLine.style.height = `${lineHeight}px`;
}

function refreshCaretUi() {
  updateCursorStatus();
  updateActiveLine();
}

function scheduleCaretUiRefresh() {
  if (caretRefreshFrame) {
    cancelAnimationFrame(caretRefreshFrame);
  }

  caretRefreshFrame = window.requestAnimationFrame(() => {
    caretRefreshFrame = 0;
    refreshCaretUi();
  });
}

function syncEditor() {
  highlight.innerHTML = `${highlightAsm(src.value)}\n`;
  highlight.scrollTop = src.scrollTop;
  highlight.scrollLeft = src.scrollLeft;
  lineNumbers.scrollTop = src.scrollTop;
  updateLineNumbers();
  updateEditorHeight();
  refreshCaretUi();
  updateDocumentStatus();
}

function updateIssieLink(url = "") {
  issieLinkInput.value = url;

  if (url) {
    issieLinkAnchor.href = url;
    issieLinkAnchor.hidden = false;
    return;
  }

  issieLinkAnchor.href = "#";
  issieLinkAnchor.hidden = true;
}

function clearCompiledArtifacts() {
  lastMachineCodeOutput = "";
  updateIssieLink("");
}

function parseAddressToken(token) {
  return /^0x/i.test(token) || /[a-f]/i.test(token)
    ? Number.parseInt(token.replace(/^0x/i, ""), 16)
    : Number.parseInt(token, 10);
}

function parseMachineCodeWords(machineCodeText, { sortByAddress = true } = {}) {
  if (!machineCodeText.trim()) {
    return [];
  }

  const entries = machineCodeText
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter(Boolean)
    .map((line) => {
      const match = line.match(/^(\S+)\s+(\S+)$/);

      if (!match) {
        return null;
      }

      const address = parseAddressToken(match[1]);
      const rawWord = match[2].replace(/^0x/i, "");

      if (!Number.isFinite(address) || !/^[0-9a-fA-F]{1,4}$/.test(rawWord)) {
        return null;
      }

      return {
        address,
        word: rawWord.toLowerCase().padStart(4, "0")
      };
    })
    .filter((entry) => entry !== null);

  return sortByAddress
    ? entries.sort((left, right) => left.address - right.address)
    : entries;
}

function findAssemblyCommentStart(line) {
  let quote = null;

  for (let index = 0; index < line.length; index += 1) {
    const char = line[index];
    const next = line[index + 1] || "";
    const prev = index > 0 ? line[index - 1] : "";

    if ((char === '"' || char === "'") && prev !== "\\") {
      if (quote === char) {
        quote = null;
      } else if (!quote) {
        quote = char;
      }
      continue;
    }

    if (!quote && (char === ";" || (char === "/" && next === "/"))) {
      return index;
    }
  }

  return -1;
}

function sourceLineProducesMachineCode(line) {
  const commentStart = findAssemblyCommentStart(line);
  const code = (commentStart >= 0 ? line.slice(0, commentStart) : line)
    .replace(/^\s*[A-Za-z_][A-Za-z0-9_]*:\s*/, "")
    .trim();

  if (!code || /^(?:\.?ORG)\b/i.test(code)) {
    return false;
  }

  const tokens = code.split(/\s+/);
  const firstToken = tokens[0].toUpperCase();

  if (tokens.length > 1 && !ASSEMBLY_INSTRUCTIONS.has(firstToken) && /^(?:\.?ORG)$/i.test(tokens[1])) {
    return false;
  }

  return tokens.length > 1 || ASSEMBLY_INSTRUCTIONS.has(firstToken);
}

function formatMachineCodeEntry(entry) {
  const address = `0x${entry.address.toString(16).padStart(2, "0")}`;
  return `${address} 0x${entry.word}`;
}

function renderMachineCodeOutput(machineCodeText) {
  const sourceLines = src.value.replace(/\r\n/g, "\n").replace(/\r/g, "\n").split("\n");
  const entries = parseMachineCodeWords(machineCodeText, { sortByAddress: false });
  let entryIndex = 0;

  const rows = sourceLines.map((sourceLine, index) => {
    const hasMachineCode = sourceLineProducesMachineCode(sourceLine);
    const entry = hasMachineCode ? entries[entryIndex++] : null;
    const codeLabel = entry ? formatMachineCodeEntry(entry) : "—";
    const rowClass = entry ? "output-row" : "output-row output-empty";

    return `<div class="${rowClass}" role="row">
      <span class="output-line" role="cell">${index + 1}</span>
      <code class="output-code" role="cell">${escapeHtml(codeLabel)}</code>
    </div>`;
  });

  while (entryIndex < entries.length) {
    const entry = entries[entryIndex++];
    rows.push(`<div class="output-row" role="row">
      <span class="output-line" role="cell">—</span>
      <code class="output-code" role="cell">${escapeHtml(formatMachineCodeEntry(entry))}</code>
    </div>`);
  }

  out.innerHTML = rows.join("");
}

function renderOutputMessage(message) {
  out.innerHTML = `<pre class="output-message">${escapeHtml(message)}</pre>`;
}

function buildIssieUrl(demo) {
  const words = parseMachineCodeWords(lastMachineCodeOutput);

  if (words.length === 0) {
    return null;
  }

  const code = words.map((entry) => entry.word).join("");
  return `https://lwzsama.github.io/issie-web/#demo=${demo}&code=${code}`;
}

function warnMissingMachineCode() {
  window.alert("No assembled machine code is available. Run Assemble successfully first.");
}

function openIssieDialog() {
  if (!lastMachineCodeOutput.trim()) {
    warnMissingMachineCode();
    return;
  }

  if (typeof issieDialog.showModal === "function") {
    issieDialog.showModal();
    return;
  }

  const selected = window.prompt(
    "Choose a CPU demo: 3 = Normal EEP1 CPU, 6 = EEP1 CPU with Pipeline, 7 = EEP1 CPU with Interupt",
    "3"
  );

  if (selected === "3" || selected === "6" || selected === "7") {
    handleIssieSelection(selected);
  }
}

function handleIssieSelection(demo) {
  const url = buildIssieUrl(demo);

  if (!url) {
    warnMissingMachineCode();
    return;
  }

  updateIssieLink(url);
  window.open(url, "_blank", "noopener,noreferrer");
}

function assembleSource() {
  try {
    const res = assembleText(src.value);

    if (res && res.tag === 0) {
      lastMachineCodeOutput = res.fields[0];
      updateIssieLink("");
      renderMachineCodeOutput(lastMachineCodeOutput);
      return true;
    }

    if (res && res.tag === 1) {
      clearCompiledArtifacts();
      renderOutputMessage(`ERROR\n${res.fields[0]}`);
      return false;
    }

    clearCompiledArtifacts();
    renderOutputMessage(`Unknown result: ${String(res)}`);
    return false;
  } catch (error) {
    clearCompiledArtifacts();
    renderOutputMessage(`JavaScript error\n${error && error.stack ? error.stack : String(error)}`);
    return false;
  }
}

async function importTxt() {
  const input = document.createElement("input");
  input.type = "file";
  input.accept = ".txt,.asm,text/plain";

  input.onchange = async () => {
    const file = input.files?.[0];
    if (!file) {
      return;
    }

    src.value = await file.text();
    clearCompiledArtifacts();
    syncEditor();
  };

  input.click();
}

async function saveRam() {
  const content = lastMachineCodeOutput.trim();

  if (!content) {
    renderOutputMessage("Run Assemble first to generate machine code before exporting.");
    return;
  }

  const filename = `machine-code-${new Date().toISOString().slice(0, 19).replaceAll(":", "-")}.ram`;

  if (window.showSaveFilePicker) {
    const handle = await window.showSaveFilePicker({
      suggestedName: filename,
      types: [{
        description: "RAM text file",
        accept: { "text/plain": [".ram"] }
      }]
    });

    const writable = await handle.createWritable();
    await writable.write(content);
    await writable.close();
    return;
  }

  const blob = new Blob([content], { type: "text/plain;charset=utf-8" });
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");
  link.href = url;
  link.download = filename;
  link.click();
  URL.revokeObjectURL(url);
}

function handleEditorKeydown(event) {
  if (event.key === "Tab") {
    event.preventDefault();

    const start = src.selectionStart ?? 0;
    const end = src.selectionEnd ?? 0;
    const insertion = "  ";

    src.value = `${src.value.slice(0, start)}${insertion}${src.value.slice(end)}`;
    src.selectionStart = start + insertion.length;
    src.selectionEnd = start + insertion.length;
    syncEditor();
    return;
  }

  if ((event.ctrlKey || event.metaKey) && event.key === "Enter") {
    event.preventDefault();
    assembleSource();
  }
}

runBtn.addEventListener("click", assembleSource);
importBtn.addEventListener("click", () => {
  importTxt().catch((error) => {
    renderOutputMessage(`Import failed\n${error?.message || String(error)}`);
  });
});

downloadBtn.addEventListener("click", () => {
  saveRam().catch((error) => {
    if (error?.name === "AbortError") {
      return;
    }

    renderOutputMessage(`Export failed\n${error?.message || String(error)}`);
  });
});

openIssieBtn.addEventListener("click", openIssieDialog);

workspaceDivider?.addEventListener("pointerdown", handleWorkspaceDividerPointerDown);
workspaceDivider?.addEventListener("keydown", handleWorkspaceDividerKeydown);
window.addEventListener("resize", () => {
  if (workspaceResizeFrame) {
    cancelAnimationFrame(workspaceResizeFrame);
  }

  workspaceResizeFrame = window.requestAnimationFrame(() => {
    workspaceResizeFrame = 0;
    applyWorkspaceSplit();
  });
});

closeIssieDialogBtn.addEventListener("click", () => {
  issieDialog.close();
});

issieDialog.addEventListener("click", (event) => {
  if (event.target === issieDialog) {
    issieDialog.close();
  }
});

issieDialog.querySelectorAll("[data-demo]").forEach((button) => {
  button.addEventListener("click", () => {
    const demo = button.getAttribute("data-demo");

    issieDialog.close();

    if (demo) {
      handleIssieSelection(demo);
    }
  });
});

src.addEventListener("input", syncEditor);
src.addEventListener("input", clearCompiledArtifacts);
src.addEventListener("scroll", syncEditor);
src.addEventListener("click", scheduleCaretUiRefresh);
src.addEventListener("focus", scheduleCaretUiRefresh);
src.addEventListener("mouseup", scheduleCaretUiRefresh);
src.addEventListener("keydown", handleEditorKeydown);
src.addEventListener("keyup", scheduleCaretUiRefresh);
src.addEventListener("select", scheduleCaretUiRefresh);
document.addEventListener("selectionchange", () => {
  if (document.activeElement === src) {
    scheduleCaretUiRefresh();
  }
});

syncEditor();
clearCompiledArtifacts();
assembleSource();
