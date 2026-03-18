import { assembleText } from "./assembler.js";

const src = document.getElementById("src");
const highlight = document.getElementById("highlight");
const lineNumbers = document.getElementById("line-numbers");
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

const OPCODES = new Set([
  "ADC", "ADD", "AND", "CALL", "CMP", "DEC", "DIV", "HALT", "INC",
  "JC", "JGE", "JG", "JLE", "JL", "JMP", "JNC", "JNE", "JNZ", "JE",
  "JZ", "LD", "MOV", "MUL", "NOP", "NOT", "OR", "POP", "PUSH", "RET",
  "SBB", "SHL", "SHR", "ST", "SUB", "TEST", "XOR"
]);

const DIRECTIVES = new Set([
  "DB", "DS", "DW", "END", "EQU", "INCLUDE", "MACRO", "ENDM", "ORG"
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
    const prev = index > 0 ? line[index - 1] : "";

    if ((char === '"' || char === "'") && prev !== "\\") {
      if (quote === char) {
        quote = null;
      } else if (!quote) {
        quote = char;
      }
      continue;
    }

    if (char === ";" && !quote) {
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

function parseMachineCodeWords(machineCodeText) {
  if (!machineCodeText.trim()) {
    return [];
  }

  return machineCodeText
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
    .filter((entry) => entry !== null)
    .sort((left, right) => left.address - right.address);
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
      out.textContent = lastMachineCodeOutput;
      return true;
    }

    if (res && res.tag === 1) {
      clearCompiledArtifacts();
      out.textContent = `ERROR\n${res.fields[0]}`;
      return false;
    }

    clearCompiledArtifacts();
    out.textContent = `Unknown result: ${String(res)}`;
    return false;
  } catch (error) {
    clearCompiledArtifacts();
    out.textContent = `JavaScript error\n${error && error.stack ? error.stack : String(error)}`;
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
    out.textContent = "Run Assemble first to generate machine code before exporting.";
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
    out.textContent = `Import failed\n${error?.message || String(error)}`;
  });
});

downloadBtn.addEventListener("click", () => {
  saveRam().catch((error) => {
    if (error?.name === "AbortError") {
      return;
    }

    out.textContent = `Export failed\n${error?.message || String(error)}`;
  });
});

openIssieBtn.addEventListener("click", openIssieDialog);

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
