import { useEffect, useRef, useState } from "react";
import { ArrowUpRight, Copy, Link2, LoaderCircle, Share2 } from "lucide-react";
import { Button } from "./ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "./ui/card";
import { Input } from "./ui/input";
import { packProject } from "../lib/project-link";

export function ProjectLink({ prepare, empty, children }) {
  const [result, setResult] = useState(null);
  const [notice, setNotice] = useState(null);
  const input = useRef(null);
  useEffect(() => {
    if (!prepare) return;
    const request = new AbortController();
    (async () => {
      try {
        const project = await prepare(request.signal);
        const link = await packProject(project, request.signal);
        if (!request.signal.aborted) setResult({ prepare, ...link });
      } catch (error) {
        if (!request.signal.aborted) setResult({ prepare, error: error.message || String(error) });
      }
    })();
    return () => request.abort();
  }, [prepare]);

  const current = prepare && result?.prepare === prepare ? result : null;
  const url = current?.url || "";
  const busy = Boolean(prepare && !current);
  const message = current?.error || (busy ? "Preparing project link…" : notice?.url === url ? notice.text : "");
  async function copy(forShare = false) {
    try {
      await navigator.clipboard.writeText(url);
      setNotice({ url, text: forShare ? "Link copied for sharing." : "Link copied." });
    } catch {
      input.current.focus();
      input.current.select();
      setNotice({ url, text: "The link is selected. Use your device’s Copy command." });
    }
  }
  async function share() {
    const data = { title: "Issie project", url };
    if (navigator.share && (!navigator.canShare || navigator.canShare(data))) {
      try { await navigator.share(data); return; }
      catch (error) { if (error.name === "AbortError") return; }
    }
    await copy(true);
  }

  return (
    <Card className="gap-4" aria-label="Project link">
      <CardHeader className="flex flex-wrap items-center justify-between gap-3">
        <CardTitle className="flex items-center gap-2 text-base"><Link2 className="size-4 text-muted-foreground" /> Project link</CardTitle>
        {children}
      </CardHeader>
      <CardContent className="space-y-4">
        <Input ref={input} readOnly value={url} aria-label="Issie Web project URL" onFocus={e => e.target.select()}
          placeholder={busy ? "Compressing with Brotli…" : empty} className="bg-background font-mono text-xs md:text-xs" />
        <div className="flex flex-wrap items-center gap-2">
          {url ? <Button asChild><a href={url} target="_blank" rel="noopener noreferrer">Open in Issie Web <ArrowUpRight /></a></Button>
            : <Button disabled>Open in Issie Web <ArrowUpRight /></Button>}
          <Button variant="outline" disabled={!url} onClick={() => copy()}><Copy /> Copy link</Button>
          <Button variant="ghost" disabled={!url} onClick={share}><Share2 /> Share</Button>
          {url && <span className="text-xs text-muted-foreground sm:ml-auto">{(current.bytes / 1024).toFixed(1)} KiB · {url.length.toLocaleString()} characters</span>}
        </div>
        <div role="status" aria-live="polite" className={message ? "flex items-center gap-2 text-sm " + (current?.error ? "text-destructive" : "text-muted-foreground") : "sr-only"}>
          {busy && <LoaderCircle className="size-4 animate-spin motion-reduce:animate-none" />}{message || (url && "Project link ready.")}
        </div>
      </CardContent>
    </Card>
  );
}
