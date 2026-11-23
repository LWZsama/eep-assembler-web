import { ArrowUpRight, CircuitBoard } from "lucide-react";
import { Button } from "./ui/button";
import { issieSite } from "../lib/project-link";

const relatedTools = [
  { label: "Issie Web", href: issieSite },
  { label: "Issie Link", href: "https://lwzsama.github.io/issie-link/" },
];

export function SiteHeader({ name }) {
  return (
    <header className="border-b bg-background">
      <div className="mx-auto flex min-h-15 max-w-6xl flex-wrap items-center justify-between gap-x-3 gap-y-1 px-5 py-2 sm:px-8">
        <div className="flex items-center gap-3 font-medium tracking-tight">
          <span className="grid size-8 place-items-center rounded-lg bg-primary text-primary-foreground">
            <CircuitBoard className="size-5" aria-hidden="true" />
          </span>
          {name}
        </div>
        <nav aria-label="Related tools" className="ml-auto flex items-center gap-1">
          {relatedTools.map(({ label, href }) => (
            <Button key={href} variant="ghost" size="sm" asChild className="text-muted-foreground">
              <a href={href} target="_blank" rel="noopener noreferrer">{label} <ArrowUpRight aria-hidden="true" className="hidden sm:block" /></a>
            </Button>
          ))}
        </nav>
      </div>
    </header>
  );
}
