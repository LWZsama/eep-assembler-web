import { ArrowUpRight, CircuitBoard } from "lucide-react";
import { Button } from "./ui/button";
import { issieSite } from "../lib/project-link";

export function SiteHeader({ name }) {
  return (
    <header className="border-b bg-background">
      <div className="mx-auto flex h-15 max-w-6xl items-center justify-between px-5 sm:px-8">
        <div className="flex items-center gap-3 font-medium tracking-tight">
          <span className="grid size-8 place-items-center rounded-lg bg-primary text-primary-foreground">
            <CircuitBoard className="size-5" aria-hidden="true" />
          </span>
          {name}
        </div>
        <Button variant="ghost" size="sm" asChild className="text-muted-foreground">
          <a href={issieSite} target="_blank" rel="noopener noreferrer">Issie Web <ArrowUpRight /></a>
        </Button>
      </div>
    </header>
  );
}
