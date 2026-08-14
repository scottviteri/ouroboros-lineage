# ouroboros-lineage

**This README sits outside the organism's generation loop.** It is explanatory
context for someone arriving cold, not the output of a generation. That is a
statement about where the file enters the system, not whether its text was
produced by a human or a model.

## What this is

A lineage produced by [ouroboros](https://github.com/scottviteri/ouroboros).

`organism.el` is an Emacs Lisp file that, when loaded, sends its own source
through the kernel's model service and writes the reply over itself. The
instruction it sends is a variable inside the same file, so each generation can
rewrite the code, the prompt, or both. The kernel that runs it and mediates the
model capability lives in the other repository and is not here on purpose: the
instrument and the experimental record occupy different layers and belong
apart.

## How to read it

```sh
git log --oneline          # the phylogeny, one commit per generation
git diff <sha>~1 <sha>     # what a single generation did to itself
cat journal.md             # the kernel's account: changed / no-change / died
git diff <seed-sha> HEAD -- organism.el   # total drift since the beginning
```

Commit subjects starting with `gen ` are executions of the inner generation
loop. Other commits enter the history from outside that loop: they may provide
context or intervene in the lineage, regardless of whether their text was
produced by a human or a model. When the kernel absorbs an uncommitted
out-of-loop change before a run, it records it as an `external edit` so it
cannot be mistaken for a generation.

`journal.md` is written by the kernel and is read-only inside the sandbox. The
organism can read it but cannot edit it, so it is the one part of the record
that isn't self-reported. Death entries carry the exit code and the tail of
stderr, verbatim, with no interpretation added.

## What to expect

Nothing here is curated. Generations that made things worse are in the history
alongside ones that didn't, and a generation that dies leaves a `died` entry
and gets reverted rather than being quietly dropped. Reading it as a designed
artifact will be confusing; it is a record, not a product.

## Running your own

Clone [ouroboros](https://github.com/scottviteri/ouroboros), check
`./kernel.sh --doctor` on your host, and point `LINEAGE` at a fresh worktree.
Don't run it against this one — a lineage is a single history, and forking it
mid-stream produces a record that reads as though one thing happened when two
did.
