# ouroboros-lineage

**This file is the only human-written thing in this repository.** It is here so
that someone arriving cold knows what they are looking at. Everything else —
`organism.el` and every commit that changed it — was written either by the
organism rewriting itself or by the kernel recording what happened.

## What this is

A lineage produced by [ouroboros](https://github.com/scottviteri/ouroboros).

`organism.el` is an Emacs Lisp file that, when loaded, sends its own source to a
language model and writes the reply over itself. The instruction it sends is a
variable inside the same file, so each generation can rewrite the code, the
prompt, or both. The kernel that runs it lives in the other repository and is
not here on purpose: the instrument and the results have different authors and
belong apart.

## How to read it

```sh
git log --oneline          # the phylogeny, one commit per generation
git diff <sha>~1 <sha>     # what a single generation did to itself
cat journal.md             # the kernel's account: changed / no-change / died
git diff <seed-sha> HEAD -- organism.el   # total drift since the beginning
```

Commit subjects starting with `gen ` are generations. Anything else is a human
touching the worktree from outside, which the kernel records as an external
edit so it can't be mistaken for the organism's own work.

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
