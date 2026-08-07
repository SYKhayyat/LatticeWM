# Changelog

All notable changes to LatticeWM.

The version lives in one file, [`VERSION`](VERSION). Both `.asd` files read it
with ASDF's `:read-file-line`, `flake.nix` reads it with `builtins.match`, and
gate 19 holds the man pages and this file to the same number. Nothing else may
hold a copy — that is what four disagreeing copies and a git tag matching none
of them bought the last time.

## Releasing

There is a procedure now, because "the one act it has no machinery for is
handing a version to somebody else" was true and is the thing a project has to
be able to do before anyone else can depend on it.

1. Edit `VERSION`. That is the whole of the version bump.
2. Move the entries below out of *Unreleased* into a new heading.
3. `make check` — all of it, including the install.
4. `make dist` — writes `dist/latticewm-<version>.tar.gz` from the git index,
   and refuses if the tree is dirty or if a tag for this version already
   exists and does not point at HEAD.
5. `git tag -a v<version>` — the `v` and the *whole* version, so `v0.1.0` and
   not `v0.1`. `make dist` checks this.

## Unreleased

### Fixed

- `flake.nix` declared `licenses.bsd3` beside 674 lines of GPLv3. Both `.asd`
  files had already been corrected from `BSD-3-Clause`; the packaging metadata,
  which is what `nix search` prints and what a distribution packager reads, had
  not been.
- The version was written out in four places and the sole git tag matched none
  of them. It is now read from `VERSION` everywhere.
- Line endings are pinned to LF by `.gitattributes`. A checkout that gave
  `install.sh` CRLF failed with `/bin/sh^M: bad interpreter`, which names
  neither the file that is wrong nor what is wrong with it.

### Added

- Gate 19 — the project says the same thing about itself everywhere it says
  one: licence, version and the number of gates that run.
- `make dist`, and this file.

## 0.1.0

The first version. A window manager for the river Wayland compositor, written
in Common Lisp, extensible from a live image over a Unix socket. `README.org`
is the tour and `doc/EXTENDING.org` is the guide; `PLAN.org` and `FINDINGS.org`
are the record of how it got here and are append-only on purpose.
