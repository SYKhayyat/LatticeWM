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
- `make install` shipped the 190 MB uncompressed image. `image` forced
  `LATTICEWM_COMPRESS=0` and `release` — named by no document, no CI job and
  no gate — produced the 13 MB one, so the nix path shipped 13 MB and every
  other Linux shipped 190.
- No `DESTDIR`, so no distribution could package this. `install.sh --destdir`
  and `make DESTDIR=… install` now stage correctly, with `--no-config`
  implied; `install-check` runs the staged path and asserts the launcher and
  the session entry name the *prefix* rather than the staging root.
- The SBCL floor is declared (2.2.6, for zstd core compression at level 22)
  and checked in `latticewm.asd`, `tools/prelude.lisp` and `bootstrap.sh`.
  `bootstrap.sh` used to print the version and check nothing.
- **The Quicklisp dist is pinned.** `bootstrap.sh`'s header claimed the nix
  and non-nix paths "compile the same source"; `quicklisp-quickstart:install`
  was called with no dist pin and took whatever was newest on the day it ran,
  so the claim described an intention rather than a mechanism and CI's `plain`
  job compiled a different wayflan from the `check` job on a schedule nobody
  controlled. `QUICKLISP_DIST=2026-01-01` is the same tree the nixpkgs wayflan
  package pins, which is what makes the sentence true.
- **The checksum moved onto the file that moves.** `QUICKLISP_SHA256` pinned
  `quicklisp.lisp`, the bootstrapper, which the comment beside it correctly
  described as byte-identical for years — a checksum on the one thing that
  cannot move and none at all on the thing republished monthly.
  `QUICKLISP_DIST_SHA256` verifies the installed `distinfo.txt`; the fetch
  itself is `http` because Quicklisp's own client speaks nothing else, which
  is exactly why the verification exists.
- `install.sh --help` ended mid-clause: it was `sed -n '2,40p'` over a header
  that runs to 41.

### Added

- Gate 19 — the project says the same thing about itself everywhere it says
  one: licence, version and the number of gates that run.
- `make dist`, and this file.

## 0.1.0

The first version. A window manager for the river Wayland compositor, written
in Common Lisp, extensible from a live image over a Unix socket. `README.org`
is the tour and `doc/EXTENDING.org` is the guide; `PLAN.org` and `FINDINGS.org`
are the record of how it got here and are append-only on purpose.
