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
  that runs to 41. `make help` had the same bug in the other direction and
  printed `LISP ?= sbcl` as though it were help.
- `make -j check` raced three SBCLs onto one fasl cache with no locking.
  `.NOTPARALLEL:`.
- **The generated documents were not reproducible**, so "generated, so it
  cannot drift" could never have been checked: `doc/OPTIONS.txt` line 1 held
  the author's home directory, `doc/EXTENSION-SURFACE.txt` held his build tree
  on four lines, and `read by:` came out of an unordered cross-reference table.
  `make surface && git diff --exit-code doc/` runs in CI now.
- `doc/HOOKS.txt` said every hook below is attached and watched firing, above a
  body in which sixteen of eighteen printed `attached: 0`. Both were true —
  `attached` counts the generating image, which loads no tests.
- **`restart-wm` did not restart.** Bound to `Shift+Super+r`, it cleared the
  running flag and exited; `install.sh` writes `exec river -c latticewm`, which
  runs the client once and does not respawn it, so the user was left with their
  windows, no keymap, and recovery by way of a TTY.
- **The state file's version gate destroyed the file it refused.** Roll back
  after testing a build and the newer layout was gone before you could return
  to the build that wrote it. It is renamed to `state.lisp.v<n>` first.
- **`--eval` exited 0 on evaluation failure.** The wire protocol is
  `(:ok …)`/`(:error …)`; the client printed the distinction and discarded it.
- **Every closed window leaked a compositor object and a proxy** for the life
  of the session. `window-destroy` and `node-destroy` were wrapped, exported,
  documented and called by nothing.
- **Fullscreen used the cursor's output, not the window's**, so fullscreening a
  window on the second monitor from the first put it on the first.
  `output-of-window` existed, was exported, and was called by nothing.
- **Every overlay was drawn into the buffer the compositor was reading.** One
  canvas per overlay, no `wl_buffer.release` listener anywhere in `src/`, no
  `wl_surface.frame` callback, and the `canvas` docstring stated the
  consequence as though it were a feature: "the compositor is looking at the
  same bytes we are, so a write is visible as soon as the surface is
  committed." The vendored `src/protocol/wayland.xml` says the opposite in as
  many words. Overlays now own a small pool, the release event is handled, and
  a frame is drawn into a buffer river is not reading.
- **The damage tracking computed the damage and threw it away.** Two drawers
  record every rectangle they touch so the next frame clears exactly those —
  "a full-screen clear is two million writes for a few outlines" — and then
  `overlay-commit` told the compositor the whole surface had changed, forcing
  the full texture upload the bookkeeping existed to avoid. The record moved
  onto the canvas, where it is also what the damage is computed from.
- **`*overlay-buffer-idle*`'s arithmetic was wrong by 8×** in its own favour:
  `make-canvas` multiplies by the output scale before computing the stride, so
  the "about four megabytes" full-screen canvas is 33 MB on the 2× panel the
  sentence was written on. It is a list of overlay kinds now, defaulting to
  `(:help)` — large and rare — rather than `t`, which had the drawn map doing
  `mkstemp` + `ftruncate(33MB)` + `mmap` twice per wobble of a continuous zoom.
- **Undo was bolted to `run-command`**, which is one of four doors into the
  layout. `Super+;`, the control socket and a SLIME connection — the three the
  program exists for — changed the tree with no undo recorded at all, while
  every inert arrow key deep-copied every workspace to record nothing and a
  nineteen-name `*undo-exempt-commands*` deny-list papered over the arithmetic.
  Undo is now taken at settle points: `note-layout-settled` compares the tree's
  signature against a baseline and copies only on a real change, and it is
  called after commands, after the control socket, and after the REPL queue
  drains. `load-state` re-baselines against the layout it restored.

- **The flagship extension discarded whatever policy was already installed.**
  `lattice:enable` was `(setf p:*policy* (make-instance 'lattice-policy))` and
  `lattice:disable` was a fresh `conventional-policy`, so loading
  `examples/03-master-stack.lisp`, running `(master-stack)` and then enabling
  the lattice threw the master-stack policy away without a word — and disabling
  did not give it back. The teaching material and the flagship demonstrated two
  idioms that could not be used together, in a project whose central open
  question is whether a *second* party can extend it. The lattice's methods now
  specialise on `lattice-mixin`, which `enable` composes over the class of the
  policy in force with `change-class`, so slots survive and `call-next-method`
  reaches the policy underneath; `disable` restores the class it saved.
  `doc/EXTENDING.org` says which idiom to write and why.
- **`install-vocabulary` could leave the extension half installed.** It was
  `(ignore-errors (use-package '#:lattice package))` under a `handler-bind`
  hunting for `cl:continue` or `sb-impl::take-new`; with neither restart
  present the error escaped, the `use-package` was abandoned partway, and how
  many names had been imported first was unspecified — behind one `:warn` line.
  Names already spoken for are `shadowing-import`ed first, so a collision costs
  one name rather than an unknown prefix of the vocabulary.

- **`border-color` was a closed `cond` forty lines from `font-for`, which had
  solved the same problem correctly.** `font-for` takes a role keyword in
  dispatch position and its docstring celebrates that "an extension can invent
  one"; `border-color` encoded the three focus states in a five-branch `cond`,
  so a policy wanting a fourth border state — urgent, tagged, recording — forked
  the branch, on the decision that runs per window per frame. It is now the
  composition of `border-state`, which says which state a border is in, and
  `border-color-for`, which says what that state looks like, with the state in
  dispatch position for both. Each shipped state is one method reading one
  option, so it is exactly as replaceable as an invented one, and a state
  nobody gave a colour draws a plain border rather than stopping the frame.
- `*lattice-border-parity*` is gone. Its own docstring said it was superseded
  by `*coordinate-tint*`, and the branch in the lattice's `border-color` was
  kept alive to satisfy it — a superseded knob surviving the build that exists
  to find dead knobs. The `:lattice/parity` property and `tag-cell-parity`,
  which existed to feed it, went with it.

- **A modal layer could declare its keys readable and never be handed one.**
  `capture-keys` became a policy generic because the set of keys the window
  manager may ever read was a `defparameter`; *when* it reads them stayed an
  `or` of three terms in `seats.lisp` that no method could reach — so the
  fix looked complete and the feature it was for was still impossible.
  `capture-wanted-p` is the other half. The prompt term stays in the runtime,
  because the minibuffer cannot read a line if the bindings are disabled and a
  policy answering NIL would break the mechanism it would have to use to say
  so. The answer is also normalised to T or NIL now: it could return the
  pending submap itself, and the caller compares it with `eq`, so two chords in
  a row disabled and re-enabled two hundred bindings to arrive back where they
  started.

- **The lattice grew for as long as the session ran.** Arriving at a cell
  creates it, because focus has to rest on something, and nothing ever took
  one away — `tidy-grid` is deliberately manual. So crossing the plane left a
  cell behind per step, `container-addresses` sorted the accumulated pile on
  every relayout, `layout-node` walked it, and `serialize-node` wrote it to the
  state file forever. `*tidy-on-leave*` (default T) drops the cell you leave
  when arriving there is all that ever happened to it; it refuses a cell with
  anything in it, a cell somebody named, and the last cell standing. Turning
  it off restores the old behaviour, for anyone who wants the drawn map to
  show where they have been.

- **The per-frame paths allocated in seven places that had no need to.**
  `emitted` keyed one hash table on a freshly consed `(window . property)`,
  so every property of every window cost a cons and an `equal` hash on every
  frame, and forgetting a window meant walking the whole table; it is two
  levels of `eq` table now. `compute-layout` and `solo-windows` each asked
  `output-content` for every output, so a relayout asked the policy twice —
  `output-contents` asks once and both take the answer. `render-order`
  `copy-list`ed and `stable-sort`ed to do what a two-bucket partition does,
  `layout-node` built a list of what it had already placed and `member`ed it
  with a function test, `default-address` on a sequential container built the
  whole address list to take its first element, and the empty-pane hint
  `length`ed three consed lists to count windows (`node-window-count`). The
  three remaining `(dolist (window (all-windows)))` loops — two in `relayout`,
  one in the pointer hit test, which runs per motion event — consed a list of
  every window to walk it; `do-windows` walks the table.

- **One layout change asked river for N+1 manage sequences.** `request-manage`
  sent `manage_dirty` every time it was called, and every caller calls it
  unconditionally — correctly, since none of them can know what the others just
  did. River answers all of them with one sequence. It sends one `dimensions`
  event per resized window and `windows.lisp` calls `mark-dirty` for each, so
  resizing a workspace of six windows asked for seven round trips of a protocol
  whose own docstring says it is not a frame clock. A `manage-requested` flag,
  set inside the `best-effort` and after the request, cleared at
  `:manage-start` before the body of the sequence runs — so an ask made inside
  a sequence is a real ask for the next one, and a send that fails leaves the
  flag alone rather than leaving the window manager quietly deaf.

- **A plane inside a plane was two places at once.** `cursor-grid` searched the
  cursor's ancestor chain backwards and returned the *innermost* grid;
  `cursor-cell` ten lines below it and `grid-path` in `commands.lisp` searched
  forwards and answered about the *outermost*. Every caller pairs them, so
  `goto-cell` called `ensure-cell` on the inner plane and jumped the cursor to
  a path resolved against the outer one, and `cell-path` — whose whole
  docstring is about not making exactly this mistake — named a top-level cell
  of the root. `cursor-plane` is one walk returning the grid, the cell address
  and the path to the grid together, and the other three are one line each on
  top of it. `on-focus-change` was already right, by looping over every grid in
  the chain rather than picking one.

- **Turning an input setting off did nothing.** `apply-device-settings`
  filtered the policy's answer on `(null value)`, so every pair whose value was
  false was dropped on the floor: `(setf *tap-to-click* nil)` sent nothing and
  the touchpad went on tapping, and so did the rule
  `("Logitech" :natural-scroll nil)` — the shape of the example in
  `*natural-scroll*`'s own docstring. Everything above that line was correct
  and said so: `option-settings` sends the six meaningful booleans either way
  and explains why in a comment, `*input-rules*` documents that a rule which
  does not mention a key does not set it, and two tests assert that the plist
  carries `:natural-scroll nil` when a rule asks for it. The plist was right
  all the way down and its last reader threw those pairs away. `settings-to-send`
  is that decision with a name and a test: absent means leave it alone, present
  means a value, whatever the value is.
- `map.lisp` argued for its own existence in the present tense from a feature
  the program does not have — "nothing resizes to enter or leave it, so
  hold-to-peek is instant". There is no hold-to-peek: it needs `modifiers_watch`
  on `river_xkb_bindings_seat_v1`, nothing sends it, and `seats.lisp` records
  deleting the handler that waited for `modifiers_update` rather than wiring it
  up. The half of the argument that survives is the half that matters and is
  enough on its own. The `modifiers_watch` wrapper's docstring and DESIGN's
  open-question block say the same thing now.

### Added

- Gate 19 — the project says the same thing about itself everywhere it says
  one: licence, version and the number of gates that run.
- `make dist`, and this file.

## 0.1.0

The first version. A window manager for the river Wayland compositor, written
in Common Lisp, extensible from a live image over a Unix socket. `README.org`
is the tour and `doc/EXTENDING.org` is the guide; `PLAN.org` and `FINDINGS.org`
are the record of how it got here and are append-only on purpose.
