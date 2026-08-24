# Changelog

All notable changes to LatticeWM.

The version lives in one file, [`VERSION`](VERSION). Both `.asd` files read it
with ASDF's `:read-file-line`, `flake.nix` reads it with `builtins.match`, and
gate 19 holds the man pages and this file to the same number — this file since
0.2.0, when it turned out that the sentence you are reading had been describing
a check nobody had written. Nothing else may hold a copy: that is what four
disagreeing copies and a git tag matching none of them bought the last time.

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

### Added

- `extensions/keyboard-macros/` — record command sequences between
  `start-macro` and `stop-macro`, play them back with an optional count,
  and save them under names. Recording is a single wrapper on the command
  path; prompts are included because their answers arrive as arguments.
- `extensions/command-journal/` — record every command run while recording
  is on, as plain `(command . arguments)` forms; save journals to files,
  load them into any session, and replay through the ordinary command path
  with per-entry failure tolerance. Undo/redo/repeat and the journal's own
  verbs are excluded.
- `extensions/window-restarts/` — when a window exits unexpectedly (a close
  you did not just ask for), a `broken-window` condition is signalled and
  the echo area offers the menu: retry respawns the application, undo walks
  back the retile, dismiss carries on. Bind the exported `*menu*` keymap
  behind any chord.
- `extensions/layout-persistence/` — named workspace layouts saved as files
  and keyed by application id, so an arrangement means the same thing after
  a reboot. Restore is `:best-effort` (empty panes for absent applications)
  or `:exact` (refuses whole rather than half-honour); windows on the
  workspace that the arrangement does not name come back as extra panes.
  With `*save-on-change*`, every layout change asks the core for a save,
  coalesced as always.
- `extensions/idle-lock/` — idle timers on the new `:user-activity` hook:
  `(seconds . command)` steps fire once per quiet period (dim, lock, DPMS
  off), resume commands undo them when presence returns, and a locked
  session stands down. `lock-now` on demand.
- `src/policy/hooks.lisp` — the `:user-activity` hook: fired on every bound
  key, every key that reaches the window manager at all, and every pointer
  move; no arguments, cheap by contract.
- `extensions/projects/` — workspaces bound to directories: define a project,
  switch to it, and everything spawned on its workspace starts in its
  directory. The binding is the workspace label, so nothing owns a workspace
  and rearranging cannot go stale. Backed by one new runtime seam:
  `r:*spawn-directory*`, consulted by the single `spawn` command every spawn
  path funnels through.
- `extensions/transient-rules/` — one-shot window rules: arm
  "the next window matching X" with `add-rule` or the `float-next` /
  `workspace-next` commands; the entry is consumed by the placement it
  causes and never fires twice. The consultation is a primary method on
  `window-rule-for`, because a second `:around` on identical specializers
  replaces rather than composes — see the module README.
- `extensions/buffers/` — named windows and pane-as-view switching:
  `name-buffer`, `switch-to-buffer` with completion, and `buffers`. Focus
  after a recall is configurable (`*focus-follows-recall*`, default follow);
  whether a switch records an undo step is configurable
  (`*undo-includes-swaps*`, default no); a buffer already on screen is jumped
  to rather than cloned, because a live Wayland window has one rectangle.
- `extensions/master-stack/`, `extensions/scrolling-columns/`,
  `extensions/window-rules/` — the remaining promoted examples, each with its
  own tests in the build. master-stack composes as a policy mixin over
  whatever is installed instead of replacing it; scrolling-columns answers
  the persistence half of the container protocol for its scroll state;
  window-rules matches predicates and honours `:workspace`.
- `extensions/` — promoted extension modules beside the lattice, starting with
  `focus-follows-mouse`: focus follows the pointer except over a floating
  window, and keyboard motion warps the pointer along. Loaded with
  `(load-extension "focus-follows-mouse")` and enabled live with
  `(focus-follows-mouse:enable)`. Gate 1 compiles every directory under
  `extensions/`, `make test` runs their suites through a registry the suites
  join at load time, and the runtime searches an `extensions/` subdirectory of
  the build tree and of `~/.config/latticewm/`.

### Fixed

- The option-surface machinery read SBCL method references assuming every
  method was primary; the first `:around` method on an option-reading generic
  (the new module's) crashed five surface tests with `(length :around)`.
  Qualifiers are now parsed as part of the reference and printed with it.

## 0.2.0

The release the grade said was three edits away, plus the three edits.

Everything below was already here; what changed is that the things this project
says about itself are now attached to something that runs. The declared minimum
compiler has been run rather than inferred, the generated documents are
byte-identical on every compiler the project supports, the integration suite
drives both ends of the supported river range instead of one, and the first
sentence a new user reads is true.

### Added

- **`doc/ONBOARDING.org`**, the contributor guide for the second week:
  the map of the tree, the five instruments and what each one structurally
  cannot see, and what each kind of change — an option, a command, a generic, a
  hook, a container kind, a protocol call — obliges you to do. `CONTRIBUTING.md`
  stays the short version. It is a *current* document, so its sentences are
  checked, and it is on gate 19's list so the gate count in it cannot rot.
- **A "shipping it as a module" section in `doc/EXTENDING.org`.** The guide
  covered tiers 0 through 3 and stopped at the edge of the user's own
  configuration file; this is the step after, for somebody packaging an
  extension other people install. The `.asd`, why to depend on `latticewm` and
  nothing narrower, the enable/disable contract, not taking a key without
  saying so, offering an option rather than a constant, testing, and what to do
  about versions.
- **The integration suite drives both ends of the supported river range.**
  Against 0.4.5 (interface version 4, the floor) and 0.4.6 (version 5, which
  the vendored XML was generated from). The two runs assert different things:
  the floor run says every request this program sends is one an older river
  understands, and the ceiling run says every event it binds for is one it can
  actually decode. Only the first was ever in doubt out loud; the second is the
  one that fails silently. `src/protocol/PINNED` said both `capture_sessions`
  handlers were "covered by the unit suite and by nothing live", which was
  honest and is no longer true.
- **Motion crosses the screen boundary.** Walking off the edge of one monitor
  arrives on the next one, at the place you last stood there. Motion in this
  program is continuous across every other boundary — out of a split when the
  direction crosses its axis, into the lattice cell next door through the edge
  you left by — and the screen was the last one it was not crossing, which made
  two monitors feel like two window managers. It costs no key, and with one
  monitor there is nothing in any direction so nothing changes.
  `*motion-crosses-outputs*` turns it off.
- **`focus-output` and `send-to-output`.** Eighty commands and not one of them
  named an output: per-output workspaces have worked for a while — each screen
  shows its own, they trade rather than collide, the arrangement survives an
  undock — and the only way to reach the other one was `workspace N`, which
  puts N on the screen you are already looking at. Both new verbs are
  compositions rather than algorithms, because the cursor is one place in one
  model: focusing another screen is a jump into the workspace it displays, and
  sending is `send-to-workspace` with the number looked up instead of typed.
  `Ctrl+Shift+Super+`(direction) sends; focusing is deliberately unbound,
  because the arrow keys already do it.
- **Per-workspace focus memory** (`*remember-place*`). Switching to a workspace
  put the cursor at its first leaf, always. On one monitor that is a small
  annoyance you stop noticing; with two it is what made crossing between
  screens not worth doing. Recorded on the workspace rather than the output —
  a workspace moved to the other screen takes its place with it — and a
  remembered path that no longer leads anywhere goes through `repair-path`
  like every other stale path here.
- **Timers** (`add-timer`, `remove-timer`). Nothing in this program happened
  because time passed: every redraw was caused by a key, a window, a monitor or
  a client. `wait-for-work` has always taken a timeout and had always been
  handed the same constant; a timer is that number becoming a question. Timers
  are registered by name, so loading a configuration file twice leaves one
  clock rather than two — the mistake `add-hook`'s docstring records this
  project losing an afternoon to.
- **`wl_surface.frame`.** There was no frame callback anywhere in `src/`, so
  drawing was opportunistic: a drawer decided it had something new and
  committed, whether or not the surface was on a screen anybody could see.
  `draw-when-ready` paces a drawer to the compositor and keeps only the newest
  redraw, and the empty-pane outlines — which redraw on every relayout,
  including the ones a pointer drag produces — now use it. The wait is bounded
  by `+frame-patience+`, because a surface that is occluded is never told to
  draw again and a redraw deferred behind that callback would wait forever.

### Changed

- **The river version check is a floor and a ceiling instead of an equality,
  so a newer river works.** LatticeWM refused to start against any river whose
  `river_window_manager_v1` was not the exact version its vendored XML
  declared, in either direction. Since river changes that interface inside a
  patch release, and since distributions upgrade river without asking, that
  meant a login screen that refused over a protocol change which was purely
  additive. It now refuses only *below* its floor — the oldest version carrying
  every request it sends — and binds at `min(offered, ceiling)` above it.
  Wayland obliges the compositor to speak the version a client bound, so a
  river released after this build answers in the dialect this build was
  compiled for, losing the features added since and nothing else. In practice
  one build now serves river 0.4.5 through anything newer;
  `latticewm --version` prints the range.

  The equality was written against a real hazard — a pre-release protocol can
  change what a request *means* without bumping its number — but it never
  addressed that hazard, because a river that does so still advertises the same
  number and still passes an equality. What it caught was the announced, polite
  case, which is the one Wayland already makes safe, and it caught it by
  refusing to start.

- **Every protocol global is now bound at a version this build can decode.**
  `river_layer_shell_v1` and the three input protocols bound at whatever river
  advertised, which is a client promising to understand events its generated
  bindings have never seen; the broken promise arrived as `dispatch-one-event`
  logging "undecodable event ignored" at `:debug`. All six protocols now have a
  ceiling constant, all six are clamped to it, and gate 5 checks all six
  against the vendored XML rather than two. The one function that decides any
  of this is out of `bind-one-global` and under test — it needed a live
  `wl_registry` to reach, so nothing had ever asked it a question.

### Fixed

- **"Any key closes this" was false, and it is the first sentence a new user
  reads.** The welcome overlay, the keymap overlay, an apropos listing and the
  undo history all print it; `*help-visible*`'s own docstring calls the rule
  "any key puts it away". The branch implementing it lived in `handle-key`,
  which river only ever reaches for a key that is *bound* — a compositor
  delivers to the focused window everything the window manager did not ask for
  — so the rule meant "any Super chord closes this". On a genuine first run,
  Escape, space, Return, `q` and `x` each left the overlay exactly where it
  was, in the sixty seconds that decide whether somebody keeps the program.

  The dismissal was not merely unwritten, it was *unreachable*: with an overlay
  up and nothing else pending, `capture-armed-now-p` answered NIL, so the
  capture bindings were disabled and river was never going to hand over the
  keystroke at all. An overlay now arms them, `handle-captured-key` takes the
  overlay down without consuming the key — so the keystroke still does its job,
  which is the lesson "the first super return did not open term" already
  bought once — and the shipped `capture-keys` gained the function keys, Page
  Up, Page Down, Insert and Menu, so the sentence is true of every key a person
  presses rather than of the ones that happened to be readable. Bare modifiers
  are deliberately still excluded. Asserted in the unit suite, and against a
  real compositor in `make integration`, which is the only place that can ask
  whether river was actually told to enable the bindings.

- **The declared minimum SBCL was not a version the project had ever been run
  on.** `latticewm.asd` said 2.2.6 and its docstring said "the oldest SBCL this
  project is known to work on". The `plain` CI job was built for the express
  purpose of asking whether that was true, on Ubuntu's 2.2.9, under a comment
  calling itself "this project's only empirical answer" — and it had answered
  *no* on thirty consecutive runs, which nobody read.

  The defect was in a test helper, not in the program: `with-captured-log` bound
  its variable only *after* running its body, and every call site ended its body
  with that variable. SBCL 2.6 deletes the reference because its value is
  discarded; 2.2.9 does not, so four tests in the suite covering `guarded`,
  `best-effort` and `with-abandon` — the error-handling substrate the whole
  program rests on — died on an unbound variable without reaching an assertion.
  The variable is a symbol macro now and reads the same inside the body and
  after it. The floor turns out to be true: the whole build is green on 2.2.6,
  2.2.9 and 2.6.6.

  A new `floor` CI job runs the declared minimum rather than something above
  it, reading the number out of `latticewm.asd` with the same `sed`
  `bootstrap.sh` uses, so there is no second copy to go stale. A claim about a
  version nothing runs is the shape this whole workflow exists to abolish.

- **The generated documents were not reproducible across supported compilers,
  so the `surface` gate could not go green on its own runner.** `make surface`
  produced a byte-identical tree on 2.6.x and a different `EXTENSION-SURFACE.txt`
  on 2.2.9. The difference was anonymous functions in the `read by:`
  cross-reference lists — entries like `(lambda (condition17) in
  emit-window-management-state)`, where `condition17` is a *gensym counter*:
  a fact about how many times `gensym` had been called during compilation, not
  a fact about the program. Newer SBCL records those inner lambdas and 2.2.9
  does not record them at all.

  `option-readers` now folds an anonymous reader to the named function
  containing it, and drops it when there is none, so the list contains only
  things a person can go and look at. The six documents are byte-identical on
  2.2.6, 2.2.9 and 2.6.6. This is the third instance of the same class the
  changelog already records twice — a generated document whose content depends
  on the machine that generated it — and the first one where the dependency was
  on the *compiler* rather than on the filesystem.

- **`install-check` failed on CI because its own stated precondition was false
  there, and the run wrote into the home directory it was written to protect.**
  Every scratch prefix came from `mktemp -d "${TMPDIR:-/tmp}/..."` under a
  comment asserting the result is not inside `$HOME`. GitHub Actions sets
  `TMPDIR=/home/runner/work/_temp`, so it was, and `install.sh` reads exactly
  that to choose between a system install and a home install — so the check
  took the home branch, wrote a `wayland-sessions` entry outside the scratch
  prefix and into the real home, and then correctly reported that the session
  entry was missing from the prefix. The prefixes are rooted at `/tmp`, and the
  precondition is asserted rather than assumed: a check whose premise is false
  on the only machine that runs it is not a failing check, it is a check that is
  not running.

- **`make image` failed with eighteen frames of SBCL backtrace on an SBCL that
  cannot compress a core, and the declared floor did not know that was
  possible.** `latticewm.asd` reasons the 2.2.6 floor entirely from the release
  at which core compression became zstd and its levels became 0..22 — correct,
  and silently assuming an SBCL of that version can compress at all. It is a
  *build-time option* of SBCL, so the version is necessary and was never
  sufficient. Found by running the new `floor` CI job locally before trusting
  it: the official x86-64 binary tarballs for 2.2.6 and 2.2.9 have no
  compression support, which is unlucky in the exact place it matters, because
  they are what a person installs by hand when they want an old SBCL to check
  this floor with. Distribution packages generally do have it.

  What arrived was an unhandled `SIMPLE-ERROR` — *"Unable to save compressed
  core: this runtime was not built with zstd support"* — at the end of a build
  that had otherwise passed every gate and every check, naming neither this
  project, nor the variable that turns compression off, nor the fact that
  everything except the dump had succeeded. `tools/image.lisp` asks before it
  dumps now and says all of that in a sentence. It refuses rather than falling
  back to an uncompressed image, because `make install` installs what `make
  image` produced and `install-check` has a size ceiling for precisely that
  accident. Nothing about the floor changes: the program builds and passes on
  an SBCL with no compression at all.

- **`install-check`'s size ceiling had stopped catching the one bug it exists
  for.** It refuses an installed binary over 60 MB, on the reasoning that the
  compressed image is 13 MB and the uncompressed one is 190 — a fourteen-fold
  gap with the ceiling in the middle. Measured today, on the same command, they
  are 12 MiB and 52 MiB: the compressed image barely moved and the uncompressed
  one lost two thirds of its weight. So an uncompressed image had quietly slid
  *under* the ceiling, and the check written for exactly one bug — "somebody
  installed the output of `make image-fast`" — would have passed it silently.
  The ceiling is 30 MB now, which restores the property rather than the number:
  two and a half times the compressed image, and well under the uncompressed
  one. Found by measuring rather than by reading, which is the only way this
  class of staleness is ever found.

- **This file's own header described a check nobody had written.** It said
  "gate 19 holds the man pages and this file to the same number". It held the
  man pages; nothing in `tools/gates.lisp` had ever opened `CHANGELOG.md`. That
  is gate 12's disease — a sentence about the program that nothing can see —
  occurring inside gate 19's documentation. Gate 19 now checks that the version
  in `VERSION` has a heading of its own here, which fires exactly when a version
  has been bumped without being written down.

- **Gate 1 said "zero compiler warnings" and did not compile `examples/`**, so
  the build printed `The variable COLUMNS is defined but never used` from
  `examples/05-status-line.lisp` on every run, underneath a gate saying there
  were none. The parameter is declared ignored, and gate 1 now compiles the
  worked examples — globbed, so one added tomorrow is covered on the day it
  lands. This is the fourth file the "compiled by nothing" hole has been found
  in, and the one with the largest audience: `examples/` is what
  `doc/EXTENDING.org` sends a stranger to read first.

- **The shipped status-line example's clock showed the time of the last layout
  change.** `clock-segment`'s docstring said the time was "asked fresh every
  frame", which was true, and there were no frames. Sit still for twenty
  minutes and it read twenty minutes ago — worse than no clock, because a wrong
  clock is believed. It ticks now, and turning the segments off takes the
  wakeup off with them.
- `with-example` restored methods and option values and not timers, so the
  clock above would have outlived the test that loaded it and fired on a world
  that had been thrown away. The list of globals an example can touch is the
  list of things that macro has to put back, and it is three long now.
- `output-showing-workspace` was answered in the runtime and motion needed the
  same question answered in policy, which may not call the runtime. It moved to
  `p:output-showing` rather than being copied.
- `CONTRIBUTING.md` still said `examples/` was load-bearing for gate 6. It was,
  and stopped being when the floors moved to `lattice/` alone.
- **`make integration` failed against a river inside the range it now
  supports.** Three checks asserted that river had reported a
  `capture_sessions` count, which is a `since="5"` event; against river 0.4.5,
  which offers interface version 4, the suite bound at 4, was correctly never
  sent the event, and reported the protocol working as three failures. The
  suite had outlived the equality it was written under — it still assumed the
  running river's version was the vendored one. Not having been told and having
  been told zero are different states, the model already keeps them apart, and
  the harness now reads which one it is: a version gap is reported as its own
  category, never fatal, naming both numbers, and separately from the things a
  headless backend cannot have. The half of the section that holds at any
  version — that nothing claims to be recording — still runs.
- **`make integration` started Xwayland on every run, having tried not to.** It
  set `XWAYLAND=0` in river's environment, a variable neither river nor wlroots
  reads, with a comment explaining that nothing there needs Xwayland. True, and
  it started anyway, costing the second the comment said it saved. It becomes
  fatal wherever `/tmp/.X11-unix` is not a real directory — WSL makes it a
  symlink, and wlroots refuses one — where river exits before creating its
  Wayland socket and the suite reports that river would not start. river's own
  `-no-xwayland` flag does what was meant.
- **One integration check raced the redraw it was checking.** The echo area's
  double-buffer assertion read the committed canvas the instant a roundtrip
  returned, which assumes the notification was painted inside it; the redraw
  can land in the next render sequence. A run that lost the race claimed the
  echo area had overwritten the buffer river was reading, which is an alarming
  thing to be told and was not true. It polls now, like the buffer-release
  check beneath it that was already written that way.
- **A fresh clone printed a backtrace instead of "run ./bootstrap.sh".**
  `tools/prelude.lisp` checks the SBCL floor by loading `latticewm.asd` before
  it checks for missing dependencies, and that file declares
  `:defsystem-depends-on ("wayflan-client")` — so on any machine without the
  dependencies, which is every fresh clone and the entire situation
  `missing-systems` exists for, the load signalled `MISSING-DEPENDENCY` and the
  useful message forty lines below was unreachable. The comment beside it had
  reasoned about a *missing* `.asd` and not about a present one that cannot be
  read. Found by running the build on a second distribution with no Quicklisp
  in it — the same way the Fedora bootstrap failure was found, and for the same
  reason: the author's machine has had the dependencies since before that file
  existed.
- `tools/integration.lisp` now asks river whether it answers a frame callback,
  beside the check that it releases a buffer. Both are inbound obligations,
  both were invisible to gate 8 because it faces outward from our code toward
  the protocol, and neither is answerable by a suite with no compositor.

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

- **Gate 6's floors were defending the tutorial's line count.** They were set at
  exactly the number the tree produces — 15 generics and 22 methods, the union
  of `lattice/` and `examples/`, with zero slack on either — so deleting any one
  worked example failed the build. An example you cannot delete is not an
  example. The floors are now sourced from `lattice/` alone (12 and 13); gate 6
  still *loads* `examples/`, which is the check that has actually caught
  something. And the number is printed twice, because 15 is measuring n = 1: the
  lattice supplies 12 of those 15 generics, and everything else outside `src/`
  supplies 7. The second figure is the uncomfortable one and is the one to argue
  with.
- **Gate 3's line budget counted comments.** `*lattice-line-budget*` was 2,600
  *raw* lines against a tree of 2,142, of which 293 were blank and 265 were
  comment — so a quarter of what it measured was how much the author explains
  himself, and the cheapest way to buy headroom was to delete an explanation.
  That is old gate 6's disease, sitting four lines under a comment congratulating
  the file for having cured it. It counts code lines now, through the same
  `code-of` that already makes the rest of gate 3 immune to being told it passes.

- **Four gates whose failing half was unreachable.**
  - Gate 15 decided whether a total override composes by looking for the
    *symbol* `call-next-method` anywhere in the method's form — no package
    check, no evaluation-position check. `'(call-next-method)` quoted satisfied
    it. It now requires a written call: operator position or after `#'`, and
    quoted subtrees are not walked. What it still cannot see —
    `(when nil (call-next-method))` — is said out loud rather than left to be
    found.
  - Gate 16's document search treats a space as a name boundary, and
    `src/package.lisp` exports thirty-two ordinary English words. Any sentence
    containing "window" kept the export `window` alive, so roughly thirty
    published names could not be reported dead however dead they were. `.org`
    files are now searched for names that are marked up as names — `~name~`,
    `=name=`, a source block, a `#+` line — which is the distinction org
    already draws and this tree already writes.
  - Gate 17's remedy is a way out of gate 17: its population is options whose
    bare name matches a generic's, and the failure message ends "rename one of
    them". Rename the option and the gate enumerates nothing and passes, with
    the user in the state the preamble calls the failure. The population has a
    floor now.
  - Gate 18 is the same shape: its population is files literally named
    `defaults-*.lisp`, there is one left, and deleting the prefix from it would
    make the gate govern the empty set forever. Also floored.
- `examples/01-focus-follows-mouse.lisp` read `:rect-index` as though it were
  part of the world. It is an artifact `emit` writes after a layout, so before
  the first frame there is no table and `(gethash node nil)` is a type error,
  not a miss — and the method is installed on `conventional-policy`, so every
  focus change before the first frame died, in extensions that had never heard
  of the example. `motion.lisp` and `server.lisp` both already asked for the
  index as a thing that might not be there; this was the one place that
  assumed.
- `PLAN.org` §generics disagreed with itself in three numbers and with the
  program in a fourth: the prose said sixty-six, the quoted ceiling said 65,
  the next-one-to-argue-about said sixty-seven, and the `#+CLAIM:` said 66
  while the surface was at 69. Gate 12 caught the claim, which is what it is
  for; the other three were sitting in the same paragraph.
- Gate 12 read one kind of prose out of the three it claimed jurisdiction over,
  and evaluated claims into an image seven later gates go on to question.
  - **The man pages were on the current list and were being read by nothing.**
    `marked-tokens` wants a *pair* of `~` or `=` on a line, which roff never
    writes, and `earmuffed-p` refused the `\-` roff spells every hyphen as. So
    `doc/latticewm-config.5` named forty earmuffed options — `*smart-gaps*`
    twice — and the check that exists because of `*smart-gaps*` validated none
    of them. Roff has no asterisk markup, so those lines are now read whole,
    escapes undone, as a source block is. 42 names checked.
  - **Docstrings are read now**, which is where most of this project's
    load-bearing prose actually is: gate 2 asks whether a docstring exists and
    nothing asked what it said. The discriminator is case — code is written in
    capitals here and emphasis in lowercase — which is org's `~...~`
    distinction spelled in a convention docstrings already follow. 86 names
    across 1420 docstrings; it found three shouted emphases, which were
    rewritten rather than exempted.
  - **A claim can change the image, and gates 14, 15 and 17 all read the image
    afterwards.** The paragraph in gate 12 asserting that it ran after every
    gate that asks the image a question was describing an intention. The image
    is now fingerprinted before and after each claim — every option and its
    value, every hook and its attachments, the command count, the method count
    of every generic on both surfaces — and a claim that moved it fails naming
    itself.
  - `*generated-documents*` was a `defparameter` with a five-line docstring and
    exactly one reader in the tree: its own definition. It also named five
    files where `make surface` writes six, so `doc/HOOKS.txt` was generated,
    installed, advertised in README and listed nowhere. `doc/*.txt` is
    enumerated against it now, with a second list for the one file in there
    that is somebody else's licence text.
- The mixin recipe in `doc/EXTENDING.org` set a `*previous*` that the program
  does not have and the example never defined, and its `disable` half — the
  whole reason for saving the class — was described but not shown. The saved
  class now rides on the mixin, which is the argument for writing a mixin.

- `src/policy/protocol.lisp` opened by citing a build gate that did not exist:
  "it contains no methods, and that rule is enforced by a build gate."
  `grep -rn protocol.lisp tools/ tests/ Makefile .github/` returned nothing —
  `*SMART-GAPS*` in the one file the project hands to strangers, and invisible
  to gate 12 because a file header is neither prose nor a docstring. The gate
  is written (gate 20) and the header now names what is in the file instead of
  under-describing it.
- `flake.nix`'s `buildPhase` was five hand-typed `sbcl --load` lines under a
  comment reading "the same steps `make check` runs", three lines above the
  `installPhase` that gate 9 exists to keep delegating. Six references to
  `installPhase` in `gates.lisp`; zero to `buildPhase`. It runs `make check
  image` now, and gate 21 holds it there on gate 9's terms.
- `tools/gates.lisp` was twenty-one bare top-level forms with no error
  containment between them, so a gate that signalled rather than failing took
  every gate behind it with it. `tools/integration.lisp` wrote that lesson
  down, and gates 13 and 16 wrap their own file reads in the same words; the
  reasoning had reached two gates and not the file. `tools/run-gates.lisp` puts
  a handler between the forms, and a contained error is a failure and not a
  skip.
- Gate 19 read `"twenty"` inside `"twenty-one"` and reported eight documents as
  stale for saying the true number.
- `tests/test-examples.lisp`'s `with-example` promised to clean up after an
  example and did not: no `remove-method`, no option restore, no
  `unwind-protect`. The examples define methods on the shipped policy class,
  and the examples suite runs before the lattice suite in the same image, so
  the whole plane suite ran with `*focus-follows-mouse*` on and example 02's
  `on-window-open` installed. It was correct by string coincidence.
- `adding-a-slot-to-a-live-class-migrates-existing-instances` redefined the
  shipped `c:leaf` from a `defclass` hand-copied into the test file and
  restored it from the same copy, silently dropping the class and slot
  docstrings for the rest of the image on every run. The property is CLOS's,
  not `leaf`'s; it is asserted on a class the test file owns.
- `motion-is-involutive` asserted a general guarantee at the one arrangement
  where it holds — a split with no weights — and `defaults-motion.lisp` stated
  it as general prose. Entry across an axis aligns on the centre of the rect
  you left, which cannot be involutive when the two sides are divided
  differently. Both halves are asserted now, the counterexample included.
- 21 tests were lint rules whose loops made checks that could not fail: an `is`
  per option, per hook, per generic, over universes defined as the fixed point
  of the predicate being tested. Collapsed to one assertion over the collected
  failures, the way `every-symbol-the-core-exports-names-something` already did
  it. The suite went from 1904 checks to 1294 with no loss of coverage, which
  is the honest number.
- `tools/bench.lisp` was compiled by nothing — the third instance of that bug,
  found while the commit fixing the second was still the tip. It is in
  `*loose-files*` now. `tools/test-lattice.lisp` was dead and is deleted.
- `swank.lisp` said "nothing is lost by having SWANK off", which is airtight
  about scripting and wrong about the audience the file names two paragraphs
  earlier. The starter configuration told every new user to `M-x slime-connect`
  to a port nothing is listening on; it now names the three ways in that work
  and the one line that turns on the fourth.
- 17 org links carrying an Emacs `::#anchor` search target, in the current documents, were
  search-target syntax, which GitHub resolves as a path and 404s — the front
  door, broken for everybody who is not reading it in Emacs. The 67 in the
  frozen records are left alone.
- `ASSESSMENT.org` told a visitor the program is "not ready to be somebody's
  daily driver" against `README`'s "working, and used on bare metal". Both were
  true when written; the record is frozen, so it now says at the top what it is
  and where the current status lives.

- **Thirteen protocol requests were called without going through the wire
  layer**, and every one of them was a teardown — three in `layer.lisp`, two in
  `outputs.lisp`, five in `seats.lisp`, three in `surface.lisp`. `wrappers.lisp`
  generates a checked wrapper for all 123 requests and gate 5 counts them, so
  the discipline looked total and was applied to the requests somebody
  remembered to route. The circle closed inside one file: `wm-manage-finish`'s
  docstring reads "Prefer `WITH-MANAGE-SEQUENCE`", and `with-manage-sequence`
  sent `manage_finish` raw. Eleven aliases added, every call site moved, and
  gate 22 now refuses a request named outside `src/wire/` — while still
  allowing the protocol package to name an *interface*, which is how a global
  is bound.
- **`policy/appearance.lisp` was three libraries in one file** because an early
  gate 6 measured a line-count ratio and the file was shaped to keep it honest.
  That gate was replaced three commits later and the shape outlived it. Split
  into `policy/font.lisp` (fonts as data, and the metrics every widget is made
  of), `policy/text.lisp` (pure functions of a string) and what is actually
  appearance policy.
- **The keymap was in `runtime/config.lisp`**, beside the code that finds
  `$XDG_CONFIG_HOME` — ninety-seven lines of `define-key`, the most customised
  object in any window manager, in the file about configuration *files*. The
  five values a person changes first had been moved into `policy/` on exactly
  that argument. It is `policy/keymap.lisp` now.
- **The runtime module was `:serial t` over twenty-nine files**, so editing one
  recompiled most of what followed, and two of the ordering comments disclaimed
  being dependencies in as many words — a narrative encoded as a build
  constraint. Replaced with the reference graph: 117 edges where a total order
  has 406. The load order is unchanged, so a clean build is byte-identical.
- **`src/runtime/font.lisp` held four of the program's functions inside a
  Python string literal** in `tools/psf-to-lisp.py`, a script run by hand when
  somebody changes fonts. The generated table is `runtime/font-table.lisp` now
  and the functions are ordinary source.
- **The re-vendor recipe was the install path for most users and nothing tested
  it.** The pin is exact and the program refuses rather than misbehaves, so on
  most distributions the packaged river will not match and `INSTALL.org`'s four
  steps are what people actually do. `make revendor-check` asserts that a river
  we cannot speak to is refused by name with both version numbers, needing no
  second river; `make revendor-check RIVER_SRC=…` runs the recipe for real. CI
  runs the first.
- **The repository root was seven `.org` files and half a megabyte**, and
  `ASSESSMENT.org` — reachable by no documented path — told a visitor the
  program was not usable. Everything but `README` and `INSTALL` is under
  `doc/` now; nothing is deleted, every one of the 140 org links still
  resolves, and the frozen documents are still frozen. `install.sh` flattens
  them into one `doc/` at the destination — an installed copy has no reason to
  reproduce the repository's layout, and `make install-check` is what caught
  the first attempt putting `FINDINGS.org` in `doc/doc/`.

- **Undo went back to the beginning of time through three of the four doors.**
  `*undo-coalesce-seconds*` merges consecutive steps that share a label, so
  that holding the resize key is one step back rather than forty — and it
  decides by comparing labels. That is right for `resize left`, which repeats
  because the gesture repeats. It is wrong for `from a REPL`, which *every*
  change through SWANK, the control socket and `Super+;` carries: an entire
  session of unrelated changes coalesced into one entry holding the oldest
  tree, and undo jumped all the way back to it. A step is coalescible now only
  when its label came from a command, which is what defaulting the new
  parameter to `(eq label *undo-label*)` says — no call site changed, and a
  door that invents a new constant is covered by construction rather than by
  being added to a list. Found the first time `make integration` was ever run
  on this machine; 1302 unit checks could not see it, because it needs four
  doors, a real compositor and a clock.
- **Two integration checks raced an asynchronous removal and failed about half
  the time.** `window-live-p` goes false the moment river says `closed`; taking
  the leaf out of the tree and running `:window-closed` happen just after, on
  our side. Asserted flat, they were a coin toss — which is worse than always
  failing, because a check that is usually green is one nobody believes when it
  goes red. They poll now, which is what the file already does one screen up.
- **`bootstrap.sh` failed on a stock Fedora and pointed at a backtrace.** One
  of the Lisp dependencies grovels a C header, Fedora's gcc is configured to
  read RPM's hardening specs, and those live in `redhat-rpm-config`, which a
  stock install does not have. The failure was `gcc: fatal error: cannot read
  spec file '/usr/lib/rpm/redhat/redhat-hardened-cc1'`, forty frames of SBCL
  backtrace above the end of `.deps/bootstrap.log`, and all the script said was
  "see .deps/bootstrap.log". It now pulls the first line that looks like a
  cause out of the log, says what it means where it knows, and names the
  package through the per-distro table it already had. Found by running the
  non-nix path on Fedora 44 rather than by predicting it.
- `INSTALL.org` said a C compiler was not needed and named no river build
  constraints beyond the tag. River pins Zig and wlroots hard — `v0.4.6` wants
  Zig **0.16** and wlroots **0.20**, and the failures name neither: a 0.14 Zig
  dies at `build.zig:10` with "'@import' of ZON must have a known result type"
  and a 0.19 wlroots with "unable to find dynamic system library
  'wlroots-0.20'". Ubuntu 26.04 packages neither version; Fedora 44 packages
  both. Both checks are one line and both are written down now.
- **`README.org` and `INSTALL.org` both advertised "1600+ checks"** in the first
  code block a reader meets. True when the suite was 1,904; the collapse of the
  21 lint tests took it to 1,302 and the two headline numbers were left behind.
  Both say 1300+ now. The deflation was deliberate and the headline was not
  updated with it, which is the same shape as every other number this project
  has had in two places.
- **`doc/latticewm-config.5` rendered its own thesis statement with literal
  asterisks** — `*This page is a map, not a reference.*`, org bold syntax in a
  roff file, where every other emphasis on the page is a `.B` line. Gate 12
  reads the man pages now, but it reads them for *names*, and org markup that
  roff prints verbatim is not a name.
- **The working vocabulary was still in no document.** The six generated
  surfaces answer "where do I hang a method" exhaustively and cannot answer
  "what do I write in the body", because the answer is ordinary functions and a
  document generated from a registry holds what a registry holds. `divide-rect`
  is exported, is the load-bearing call in both tier-3 examples, and appeared in
  no file under `doc/`. "The calls, once" in `doc/EXTENDING.org` is the
  eighteen names — geometry, paths, the world, the pointer, the hooks — with
  what each is for and which of them must not be reimplemented.

### Added

- Gate 20 — no methods in `src/policy/protocol.lisp`, which its own header had
  claimed a gate enforced.
- Gate 21 — `flake.nix`'s `buildPhase` delegates to the Makefile, on gate 9's
  terms.
- Gate 22 — every protocol request goes through `src/wire/`.
- `examples/05-status-line.lisp`, the thing people ask for first: the time, the
  load average, and how many windows are in a workspace you are not looking at.
  Twenty lines, no new class, no second process, and `call-next-method` so that
  the shipped line survives and a second extension doing the same thing appears
  as well as yours. The third segment is the argument for the whole approach —
  it is a fact about the layout, which nothing outside the window manager can
  produce.
- A vocabulary section in `doc/EXTENDING.org`. Nine words the whole corpus uses
  as though you already had them, defined once.
- `doc/EXTENSION-SURFACE.txt` is grouped by protocol. Every paragraph of its
  preamble explains that the shipped defaults sit on six protocol classes, and
  then the list gave no sign of which generic belonged to which.
- `CONTRIBUTING.md`, an issue template, an extension-point issue template and a
  pull request template. Twenty-one gates are a superb instrument for one
  author and a wall to the second, and `.github/` held `workflows` and nothing
  else — no warning anywhere about what a first branch has to survive.
  `CONTRIBUTING.md` is on gate 19's document list from the day it was written,
  because the number it tells a newcomer is how many ways their branch can
  fail.
- Dist metadata in both `.asd` files: `:long-description`, `:homepage`,
  `:source-control`, `:bug-tracker`, `:mailto`. Quicklisp, ocicl and qlot read
  a system definition and none of them read a README.

- Gate 19 — the project says the same thing about itself everywhere it says
  one: licence, version and the number of gates that run.
- `make dist`, and this file.

## 0.1.0

The first version. A window manager for the river Wayland compositor, written
in Common Lisp, extensible from a live image over a Unix socket. `README.org`
is the tour and `doc/EXTENDING.org` is the guide; `PLAN.org` and `FINDINGS.org`
are the record of how it got here and are append-only on purpose.
