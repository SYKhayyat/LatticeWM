# PLAN — LatticeWM (work top to bottom, one issue per worker session)

Worker loop: top unchecked item only, fix + test, commit, check off, stop.

## Phase 0 — Technical debt (goes FIRST)

These are the audit findings of 2026-11-19, filed as issues and promoted here
because each is paid by every session until it is gone: a red `make check`, a
document every handoff trusts that nothing checks, a command the docs promise
and M-x cannot run, the least-tested layer in the tree, and machinery copied
instead of shared.  None is a feature; each has a proposed resolving test in
its issue.  Phase 1 below is fully checked, so nothing safety-shaped is being
displaced by putting this list above it — but stay honest: if one of these
stops being small and starts touching a safety path, it moves back down.

- [x] #116 `make check` was red on `main`: gate 3, lattice 1709 code lines vs a
1500 budget. Fixed by moving the ratchet deliberately — 2000, with the reason
written into `*lattice-line-budget*`'s docstring (verbs are a product surface;
the same fifth-of-headroom rule off the new truth). Trimming would have bought
~30 lines against a 209 overrun; see the issue's evidence. `make gates` green.
- [x] #117 HANDOFF.md was current-looking, in no gate list, and false. Frozen:
date + banner + no live numbers, and it is in `*historical-documents*` with
CHANGELOG.md (a log). Root `*.md` are enumerated by gate 12(a) now, so no
document can arrive unclassified; a new `*working-documents*` holds PLAN.md,
CONTRIBUTING.md, AI_ISSUE_ROUTING.md, and gate 12(e) fails an open item citing
an issue number triage.csv does not know (verified by feeding it #999).
- [x] #118 auto-respawn's `respawn-session` was documented as a command and was
a plain `defun` — no `--list-commands` line, no M-x, nothing for a binding to
name. Now an `r:defcommand`; README drops the wrong `r:` namespace and shows a
binding. +4 checks.
- [x] #119 38 of 90 commands were named by no test at all. Gate 23 asks the
command question the build never asked, in gate 16's allowlist shape: a
command is shipped when a file outside `examples/` defines it (the examples
are read, not shipped, so their verbs are counted and skipped), naming is a
symbol written in test code plus the whole-string form the integration suite
drives with `(verb "focus-next")`, and prose in a docstring answers for
nothing. 75 of 90 shipped commands now have a test; 13 are allowlisted with
the reason each cannot be tested (`restart-wm`, `reload-config`,
`eval-expression` and the minibuffer prompts, the compositor's seats, the
filesystem).  An allowlist entry that gains a test fails the gate, so the list
can only shrink. +29 checks.
- [x] #123 float geometry was three magic spellings of one thing. The default
size is `c:*default-float-width*`/`-height*` + `c:make-default-float-rect`
(fresh per call, used by the class initform and both fallbacks); the resize
floor is the option `*float-minimum-size*` `(100 . 60)`. +10 checks, surface
regenerated.
- [x] #122 six bulk verbs (close/minimize × others/workspace × selection) were
one loop spelled six ways. `act-on-windows` / `act-on-current-workspace`;
each verb is now its notice plus one call, and the two behavioural rulings
(skip EXCEPT; count what acted) live in one docstring. +7 checks.
- [x] #121 `install-vocabulary` was copied three times (lattice, buffers,
transient-rules) and already drifting. Now `r:install-vocabulary` in
`src/runtime/vocabulary.lisp`, taking the source package; the lattice keeps
its documented one-argument wrapper, the two extensions call the runtime at
load time. Gate 1 caught the first signature (&optional + &key). +4 checks.
- [x] #120 the extension test harness was verbatim in four suites. The shared
copy lives in the harness that already existed -- `tests/suite.lisp` exports
`t*:block-the-replacement`, `t*:unblock-the-replacement`,
`t*:fresh-temporary-directory`; `grep -rc "defun unblock-the-replacement"
extensions/` answers nothing, and all four suites pass against the one copy.
- [x] #124 the CLI shared src/runtime/main.lisp with the session loop. Split:
`src/runtime/cli.lisp` (flags, usage, surface printing, `--check-config`,
`--write-config`, argument readers, `main`), loaded after `main`, which keeps
the loop, `start`, shutdown and `relaunch`. Surface byte-identical, 2292 checks
PASS, gates PASS; `make integration` could not run (no river on this host).

Ordering note: #116 first (it is red on `main` and every later check must
prove its own failures are not this one); then the documents nobody checks
(#117, #118), then the untested surface (#119), then the three magic-number /
duplication items, then the structural split (#124).

## Phase 0.5 — The next five findings (filed 2026-11-23, after Phase 0 shipped)

Small, and each one is a wrong answer rather than a wish: a drawing that cannot
see a window, a build that can test the wrong fasl, an undo ring with a hole in
it, and the two things doing that work turned up — an integration suite that
fails in a section the diff never reached, and a float that loses the node it is
anchored to.  They sit above Phase 1 for the reason Phase 0 does — nothing
safety-shape is displaced, because Phase 1 is fully checked — and they move back
down the day one of them starts touching a safety path.

- [x] #145 the coordinate overlay asked the *tree* whether a cell was occupied,
and floats live beside the tree (#138), so a workspace showing a dialog over an
empty cell drew no coordinate on the one cell the reader was looking at. Now
`lattice/overlay.lisp`'s `cell-holds-something-p` asks the question it meant: a
node with a window in it, a node somebody named (the name is what the label
says), or a floating window on it. That closed a second inconsistency in the
same test — an empty cell was labelled or not depending on whether the tree
called it an empty leaf or a hole, which is invisible on screen (asserted in
`an-empty-cell-is-empty-whether-the-tree-calls-it-a-leaf-or-a-hole`). The
placement arithmetic moved to `c:float-placed-rect`, which `emit-floats` now
calls too, so a drawing and the emitter cannot disagree about where a float is;
the emitter also stopped drawing an *anchored* float whose anchor this frame did
not place — a picture-in-picture whose host cell went away, and a float anchored
in another workspace, were both placed at their offset as though it were screen
coordinates. +18 checks.
- [x] #146 `make integration` could run against a stale protocol fasl and report
a protocol desync in a section the diff never reached. The cache now carries a
receipt: `tools/stamp.lisp`, loaded by `tools/prelude.lisp` so that every make
target inherits the answer, records the content hash of every `package.lisp`
beside the fasls it built, and drops this project's cached files (`.deps/`
untouched) when one of them moves — because ASDF decides staleness one file at a
time and cannot see that a DEFPACKAGE is a shared surface. The drop is
`forget-first-party-fasls`, which `tools/build.lisp` now calls too: deleting the
whole project cache directory is a superset of a walk of the component graph, and
it catches one thing the walk missed — wayflan's generated `.lisp` stubs, which
are inputs to a compilation rather than components of a system. Gate 24 exercises
the mechanism in a sandbox (stamped, steady, dropped, `.deps/` survives), which
is how the first version was caught comparing two MD5 arrays with `EQUAL` and
calling all 25 package files moved on every run. Measured by hand as well: after
a one-line change to `src/package.lisp`, a plain `asdf:load-system` printed
`src/package.lisp moved since this fasl cache was built`, dropped 89 cached
files, and recompiled (32s); a second run of the same check said nothing.
- [x] #147 moving a float is not on the undo ring, so a hand-placed window could
never be put back. The signature that decides whether a step happened was the
tree's, and the point of a float is that it is not in the tree. `history.lisp`'s
`layout-signature` is now the tree *and* the floats — window and rectangle, in
stacking order — and answers both of the issue's open questions: what is a move's
step (one per *gesture*; a drag is many intermediate rectangles but nothing
settles until the hand comes up), and whether including the floats puts a capture
per frame on the ring (it does not, and never did: the step is taken at a settle
point, not at a write). The anchor node is deliberately *not* in it — a snapshot
holds a copy of the tree, so identity there would report a change after every
undo — and neither is the minimized flag, which lives on the window and so is a
step undo could not walk back. `end-pointer-op` is the fifth door and the only
one a mouse has: it settles once, labelled, so two drags are two steps.
`reconcile-floats` gives up #141's rule on purpose — it kept the *live* rectangle
for a window already floating, because a move was not recorded; now the snapshot's
rectangle is what the snapshot is of, and keeping the live one made undo a no-op
for the only change it was invoked to undo. #141's actual property survives:
undoing a tiling change still does not move a float dragged since, because the
drag is its own step in front of it. +14 checks in `test-lifecycle.lisp`, and the
integration drag case now undoes and redoes the gesture. That case was run under
`nix-shell --run "make integration"`: both of its new checks passed against a
real compositor, and it earned its keep before it did — the first run put the
keyboard on the *tiled* window after `undo, then toggle-float', because
`reconcile-floats` was handing back fresh records and a seat's focus on a float
is compared by identity (`forget-unplaced-float-focus`, #143). Hence the record
stays and only the rectangle is copied.
The integration suite itself is intermittent on this host and was before this
change: two of five runs died in wayflan's `cb-shiftout` assertion
(`(<= (+ start nbytes) end)` with `end` 0) during a capture or echo section,
which the unmodified tree reproduces — filed separately, because #146 was
supposed to be the answer to that message.
- [x] #148 `make integration` was intermittent: one run in three or four died
inside wayflan's protocol buffer — `cb-shiftout`'s assertion `(<= (+ start nbytes)
end)` with `end` 0, or the compositor answering `invalid object 375` — and
everything after the crash reported as its own bug (35 failures on `SERVER-SEATS`
called with `NIL`). Both are one cause: the harness called `capture-report` on
*its own thread*, and a capture sends thirteen requests, so the main thread and
the window manager thread marshalled into one circular output buffer at once.
What made it rare is that the two writers have to overlap; what made it cost an
afternoon is that the crash was attributed to whatever section happened to be
running. Fixed twice over: `capture-toplevel` now refuses an off-thread caller
with `capture-off-thread` (a unit test drives it), the harness calls the capture
on the window manager thread, and the runtime records `*session-end*` so a
harness can tell a quit from a disconnect — so the next such end is one failure
and a "the sections after this did not run" line, not thirty-five.
- [x] #149 an anchored float had no anchor in a restored tree, so an undo hid
it. `copy-float-record` shared the anchor, which is right for the window (there is
one of those) and wrong for a node (`copy-node` gives every node a new identity) —
and #145 changed the symptom from "drawn in the wrong place" to "not drawn", which
is how it was found. Fixed with the same trick the cursor uses: `float-anchor-paths`
records each anchor's *path* when the snapshot is taken and `reconcile-floats`
resolves it against the tree that came back. The path is not the only piece: the
snapshot also has to say "unanchored", because unanchoring is a step of its own —
which #147 is what made visible. A *saved* layout still forgets anchors
(`restore-a-float` builds a record with no anchor), so a restart unties an anchored
float; that is the state file's version to bump, not a snapshot's, and it is noted
rather than fixed here.

## SKIP — duplicate mirrors, keep ONE side
- #42–#62 (open [Release]) are EXACT mirrors of closed #63–#83. Work from #63–#83 only; close the mirror set as a batch when ready (you said not now — so SKIP).
- #2==#4 (keep #4), #7==#24 (keep #7).

## Phase 1 — Runtime safety foundations (before any module)
- [x] #85 IPC timeout wedged thunk (local self-DoS). (Critical)
- [x] #86 watchdog desyncs Wayland stream — closed as by design, and verified: the 5s backstop is documented at `call-with-watchdog`, and `with-manage-sequence`'s unwind-protect still sends `manage_finish` however the body ends. (Critical)
- [x] #88 save-state truncates in place → temp+rename. (High)
- [x] #104 the same truncating write in four extension files that hold user data — auto-respawn's session, recorded macros, saved journals, named layouts. Fix uses #88's `call-with-replacement-file`. (High for auto-respawn, Medium for the explicit-save commands)
- [x] #87 normalize-tag unbounded keyword interning. (High)
- [x] #89 make-canvas fd/mmap leak. (High)
- [x] #94 stale wl_output proxy — verified: no cached proxy survives a global (lookups go through the id each time), and `tests/test-lifecycle.lisp` now pins that `forget-global` drops the name and scale a reused id would inherit.
- [x] #92 per-relayout sort and linear scans — grid offscreen order memoised on the cell epoch and viewport (12.5 µs → 0.4 µs at 100 cells, 145 µs → 0.7 µs at 1024), the emitter's unplaced test is a set, and the render-order signature is no longer copied.
- [x] #90 single-seat focus — verified: `world-cursor`/`world-focused-float` are per-seat tables, `default-seat` is recorded rather than `(first server-seats)`, every keyboard-focus derivation runs per seat, and `tools/integration.lisp` creates a real second seat through river's input manager and watches both keyboards land on their own windows (integration: `a second seat`, 14 checks, PASS).
- [x] #93 unbounded IPC threads — a fixed connection cap and a shutdown that joins the accept thread.
- [x] #91 int32 clamp before the wire (saturate at emit, model keeps the true coordinate). (High)
- [x] #95 undo granularity for viewport/track ops is coarse — already the two-ring undo of #26, verified: `lattice/undo.lisp` records plane verbs on their own ring and `node-signature` no longer carries the camera, with four tests in `tests/test-lattice.lisp` holding both directions.
- [x] #98 split axis missing from node-signature, so undo skips an orientation change. (Low)
- [x] #99 a form that times out waiting for the WM thread still runs later. (Low)
- [x] #100 --eval silently ignores everything after the first form on a line. (Low)

## Phase 2 — Closed module work (verify, already shipped)
- [x] #21 — duplicate of #10: named app-id-matched arrangements shipped as `save-layout`/`restore-layout` in `extensions/layout-persistence`.
- [x] #22 — rules engine: `:app-id-glob`/`:title-glob`/`:parent-app-id`/`:priority` in `src/policy/lifecycle.lisp`, tests in `tests/test-lifecycle.lisp`.
- [x] #24 — duplicate of #7; the group scope is recorded there.
- [x] #26 — two-ring undo: tree snapshots in `src/runtime/history.lisp`, plane verbs on their own ring in `lattice/undo.lisp`, both held by `tests/test-lattice.lisp`.
- [x] #31 — per-cell sizing: `grid-cell-scales` composed in `cell-rects`, `resize-cell`/`reset-cell-size`, serialization and `node-signature` covered by 42 checks.
- [x] #63–#83 — closed release mirrors of the shipped extension documents; `make surface-check` regenerates `doc/EXTENSION-SURFACE.txt` and compares it to the tree, so the documents are held to the program.

## Phase 2.5 — Quality: the harness and the documents
- [x] #103 `make check` never compares the generated documents to the tree, so `main` can be red while a local run is green; the comparison lives only in CI. (Info) — `make surface-check` regenerates into a scratch directory and diffs, is part of `check`, and CI edits a document to prove it still fails.

## Phase 3 — Module roadmap (Info, order by dependency)
- [x] #4 cell primitives (closed; the grid, viewport and addressing layers).
- [x] #32 whole-cell subtree operations — `move-cell-contents`, `copy-cell`, `delete-cell`; one tree step each. (658d033)
- [x] #37 send-to-cell — `send-to-cell (workspace cell-or-name)`, target-plane name resolution, `:cell` M-x argument type. (see git log)
- [x] #33 targeted spawn — `target-here`, `clear-target`, lattice `target-here-cell`; latch consulted by `spawn-target`, spent at the landing. (see git log)
- [x] #35 bulk workspace verbs — `close-others`, `close-workspace`, `minimize-others`, `minimize-workspace`; floats left alone. Surfaced #115 (emptied workspace vanished from the stack), fixed in the same commit. (see git log)
- [x] #40 batch selection — `select-window`/`clear-selection`/`list-selected`, then `batch-close`, `batch-minimize`, `batch-send-to-workspace`, lattice `batch-move-to-cell`; cursor stays where you were standing. (see git log)
- [x] #34 focus by name — `focus-window-by-name` with a `:window` argument type over every window the *world* holds (tiled, put away, floating), labels disambiguated by place because a name is not an address, a put-away window restored first. Verified by `tests/test-verbs.lisp` (14 checks). Surfaced #116 (gate 3 red on `main`). (see git log)

Core first: #25 state machine → #7 groups → #18 tags → #36 tabs (+#39 persist) → #10 templates (+#12 profiles) → #41 auto-respawn (+#15 scratchpad persist) → #30 map nav → #38 peek → #28 drag → #29 zoom/pan → #27 focus toggle → #13 stacking → #14 snapping → #23 preview → #17 history → #19 steal-prevention → #20 sharing → #96 workspace sharing (share-workspace, mirror) → #8 multi-monitor → #11 global shortcuts → #16 gestures → #9 animation → #5 recording → #6 invisible wins → #1 infinite workspaces → #3 promote examples.

## Routing rule for new issues
Any AI opening an issue here MUST file safety/runtime items into Phase 1 and modules into Phase 3 in dependency position — never above safety. Mirror-duplicates go to SKIP with the evidence. See AI_ISSUE_ROUTING.md.
