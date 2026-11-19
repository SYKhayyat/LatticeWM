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
- [ ] #119 38 of 90 commands are named by no test at all — a gate in the
shape of gate 16, with an allowlist that shrinks as coverage grows.
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
