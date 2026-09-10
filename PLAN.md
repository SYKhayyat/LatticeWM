# PLAN — LatticeWM (work top to bottom, one issue per worker session)

Worker loop: top unchecked item only, fix + test, commit, check off, stop.

## SKIP — duplicate mirrors, keep ONE side
- #42–#62 (open [Release]) are EXACT mirrors of closed #63–#83. Work from #63–#83 only; close the mirror set as a batch when ready (you said not now — so SKIP).
- #2==#4 (keep #4), #7==#24 (keep #7).

## Phase 1 — Runtime safety foundations (before any module)
- [ ] #85 IPC timeout wedged thunk (local self-DoS). (Critical)
- [ ] #86 watchdog desyncs Wayland stream (closed — VERIFY fix held). (Critical)
- [ ] #88 save-state truncates in place → temp+rename. (High)
- [ ] #87 normalize-tag unbounded keyword interning. (High)
- [ ] #89 make-canvas fd/mmap leak. (High)
- [ ] #90 single-seat focus, #91 int32 clamp, #93 unbounded IPC threads, #94 stale wl_output proxy (closed — verify), #92 per-relayout sort.

## Phase 2 — Closed module work (verify, already shipped)
- #21, #22, #24, #26, #31, #63–#83 — verify each held; check off.

## Phase 3 — Module roadmap (Info, order by dependency)
Core first: #4 cell primitives → #32 subtree ops → #37 send-to-cell → #33 targeted spawn → #35 bulk verbs → #40 batch ops → #34 focus-by-name → #25 state machine → #7 groups → #18 tags → #36 tabs (+#39 persist) → #10 templates (+#12 profiles) → #41 auto-respawn (+#15 scratchpad persist) → #30 map nav → #38 peek → #28 drag → #29 zoom/pan → #27 focus toggle → #13 stacking → #14 snapping → #23 preview → #17 history → #19 steal-prevention → #20 sharing → #8 multi-monitor → #11 global shortcuts → #16 gestures → #9 animation → #5 recording → #6 invisible wins → #1 infinite workspaces → #3 promote examples.

## Routing rule for new issues
Any AI opening an issue here MUST file safety/runtime items into Phase 1 and modules into Phase 3 in dependency position — never above safety. Mirror-duplicates go to SKIP with the evidence. See AI_ISSUE_ROUTING.md.
