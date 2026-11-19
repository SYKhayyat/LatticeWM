# Handoff — frozen 2026-11-19

**This is a dated record, not the state of the tree.**  It is what one session
handed to the next on 2026-11-17, kept because the reasoning in it is worth
keeping, and it is listed in gate 12's `*historical-documents*` so that nothing
treats it as current.  Do not edit it into agreement with whatever has shipped
since; read `PLAN.md` for what is left to do.

For the same reason there are no live numbers below.  The line this file
carried — that `make test` reported a specific check count — was wrong within a
week of being written, and a check count is the one claim the build can answer
for itself: `make test` prints it.

## What the session shipped

- **Window rules engine** (#22).  `src/policy/lifecycle.lisp` gained
  `:app-id-glob` / `:title-glob` / `:parent-app-id` match keys and a
  `:priority` ordering; `window-rule-for` stable-sorts higher priority first
  and strips `:priority` from the overrides it returns.  Docs in `README.org`
  and `doc/EXTENDING.org`; tests in `tests/test-lifecycle.lisp`.
- **Auto-respawn extension** (#41).  `extensions/auto-respawn/`.  Records the
  launcher of each window (spawn argv matched to app-id, with the
  `*window-command-guesses*` alist as the fallback), saves the session on every
  layout change and at shutdown, names windows by app-id so the file outlives
  the compositor, and `respawn-session` runs the launchers back into the panes
  they left.

## What it left open, and what became of it

Recorded here because a handoff note's value is in the second half, and the
second half is what the next sessions wrote:

- **`respawn-session` was a function, not a registered command.**  Reachable
  from Lisp, not from `M-x` or a key binding, while three documents called it a
  command.  Shipped as #118: it is an `r:defcommand` with the same body, and
  the README stopped spelling the namespace `r:`.
- **The by-name window picker and batch selection.**  Shipped as #34 (the
  picker, `focus-window-by-name`) and #32/#33/#37/#40 (selection and the bulk
  verbs over it).
- **`nexus` / `linix`** — the session's second ask (assess uncommitted and
  unpushed work there and finish it) was never reached, and is not recorded on
  any issue.  If it matters, it is still nobody's; the drill was `git status`,
  `git log @{u}..`, finish, commit, push.
