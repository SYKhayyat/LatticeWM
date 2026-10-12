# Handoff

State of the tree at the end of the session that shipped the window-rules
engine and the auto-respawn extension.  Everything below is on `main` and
pushed unless a line says otherwise.

## Shipped

- **Window rules engine** (#22) — commit `7e5fd2f`.  `src/policy/lifecycle.lisp`
  gained `:app-id-glob` / `:title-glob` / `:parent-app-id` match keys and a
  `:priority` ordering; `window-rule-for` now stable-sorts higher priority
  first and strips `:priority` from the overrides it returns.  Docs in
  `README.org` and `doc/EXTENDING.org`; tests in `tests/test-lifecycle.lisp`.
- **Auto-respawn extension** (#41) — `extensions/auto-respawn/`.  Records the
  launcher of each window (spawn argv matched to app-id, with the
  `*window-command-guesses*` alist as the fallback), saves the session on every
  layout change and at shutdown, names windows by app-id so the file outlives
  the compositor, and `respawn-session` runs the launchers back into the panes
  they left.

Both are green: `make test` reports `2003 checks, PASS`.

## Open

- **`respawn-session` is a function, not a registered command.**  The
  extension's `README.org` writes `r:respawn-session` as though it were
  bindable, and `package.lisp` exports it, but the definition is a plain
  `defun` — reachable from Lisp, not yet from `M-x` or a key binding.  The
  follow-up is to split it into an implementation function and a thin
  `r:defcommand` (watch the name collision — `defcommand` may define a function
  of the same name), or register a command through the command API directly.
- **`nexus` / `linix`** — the second half of the session's ask (assess
  uncommitted/unpushed work there and finish it) was not reached.  Locate the
  repo/directory and do the same drill: `git status`, `git log @{u}..`, finish,
  commit, push.
