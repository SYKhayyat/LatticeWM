<!-- CONTRIBUTING.md lists what the twenty-two gates refuse, before you hit
     one.  It is worth the four minutes. -->

**What this changes, and why**

<!-- The why is the part that is hard to recover later.  If the reason is not
     obvious from the diff, it belongs here and in the commit message. -->

**Checklist**

- [ ] `make check` is green (build, twenty-two gates, unit suite, integration)
- [ ] New behaviour has a test
- [ ] Anything on the extension surface has a docstring — gate 2 requires it
- [ ] `CHANGELOG.md` has an entry under `## Unreleased`, if a user or a
      packager would notice this
- [ ] `make surface` re-run and committed, if this changes a generic, an
      option, a command, a hook or a key
- [ ] No edits to `DESIGN.org`, `PLAN.org`, `ASSESSMENT.org` or
      `SPIKE-WEEK0.org` — those are frozen records; add a note instead
- [ ] Any new `.org` or `doc/*.txt` is classified in `tools/gates.lisp` gate 12

**If `make check` will not run for you**

Say so and open the PR anyway. `make integration` needs river on the machine
and is the step most likely to be missing; `make build gates test` is the rest
of it and is worth reporting on its own. A contributor who cannot run the
checks is a fact about the checks.
