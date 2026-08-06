# The ratio and the untested axis

**LatticeWM, swept 2026-08-05.** Whole repo, region by region. This argues with the
intent, not the bugs — `/code-review` owns those.

**CLOSED 2026-08-06 at `ffdbc94`.** Every claim and every smaller thing below carries its
disposition inline, marked at the head of its section. Four dispositions are used and they
are not the same thing:

| | meaning |
|---|---|
| **LANDED** | done substantially as prescribed |
| **LANDED, DIFFERENTLY** | the observation was accepted and the prescribed remedy replaced; the replacement is named |
| **RULED AGAINST** | the project ruled the other way on the record, and the ruling is cited |
| **WITHDRAWN** | the observation was wrong when written |

Score: 4 claims and 6 smaller things **LANDED**, 2 prescriptions **RULED AGAINST**,
1 smaller thing **WITHDRAWN**. Nine gates were written in the course of answering it
(gate 6 replaced; gates 11–18 new), taking the build from ten gates to eighteen.

The pattern across the whole sweep, stated once because every section repeats it: in almost
every case the finding was right and the *remedy* was the deletion of a mechanism, and in
almost every case what actually landed was a **check** instead — because the mechanisms
were not the fault, the absence of anything asking whether they were connected to the
program was. `*smart-gaps*` got implemented and gate 11; the colliding options stayed and
got gates 15 and 17; the unattached hooks stayed and got gate 14.

---

## The sketch, committed before reading any implementation

Written from README.org, both `.asd` files, the Makefile, all 213 test names, and the
org headings only. Recorded verbatim so it can't be retconned.

> **The want, laddered.** A tiling WM for river whose layout model is a replaceable
> policy layer, plus an optional infinite-plane extension. Two wants, and they are not
> the same want: (1) an Emacs-shaped WM the author personally uses; (2) a *proof* — the
> policy/runtime split is the thesis and the lattice is the falsifying experiment run
> daily by CI. Want 2 is the real one; PLAN.org gives it away with "the real
> requirement: a cheaper AI must be able to extend it." The WM is the apparatus.
> Minimum that satisfies want 2: a tree model, a river client that translates events
> into model ops, one policy protocol of generics, and the lattice outside `src/` with a
> gate asserting it. Maybe 4–6k lines.
>
> **What I'd have built.** Three things and no fourth. `model/` — the node tree as inert
> data plus a total, small surgery API, every operation total over container kinds so a
> foreign kind can never fall off the edge. `wire/` + a session loop that is a *pure
> translator*: river events in, model ops out, no decisions. `policy/` — one protocol,
> generics dispatching on a policy object and a container kind, shipped answers as
> ordinary methods on it.
>
> **What I'd have refused to build, and expect to find anyway.** A widget layer (bitmap
> fonts, PSF parsing, shm surfaces, a minibuffer with a history ring and readline chords,
> echo area, help overlay, which-key overlay, welcome screen, drawn map) — a second
> product wearing the first one's clothes; I predict this is the largest single mass in
> the tree. And a settings framework: `define-option` with docstrings, defaults,
> rationale, `--list-options`, `--write-config`, a generated man page.
>
> **Prediction I expect to be wrong about.** That the policy/runtime split is theatre.
> The lattice-as-separate-system gate is genuinely good and FINDINGS.org's willingness to
> enumerate the core edits suggests the experiment was actually run. I expect the weak
> point to be the *measurement* of it, not the split.

### Scoring the sketch, because that is the point of writing it down

**The widget-layer prediction was wrong, and wrong in the way that matters.** It is 2,115
lines — 8.8% of hand-written Lisp, fourth-largest region, not first. The shm/canvas/damage
machinery is written **once** in `src/runtime/surface.lisp` and has five consumers
(`echo.lisp:53`, `help.lisp:35`, `cursor.lisp:61`, `lattice/map.lisp:118`,
`lattice/overlay.lisp:81`). There is no per-overlay duplication. Of the six widgets I
listed, only **three surfaces exist**: the minibuffer rides the echo area, which-key rides
the echo area (there is no which-key overlay), welcome rides the help overlay.
`show-help-page` is **five lines** (`help.lisp:127-131`). Seven user-visible features on
two surfaces. And the load-bearing fact I didn't have: D18 makes focus a *place*, so a
focused empty pane has no window to hang a border on — `surface.lisp` has to exist for
the cursor whether or not help does, and the marginal cost of the help overlay is 140
lines on machinery that was already paid for.

The one thing that survives from that prediction is `psf.lisp` — 131 lines of PSF1/PSF2
header archaeology and a shell-out to `gzip`, so a user can swap one bitmap console font
for another bitmap console font of the same shape, given a blitter that only supports
monospace 1-byte-per-row anyway. Delete it and `M-x load-font` dies and nothing else does.

**The settings-framework prediction was right about the shape and wrong about which end is
expensive.** `define-option` is 13 lines and buys five features. The cost is downstream:
3,787 lines of generated documentation committed to git, plus a hand-written man page and
config man page restating the same facts a fifth time.

**The prediction I expected to be wrong about, I was right about — for a reason I did not
anticipate.** The split *is* real. The measurement is worse than weak; see claim 1.

### Coverage

Eleven regions, every tracked file in exactly one, all eleven read. Excluded and named:
seven vendored protocol XMLs + `PINNED`, twelve PNGs, four generated `doc/*.txt` plus
`OFL-TERMINUS.txt`, two generated man pages, `LICENSE`, `flake.lock` — 29 files.
102 + 29 = 131 = every tracked file.

Two regions (the lattice, and build/gates) were interrupted on the first pass and re-run;
`tools/gates.lisp` and `FINDINGS.org` were additionally read start to finish by hand. One
region (docs/examples) was missed on the first pass and run afterwards. Nothing was
downgraded to a filename skim. Named gaps: nobody ran `make`, the gates, or the
integration suite — no SBCL on this box — so every count here is static, reconstructed
from source and `git show`. The "1000+ checks" figure in README is therefore **unverified,
not refuted**: 213 `(test …)` forms and ~563 static `(is …)` forms, with loop constructs
multiplying an unknown number of them.

> **Settled since.** The named gap closed itself: everything below was landed on a machine
> that could run `make check`, so the counts are no longer static. **1,707 unit checks** and
> **221 integration checks in 27 sections against a real headless river 0.4.6**, under
> eighteen gates. The README's "1000+" was an understatement, and the reason nobody could say
> so is that nothing counted them — `c014ccf` found "566 tests" in three places against 1,433,
> which `ASSESSMENT.org` had already caught once before.

---

## 1. Gate 6 does not measure the property it is named after, and four commits were spent arranging the tree to satisfy it

`delete` — and replace it with the check gate 3 already knows how to write.

> **LANDED — `f74cd07`.** Gate 6 is now "how much of the behaviour is answered from outside
> `src/`" (`tools/gates.lisp:371`). It loads `lattice/` and the four worked examples — after
> gate 4, whose job is proving the core comes up without them — and is the first thing in the
> build ever to compile `examples/`. **15 of 65 generics, 22 methods.** Both floors are set
> *at* the number rather than below it, on the argument that nothing can be rearranged to
> lower either, so slack would only be there to spend. The old ratio is deleted; the file's
> header carries the argument for deleting it, since deleting a gate is the thing that file
> is least willing to do.
>
> The fifth core edit is repaired: `cursor-place-name`, default answers the cursor path, the
> lattice answers `cell-string` — four lines, outside `src/`, zero behaviour change. And gate
> 3 gained the direction it never had: **no file under `src/` may name an extension's private
> namespace**, scanned with comments and string bodies blanked so the docstrings explaining
> the rule can neither satisfy it nor trip it.
>
> Both new checks were verified by breaking them.

Gate 6 (`tools/gates.lisp:162-212`) counts non-blank, non-comment lines. `runtime` =
`src/wire/*` + `src/runtime/*` minus `font.lisp`. `policy` = `src/model/*` +
`src/policy/*` + **`lattice/*`**. Ratio = policy ÷ runtime, floor **0.80**. Current: 6487
/ 6423 = **0.9901**.

It passes on two accounting choices, either of which alone sinks it:

- move `src/policy/appearance.lisp` (503) and `src/policy/keys.lisp` (353) back to
  `src/runtime/` → 7343 / 5567 = **0.7581, FAILS**;
- drop `lattice/` (1,422 counted lines of an **optional extension that gate 4 exists to
  prove the core does not need**) out of the numerator → 5001 / 6487 = **0.7710, FAILS**;
- both → **0.5645**.

The margin over the floor is exactly the two files that were moved last, plus an
extension in the numerator.

Now the history. Four commits, with the ratio reconstructed under gate 6's own rule at
each parent and each child:

| commit | before → after | what moved | verbatim? |
|---|---|---|---|
| `4fe8811` "Gate 6 slipped to 0.97 …" | 1.0091 → 1.0130 | 11 lines into policy | a `defgeneric` docstring |
| `0e1d166` "the widget layer starts crossing the line" | 0.8196 → 0.8556 | 145 out of runtime, 178 into `appearance.lisp` | 52 of 178 = **29%** |
| `7e1f13b` "Gate 6 crosses 1.0" | 0.8556 → 1.0107 | `commands.lisp`, `log.lisp` git-detected **renames**; +82 into `appearance.lisp` | **100%** |
| `5c3db2e` "The keymap and the status line cross the line" | 1.0107 → 1.1653 | `policy/keys.lisp` +278, `runtime/keys.lisp` −222, `runtime/echo.lisp` −99 | **96%** |

And the part that decides it. `4fe8811`'s message says the recovery was "on a real change
rather than an accounting one." That commit **also changed the counting rule** — it added
the `font.lisp` skip. Under the old rule its parent is 0.9740; under the new rule the
*same parent* is 1.0091. So: rule change **+0.0351**, code change **+0.0039**. Ninety
percent of the recovery the commit explicitly denies being accounting was accounting. (The
exclusion is filename-based, not content-based: `src/runtime/font.lisp:138-162` holds 18
hand-authored runtime lines that the "generated file" skip removes from the denominator
today.)

Across all four commits: **two** new policy generics, `echo-content` and `font-for`.
`font-for`'s default `(declare (ignore role))`s and never touches `policy`. Neither has a
single specializing method outside `src/` — not in `lattice/`, not in `examples/`. And
`4fe8811`'s own argument for making `echo-content` a generic was that "a lattice policy
would sensibly add the viewport." The lattice never overrides it. Instead the *default
method* reaches into the extension's private namespace:

```lisp
;; src/policy/appearance.lisp:511-514  — verified by hand
(place (or (and node (c:prop node :lattice/address)
                (format nil "~d,~d"
                        (car (c:prop node :lattice/address))
                        (cdr (c:prop node :lattice/address)))))
```

The generic created to prove the boundary exists is crossed by its own default
implementation, in the same file, hard-coding both the extension's key and its cons
representation. That is a **fifth core edit**, it is not in FINDINGS.org's list of four,
and it is the one edit for which an extension point already existed.

**Steelman, and I built it properly.** The ratio caught something no other check could:
`PLAN.org:1421` records `echo-content` being a policy generic whose only method lived in
the runtime — "overridable in principle and on the wrong side of the line in fact." Gate
2 sees a documented generic; the tests never notice which package a method is in. That is
a real catch and nothing else in the project would have made it. `PLAN.org:1229` records
refusing to game the metric when `psf.lisp` moved it the wrong way, which is the correct
response and is on the record. And `5c3db2e`'s central move is defensible on merit
independent of the number: `modifier-mask` sat in `src/wire/` and nothing in `wire` called
it. That is the right tell in the right direction.

**And it does not survive.** A proxy is legitimate while it tracks the property. This one
has three independent tells that it stopped: the floor was set at 0.80 in the same session
the ratio was walked from 0.82 to 1.17 by relocation, so "deliberately well below where
the project sits" describes a floor set *after* the position was arranged; two of the four
commits reproduce 96–100% of the moved lines verbatim and add zero dispatch points; and the
one commit that claims to be a code change is 90% a rule change. When the metric and the
property disagree, the tree gets rearranged to satisfy the metric. That is not a
measurement, it is a shape the codebase has been bent into.

**The change.** Delete gate 6. Replace it with the check gate 3 already contains in
embryo — gate 3 counts `defmethod`s in `lattice/` against a floor precisely because "an
extension that answers almost no generics is going round the protocol." Generalize that:

1. count policy generics with **≥1 specializing method defined outside `src/`**;
2. count methods on policy generics defined outside `src/`;
3. floor both.

That measures the Emacs claim — how much of the system's behaviour is expressible above
the boundary — which line counts by directory do not. First commit: write the check, print
the number, no floor. **The number today is 14 of 62.** Then argue about the floor with
the real number in hand.

**Cost.** ~30 lines, one gate deleted, one added. Zero source files move. The awkward part
is not the code, it is that the new number is much less flattering than 0.99, and the four
commits that moved files to satisfy the old one do not get undone by it — most of those
moves were right for other reasons and should stay.

---

## 2. The project wrote down its own ceremony threshold, blew through it 2×, and built a reporter that congratulates it

`wrong-but-keep` on the existing surface. `don't-build` on the next generic.
`rewrite` on gate 2.

> **LANDED, DIFFERENTLY — `f74cd07`, `6f6e8c2`, `40faa79`.**
>
> (a) The specialization ratio is printed, but by the **new gate 6** rather than by gate 2 —
> the number belongs to the gate that floors it, not to the one that counts docstrings.
> Gate 2 still prints the documentation count and now covers both surfaces.
>
> (b) The threshold is a live tripwire rather than a soft gate: `tests/test-surface.lisp:24`
> holds `(<= 10 n 65)`, and when `6f6e8c2` took the count 62 → 64 **the ceiling did not move
> with it** — the count is held one below the ceiling on purpose, so the next generic is a
> decision somebody makes rather than a number that drifts. `PLAN.org` additionally carries
> `#+CLAIM: (= 65 (length (policy-generics)))`, which gate 12 evaluates against the loaded
> image on every build.
>
> (c) `PLAN.org` §generics is rewritten (`40faa79`): it records that the threshold was blown
> through by a factor of two while the paragraph went on saying thirty, and points at
> `tests/test-surface.lisp:24` as where the reasoning now lives.
>
> The 48 unspecialized generics were not deleted, as recommended. The count today is 65.

`PLAN.org:184-185`, verbatim:

> Thirteen. If this list reaches thirty, the decomposition has gone wrong in the direction
> of ceremony. If it drops below ten, it has gone wrong in the direction of a monolith.

The count today is **62** — 60 `defgeneric` in `src/policy/protocol.lisp`, plus `font-for`
and `keys-hint` in `appearance.lisp`. `doc/EXTENSION-SURFACE.txt:3` says so on line three.
`PLAN.org:184` was never revised.

**48 of the 62 have never been specialized by anything other than their own shipped
default.** Not by the lattice, not by any of the four worked examples. Each carries a
5-to-30-line docstring and a gate-2 documentation obligation — roughly 700 lines of prose
bought against a maybe.

No gate enforces the number. Gate 2 prints it as a compliment: *"all 62 generics and 74
commands documented."* And the tripwire that does exist has been moved on the record:
`tests/test-surface.lisp:24` asserts `(<= 10 n 65)` under a 55-line comment documenting
four separate occasions the ceiling was raised — 30 → 40 → 45 → 60 → 65 — each with a
reason. Every reason is good. A tripwire you move four times is a decoration, and its own
comment is the evidence.

**Steelman.** Option value is real: an unspecialized generic costs a docstring and buys the
ability to change one decision later without touching a caller. The six protocol classes
(`LAYOUT-POLICY`, `MOTION-POLICY`, …) make a single-protocol mixin writable, and 62 fine
generics genuinely beat 13 coarse ones if the coarse ones would each need a `cond` inside.
The count going up is also partly gate 6 working correctly — decisions crossing from
runtime into policy *should* increase it.

**Where it dies.** Nothing in the repo writes a single-protocol mixin. The lattice
subclasses `conventional-policy` (`lattice/policy.lisp:9`); all four examples subclass or
specialize `conventional-policy`. The six-class split bought a mixin nobody has written and
cost `policy-lineage-p`, `classp` and `+policy-protocols+`
(`src/policy/protocol.lisp:955-999`) — MOP introspection over a class lineage, existing
only to answer "is this an extension generic?", which was one `subtypep` before the split.
And the option-value argument proves too much: it justifies generic number 63 exactly as
well as it justifies number 14, which is what a threshold is for.

**The change.** Don't delete the 48 — that is a real refactor with no user visible on the
other side of it. Instead: (a) make gate 2 report the **specialization ratio** alongside the
documentation count, so the build prints "62 generics, 14 with any outside specializer"
rather than a compliment; (b) restore `PLAN.org:184`'s threshold as a soft gate at the
count where it currently sits, so the next increase is a decision someone makes on
purpose; (c) fix the two stale statements — `PLAN.org:184` should point at
`tests/test-surface.lisp:24-83`, where the real reasoning now lives.

**Cost.** Ten lines in gate 2, one line in PLAN.org. The number it prints will be
uncomfortable, which is the point.

---

## 3. Three mechanisms serve one want, and the file that documents how to choose between them is the proof

`delete` the 13 unattached hooks and the four name-colliding options.

> **Item 1 LANDED. Items 2 and 3 RULED AGAINST — `17ab76b`, `32a7d0e`, `38b989c`,
> `762455a`.** The observation held in all three cases; two of the three remedies did not.
>
> **1. `*smart-gaps*` — LANDED (`17ab76b`), implemented rather than deleted,** and the
> docstring did not survive being implemented: "a workspace holds exactly one window" fails
> three ways (an empty pane beside a window is a second pane; three tabs are one pane; a
> float is not a pane), so the shipped rule is what the eye can check — one pane on the
> screen, holding a window — decided by asking `layout-children` rather than by keeping a
> second copy of the rule. **Gate 11 is the recommended check**, built from SBCL's
> cross-reference data rather than by grepping, because a grep for an option's name counts
> its own `defparameter`, its export, four docstrings and the man page. 134 options, 0
> unread; verified failing by registering a knob wired to nothing.
>
> **2. The name-colliding options — RULED AGAINST (`32a7d0e`, then `38b989c`).** There are
> five, not four; `*keys-hint*` arrived after this sweep. `17ab76b` had already settled the
> relationship the other way: *the generic is the extension point, the option is what its
> shipped method returns, and a policy that overrides the method stops reading the option.*
> Under that ruling the five pairs are the design's own worked examples, and deleting them
> takes gaps and border width — the two things every tiling-WM user sets on their first day
> — and makes them reachable only by `defmethod`. What was actually unchecked was the third
> clause: `option-shadows` and **gate 15** (an override that takes an option away must offer
> one of its own), and **gate 17** (an option named after a generic must be read by a method
> on it). That found `*new-workspace*` — an option whose reader still exists and never runs
> under the lattice, which every instrument in the project certified as working, including
> gate 11.
>
> **3. The 13 unattached hooks — RULED AGAINST (`762455a`).** This section's own advice was
> "do item 3 last, or not at all"; it was done not at all, and the hooks went 16 → 18 (this
> sweep counted 16; a seventeenth arrived before `762455a`, which added the eighteenth). Three
> of the fourteen unattached ones were **wrong** in ways only a consumer finds — `:startup`
> ran before `connect-to-compositor`, `:output-added` fired on a nameless monitor of no size,
> `:focus-changed` was skipped by two of the five places that move the cursor — which is the
> argument against deleting them: a hook nothing attaches to is not inert, it is unverified.
> `defhook`'s lambda list stopped being a comment (checked at `add-hook` and by a compiler
> macro on `run-hooks`), `:keyboard-focus-changed` was added because the cursor is a place,
> `doc/HOOKS.txt` is generated, and **gate 14** has a static half and a dynamic half that
> records every firing during the integration run.

`src/policy/hooks.lisp:5-14` is a ten-line essay explaining when to use a generic, when an
option, and when a hook. An abstraction that needs a decision procedure has a competitor.

- **Four options collide by name with a generic**, and each shipped method's whole body is
  reading the identically-named special: `gaps` (`conventional.lisp:71` / `protocol.lisp:244`),
  `border-width` (`:78` / `:251`), `move-into-occupied` (`:142` / `:550`), `new-child-side`
  (`:133` / `:535`). Two extension mechanisms, same decision, resolved by which one you
  found first.
- **"What happens when a window opens" is answered four ways**: generics
  (`spawn-target`, `on-window-open`, `should-float-p`, `window-rule-for`), options
  (`*spawn-mode*`, `*float-dialogs*`, `*focus-new-windows*`, `*window-rules*`), and hook
  `:window-opened`. `window-rule-for`'s own docstring (`protocol.lisp:757`) admits it:
  "the declarative escape hatch for people who do not want to write methods; the method is
  still there underneath."
- **16 hooks declared, 3 with any attacher.** `:draw-overlays`, `:reserve-space`,
  `:layout-restored`. The other 13 — `:focus-changed`, `:window-closed`, `:startup`,
  `:shutdown`, … — are seams for hypothetical status bars. Gate 7 checks declared == run
  and structurally cannot see "declared, run, nobody listening," which is the actual state.
- **109 registered options.** 46 of the 95 under `src/policy/` have zero or one call site.
  In 46 commits, exactly **one** default has ever changed. And `*smart-gaps*` — verified by
  hand — has three occurrences in the entire tree: its `define-option`, its export, and
  `doc/EXTENSION-SURFACE.txt:1823` where it is documented as working. It has **no readers**.
  `gaps` at `src/policy/layout.lisp:23` returns `*gaps*` unconditionally. The option does
  nothing, and the metadata system is what makes that legible as a lie.

That last one is the same species as the finding `PLAN.org` §log3 is proudest of — twenty-four
options that could not be *set*. The test written in response checks symbol identity between
the option and `find-symbol` in the user package. It never asks whether anything *reads* it.

**Steelman.** The three mechanisms have genuinely different audiences. `*gaps*` is for
someone who will never write a `defmethod`; `gaps` is for someone who will. P1 in DESIGN
states this as policy: "where a fork is situational rather than principled, ship both."
Hooks that nothing attaches to are the cheapest possible forward compatibility. All true.

**Where it dies for the specific cases named.** The project ships four worked examples of
writing a `defmethod` and a generated 2,067-line surface document for finding one. The
"users who won't write a method" tier is served by `*window-rules*` and `*input-rules*`,
which are real declarative tables with real matchers. The four *name-colliding* options are
not that tier — they are a hedge, and the hedge costs a reader having to know which of two
identically-named things is authoritative. And an option that does nothing at all is not
forward compatibility, it is a documented lie with a gate asserting the documentation exists.

**The change.** Three commits, each independently landable:

1. Delete `*smart-gaps*`, or implement the reader in `src/policy/layout.lisp:23`. Add the
   four-line gate that would have caught it: every registered option is read somewhere.
2. Delete the four name-colliding options; inline the default into the method body. The
   generic was always the real extension point.
3. Delete the 13 hooks with no attacher. Keep the three that have one — and note that
   `:draw-overlays` (5 attachers) is really "a list of overlay drawers" and
   `:reserve-space` (2) is "a list of edge functions, summed." Three `defvar` lists and
   three `dolist`s replace 198 lines, a macro, two hash tables, a gate, and an option.

**Cost.** Item 1 is a one-line fix plus a gate. Item 2 touches four call sites and is a
documented behaviour change for anyone who set them (nobody, by git history). Item 3 is the
only one with real cost: deleting a hook is irreversible in a way adding one isn't, and if
a status bar ever gets written `:focus-changed` is the first thing it wants. **Do item 3
last, or not at all.** The other two are free.

---

## 4. The thesis holds on exactly the axis the one extension needed, and nowhere else

`rewrite`, narrow — three specific decisions, not seventeen.

> **LANDED, all four — `6f6e8c2`.** This is the one section whose prescription was taken
> verbatim.
>
> 1. `p:focus-target` — a generic, the `cond` as its default method.
> 2. `+capture-keys+` → `p:capture-keys`, a generic. Verified against a real river: 214 real
>    `river_xkb_binding_v1` objects.
> 3. `emit-render-order` is asked **once, about the whole render list** — tiled placements and
>    floats together — instead of about the tiled half with floats appended by the caller. The
>    shipped method still says "tiled, then floats, then overlays", so nothing moves on
>    screen; what changed is that a method saying otherwise is now obeyed.
> 4. **The container-protocol surface exists**: `latticewm --container-surface`, generated from
>    the running image by the same formatter as the policy one, gate 2 requiring every member
>    documented. **It is nineteen members**, and six of them have defaults correct for a dense
>    integer-addressed container and wrong for anything else — which is what made skipping them
>    silent. `serialize-node`/`deserialize-node` moved onto the contract.
>
> Generating the document immediately found one in the flagship extension: the lattice
> answered thirteen of nineteen, and the one it should have answered was `c:node-signature`,
> so undo recorded nothing for zoom, pan, resize and name-cell and silently reverted the
> tracks with the tree. One method, in `lattice/`, no core edit.

Let me be precise about what is proven, because a lot of it is.

**The lattice is a real extension.** Measured: 1,941 lines, 26 `defmethod`s. Zero drawing
code copied — it calls `r:overlay-for`, `r:canvas-fill`, `r:canvas-text`, `r:overlay-commit`,
the same machinery `cursor.lisp`, `echo.lisp` and `help.lisp` use. Zero geometry
reimplemented — `divide-rect`, `tree-move`, `tree-swap`, `repair-path` all called, none
defined. Zero parallel command system — all 18 commands use `r:defcommand`, the same macro
the core's 44 verbs use. Every durable fact lives in a real slot; three `props` keys, all
derived presentation. No `&rest` escape hatches. Four reach-arounds, of which three
(`p::%name`, `r::modifier-string`, `r::+direction-keys+`) are missing exports and one is an
SBCL internal behind a `find-restart`. Gate 3's fear — "push everything through props until
the letter passes and the spirit dies" — did not happen. FINDINGS.org is an honest document
and the four core edits it records are, on inspection, four genuine generalizations.

**And it overrides layout and motion.** That is the whole list:
`layout-children`, `clip-rect`, `gaps`, `border-color`, `border-width`, `step-address`,
`entry-address`, `motion-escapes-p`, `keys-hint`, `on-focus-change`, `make-workspace`.

So when the runtime sweep comes back with seventeen decisions taken in `src/runtime/` that
no generic and no option can replace, the thesis has not been tested against a single one
of them. The three that are not merely leaks but *model* decisions:

- **`src/runtime/windows.lisp:275-302`, `apply-keyboard-focus`.** D18 — focus is a place,
  Wayland keyboard focus is *derived* from where the cursor rests — is the single idea the
  README leads with. It is a `cond` in an event handler. There is no `focus-target` generic.
  A policy cannot change the one thing the project says makes it unlike i3.
- **`src/runtime/seats.lisp:195-221`, `+capture-keys+`.** A `defparameter`, not a
  `define-option` and not a generic. **The set of keys the window manager may ever read is
  fixed at compile time.** A policy with a modal editing layer — the single most obvious
  thing an Emacs-shaped WM's users will want — cannot add one.
- **`src/runtime/emit.lisp:418-443`, `emit-render-order`.** It consults `p:render-order`,
  uses the answer for the tiled half, and then appends floats unconditionally at `:429`. A
  policy that puts a float below a tiled window gets ignored, and it will look like the
  generic is broken rather than like the caller is.

**Steelman, and it is strong.** Not everything should be a generic — the 62-count problem
above is the direct consequence of over-supplying them, and a project that generic-ified all
seventeen would have 79 and be worse. Every one of these is defensible as "the runtime's
business." `emit-render-order` may simply be a bug rather than a design position. And the
honest control question — could a stranger have found the surface? — has a better answer
than I expected: `doc/EXTENSION-SURFACE.txt` is generated from the running image, so it
cannot drift, and all 11 generics the lattice uses are in it.

**Where it dies.** Two of the three are not "not everything should be a generic" — they are
the program's own stated central ideas, unreachable. D18 is in the README's first section
under "the one idea to read first." `+capture-keys+` isn't a decision that was considered
and placed in runtime; it is a `defparameter` nobody revisited. And the stranger test has a
hole the writeup doesn't own: the *policy* protocol has a generated, undriftable document;
the **container** protocol has none. `EXTENSION-SURFACE.txt` covers policy generics only,
and nothing outside `src/model/node.lisp` tells you the container protocol has twelve
members or which twelve. A stranger's grid answers six, loads fine, and breaks on copy, save
and focus repair — which is precisely what happened to the author, with the source open
(`lattice/grid.lisp:227-234`). `serialize-node`/`deserialize-node` are documented nowhere
at all, and their failure mode is silent data loss on restart.

**The change.**
1. `focus-target (policy world)` — a generic, with the current `cond` as its default method.
   One generic, one method, one call site.
2. `+capture-keys+` → `define-option`, or better, derive it from the live keymap plus a
   policy generic for the extras. It is a `defparameter` in a project whose entire premise
   is that this class of thing is configurable.
3. `emit-render-order` — use the generic's answer for the whole ordering, not half of it.
   That is a bug fix, three lines.
4. Generate a container-protocol surface document the same way the policy one is generated,
   and put `serialize-node`/`deserialize-node` in it.

**Cost.** (1) and (2) are one generic each and are exactly the shape FINDINGS.org core edit
4 already established as correct. (3) is a bug fix. (4) is the expensive one — it means
`policy-generics` gains a sibling that walks the container protocol — and it is the one with
the largest payoff, because it is the half of the surface the second extension will need and
the first one already tripped over.

---

## 5. The tests defend the extension and leave the program to the screenshots

`rewrite` — grow `integration.lisp`, and make `make check` fail honestly.

> **LANDED, all four — `7d25623`** (CI follow-ups in `c014ccf`, `da72dc9`).
>
> 1. `tests/test-surface.lisp:603-608` deleted. The `remove-method` alone is the restore.
> 2. `make check` sets `LATTICEWM_REQUIRE_INTEGRATION=1`, so a missing river or a missing
>    terminal emulator fails instead of printing a paragraph and exiting 0. Skips are counted
>    in two kinds — a tool that should have been installed and a thing a headless backend
>    cannot have are not the same kind.
> 3. `README.org:437-440` fixed.
> 4. `tools/integration.lisp` rebuilt around a **section** — a name and a body that catches
>    its own errors, so a broken section costs one section, and fixtures are functions rather
>    than scope. It was one 250-line `LET`, which is why it stopped at eighteen: the
>    nineteenth check meant finding the right nesting depth inside somebody else's binding
>    form. **18 checks → 160 in 21 sections**, and 221 in 27 sections by `762455a`.
>
> There is a CI workflow now (`.github/workflows/check.yml`), so "ten CI gates run on every
> build" stopped meaning "when a human types make" — and its first run was red, on the river
> version mismatch nothing else could see (`c014ccf`; the claim that it had been red for a
> month was itself wrong and is corrected on the record in `da72dc9`).
>
> The rebuilt harness found **four live bugs**, none reachable by a test that builds its own
> world, because in every one the model was right and the screen was wrong: a window that
> leaves the tree was never hidden (minimize, and every workspace you are not on);
> `new-workspace` switched the cursor and not the screen; `op_start_pointer` was sent outside
> the one sequence that may carry it, so Super+drag did nothing; and a lost wakeup left the
> screen thirty seconds stale, about one run in five.

| | source lines | test-lines per source-line |
|---|---|---|
| `lattice/` — optional, ships **off** | 1,941 | **0.386** (48 tests) |
| `src/runtime/` — every user, every keypress | 8,511 | **0.053** (~30 tests) |

**7.3× density, in favour of the part that ships turned off.** `test-lattice.lisp` is 22% of
the suite against 12% of the source. `src/runtime/` is 51% of the source against 13% of the
tests.

**5,934 lines — 35% of `src/` — have zero test reference of any kind**, including
`main.lisp` (627), `wrappers.lisp` (462), `config.lisp` (470), `emit.lisp` (443),
`surface.lisp` (437), `server.lisp` (363), `seats.lisp` (321), `windows.lisp` (302),
`pointer.lisp` (280), `session.lisp` (264). Forty-four commands ship in `verbs.lisp`; five
are ever called by a test. Every binding in both README key tables has a docstring, a gate
asserting the docstring exists, and no test that runs the verb.

And so **README.org:437-440 is false**: "HiDPI scaling of what the window manager draws
itself, the layer-shell path that panels and screen lockers use, and pointer drag-resize.
All three are exercised by the test suite." Zero test references to any definition in
`surface.lisp`, `layer.lisp`, `pointer.lisp` or `outputs.lisp`. Two independent readers
converged on this.

The suite also contains **11 lint rules in test clothing** and a handful of tests whose
premise guarantees their conclusion — `the-zoom-ladder-goes-1-2-4-6-8` asserts a
`defparameter` equals itself; `names-are-the-layer-humans-remember` is a test of `gethash`;
`every-policy-generic-is-documented` is gate 2 run twice in the same build, so it cannot fail
without gate 2 having already failed.

One of them is a live hazard. `tests/test-surface.lisp:585`,
`redefining-a-method-takes-effect-immediately`, installs an override on
`p:conventional-policy`, then "puts the shipped method back" — first correctly, with
`remove-method` at `:596`, and then **again**, by `defmethod`-ing a hand-copied duplicate of
`layout-children`'s body onto `conventional-policy` at `:603-608`. The shipped method lives
on `layout-policy` (`src/policy/layout.lisp:32`) and was never removed. So the restore
over-restores: it leaves a permanent copy of the split-layout algorithm frozen in a test
file, on a *strictly more specific* class, for the rest of the image. Every later suite —
devices, minibuffer, the entire lattice suite — dispatches split layout through it. Today
the bodies match and it is invisible. The day `layout.lisp:32` changes, the suite keeps
testing the old version and passes.

The joke is that this is exactly the failure mode FINDINGS.org core edit 3 exists to prevent
— *the class a user is told to specialize must never be the class the defaults are defined
on* — reintroduced by the test that proves core edit 3 works. **Fix: delete lines 603-608.**
The `remove-method` alone is a correct restore.

**Steelman.** `tests/suite.lisp:3` states the position honestly: "A window manager is mostly
untestable without a compositor." The suite bought total confidence in tree surgery, which
is where the thesis lives, and tree surgery is genuinely the part where a subtle bug is
unrecoverable. Screenshot-and-bare-metal verification is real verification; the README's
Status section is unusually careful about what has and hasn't been sat in front of.

**Where it dies.** The project itself refuted the premise. `tools/integration.lisp` runs a
real river headlessly in about two seconds, no screen, no GPU — and found a real bug on its
first run. The Makefile says so in capitals. The response was to write the file and stop: it
has **18 `check` calls** and **two commits in its entire history**, the second of which is
the most recent commit in the repo. Meanwhile `make check` does not set
`LATTICEWM_REQUIRE_INTEGRATION`, so on a machine without river it **exits 0 having verified
nothing**, and three of the eight things README claims it verifies are skipped when no
terminal emulator is on `PATH` — which is the default state of a CI container. There is also
no CI: no `.github/`, no `.builds/`. "Ten CI gates run on every build" means `make check`
typed by a human, and `flake.nix:60-67` — the one automated path — runs build, gates, test
and image, and omits integration entirely.

**The change.** In order, cheapest first:
1. Delete `tests/test-surface.lisp:603-608`. One line of diff, removes a silent-staleness
   trap.
2. Make `make check` set `LATTICEWM_REQUIRE_INTEGRATION=1`. A green `check` that verified
   nothing is worse than a red one.
3. Fix README:437-440 to say what is actually true — it is the only false sentence in an
   otherwise scrupulous Status section, and it is load-bearing for trust in the rest.
4. Grow `integration.lisp` toward the five bugs its own header lists as the class it exists
   to catch. Every one of them lives in a file with zero unit coverage.

**Cost.** (1)–(3) are minutes. (4) is the real work and it is the highest-value work in this
document: the marginal integration assertion is worth more than the marginal unit test by a
wide margin, and the harness already exists.

---

## 6. The record has rotted exactly where the code moved, in a project whose thesis is that the record outlives the author

`rewrite`, small and mechanical.

> **LANDED — `40faa79`** (counts in `6f6e8c2`, `c014ccf`, `5106e34`, `ffdbc94`). It was not
> small and it was not mechanical: the sentences were the cheap half.
>
> **Gate 12 is the missing gate this section names.** Three checks of three kinds. Every prose
> file is *classified* — current or historical — with the list checked against the directory
> rather than consulted, so an unclassified document fails. Every earmuffed name a current
> document writes **in code markup** must be a registered option or an exported special; code
> markup rather than raw text, because org's bold syntax is also asterisks and a check with
> false positives is a check that gets weakened. And `#+CLAIM:` makes any sentence executable
> — an org keyword holding a form, read in `latticewm/user`, evaluated against the loaded
> image, failing the build with the file, the line, the form and the sentence above it. A
> claim must name something the program owns, or `#+CLAIM: t` would be a gate that can be told
> it passes; a claim cannot read a file, so `FINDINGS.org`'s line counts stay gate 3's job.
> **30 claims today.** It caught two errors in the prose written to introduce it.
>
> The seven rows: `EXTENDING.org`'s development loop leads with the control socket and says
> plainly that SWANK is opt-in; `PLAN` §generics carries what happened to its threshold;
> `PLAN`'s flat-bindings row records the submap reversal 300 lines below it; `README`'s forty
> key bindings are pinned to the shipped keymap chord by command. `DESIGN.org` gets the "what
> of this is still true" index — 22 decisions, one row each — **and does not get edited**.
> `FINDINGS.org`'s fossil counts table was **corrected rather than deleted** (`6f6e8c2`), and
> now says what `make gates` says. `SPIKE-WEEK0.org` was **classified historical rather than
> cut**, which is the cheaper form of the same answer.

Verified against source, one row per claim:

| claim | where | verdict |
|---|---|---|
| "Thirteen. If this list reaches thirty…" | `PLAN.org:184` | **STALE** — 62 |
| "SWANK is listening on port 4005" — step 1 of the development loop | `doc/EXTENDING.org:403` | **FALSE** — `swank.lisp:22` makes it `nil`; ASSESSMENT U1's fix broke the extension guide's opening instruction |
| HiDPI / layer-shell / drag-resize "exercised by the test suite" | `README.org:437` | **FALSE** |
| `*smart-gaps*` "drop gaps and borders when a workspace holds one window" | `conventional.lisp:183`, `EXTENSION-SURFACE.txt:1823` | **FALSE** — no readers |
| "lattice/: 1,049 lines, 16 defmethods, 3 core edits" | `FINDINGS.org:457` | **STALE** — contradicted by `FINDINGS.org:52` (1,941 / 26 / 7) in the same file |
| "All six gates run on every commit" | `PLAN.org:280` | **STALE** — ten |
| "Flat bindings for v0.1; submaps in Phase 6" | `PLAN.org:578` | **STALE** — reversed by `PLAN.org:866`, 300 lines below |
| `protocol.lisp` "contains no methods" | `PLAN.org:157` | **HOLDS** |
| "+Y is up… one sign inversion at the boundary" | `DESIGN.org:1269` | **HOLDS** — `lattice/policy.lisp:148`, and nowhere else |
| river-window-management is v4/42 events, not v5/44 | `DESIGN.org:57` | **HOLDS** |

The lattice's own line count appears in **four** places and **three of them disagree**:
FINDINGS.org:52 (1,941/26, correct), FINDINGS.org:457 (1,049/16), PLAN.org:709 (1,060/16),
ASSESSMENT.org:578 (1,529/19).

**Steelman, and it is the best one in this document.** A frozen DESIGN.org corrected by
appendix is the *right* call. The alternative — editing the design doc into agreement with
whatever shipped — destroys the record's only value, which is showing where the plan was
wrong. `PLAN.org:12` says "DESIGN.org is the design. It is frozen," and means it.
`DESIGN.org:57-62` is a correction slip rather than a rewrite. That is discipline, not
neglect. And `doc/latticewm-config.5:22` has exactly the right instinct: "*This page is a
map, not a reference.* The reference is generated from the running program and cannot go
stale."

**Where it dies.** Freezing was right at day 15 and is wrong at day 25, because a stranger
now pays 2,000 lines to reach the four that are current — and the fix is not to unfreeze it,
it is a two-page "what of this is still true" index at the top. More importantly: gate 2
checks a docstring exists, gate 7 checks hooks balance, gate 8 checks events exist, and
**nothing checks that a sentence in a `.org` file is still true about the program.** For a
project whose stated bet is that the reasoning survives without the author, that is the gate
that is missing. `EXTENDING.org:403` is the proof — the security fix landed, the guide's
first instruction became wrong, and every check passed.

**The change.** Fix the seven stale/false rows (an afternoon). Delete
`FINDINGS.org:457-469`, which is a fossil the same file contradicts. Add a "still true?"
index to DESIGN.org. Consider folding SPIKE-WEEK0.org's four surviving facts into that index
and deleting its other 513 lines — it answers one question, settled 2026-07-29, and its
nineteen inbound links are all from the frozen document.

**Cost.** Prose only. No code moves. The one that repays itself immediately is
`EXTENDING.org:403`, because it is the first command a new extension author runs.

---

## Smaller things, stated once

- **`lattice/policy.lisp:251-263`, `clip-rect` is dead.** `:lattice/viewport-bounds` is read
  here and written nowhere in the repository, so the method always falls through to
  `call-next-method` and `:FIXED` zoom's cropped trailing cell — advertised in two
  files — never happens.

  > **LANDED — `fe071e7`.** And `:FIXED` was half-built underneath it: `fixed-tracks` took a
  > *count* and gave every track exactly `*cell-width*`, so `resize-column` and `resize-row`
  > did nothing at all under the mode `resize-column`'s own warning sends you to; a cell whose
  > track had run entirely past the edge was still placed off-screen, and river shows a window
  > unless it is explicitly hidden; and `map-mode-p` decided cells were too small by dividing
  > output width by column count, which is the `:FIT` answer. The core had the same bug with
  > nothing in common but the mechanism — `render-order`'s third tier keyed on an `:overlay`
  > property nothing has ever written — deleted rather than wired up. **Gate 13** is the half
  > that matters: props was the third way to keep state here and the unchecked one, so every
  > extension property key is now read with the *reader* rather than grepped, matched by symbol
  > identity against `prop`, with a `setf` place told apart from a read structurally. It fails
  > on a key read and never written (provably dead) and merely reports the other direction,
  > because a key written and never read is what D20 exists for.

- **`tools/wmeval` (179 lines) is dead on arrival.** It speaks SWANK's wire protocol to port
  4005; `swank.lisp:22` makes SWANK off by default. Its own error text describes the world
  before that decision. `flake.nix:71` installs it to `$out/bin`; `install.sh` never
  mentions it — so nix users get a broken binary on `PATH` and everyone else gets nothing,
  and gate 9, whose entire job is making the image, the installer and the config agree,
  checks only the lattice. Delete it, or point it at the Unix socket `ipc.lisp` already
  serves.

  > **LANDED — `5106e34`, deleted.** `latticewm --eval` is the same capability over the control
  > socket that is on by default, and the binary is its own client. The gate-9 half was worse
  > than this bullet knew: `flake.nix` had a hand-maintained `installPhase` beside
  > `install.sh`'s list and the two had drifted, so `lattice.asd` — gate 9's *own* founding bug
  > — was still uninstalled on the nix path for the whole life of the gate, plus man pages,
  > `latticewm-session`, and both licences. **There is one installer now**: `installPhase` runs
  > `./install.sh --prefix $out`, and gate 9 asks that it delegates rather than naming
  > artifacts, since a gate that names an artifact can be satisfied by adding a row.
  > `tools/install-check.sh` does the half a gate cannot — install to a scratch prefix, assert
  > every promised artifact, then assert `--uninstall` gives all of it back.

- **`src/protocol/wayland.xml`, 143 KB, named by nothing** — not `latticewm.asd`, not gate
  5's pin list, no grep hit anywhere. Gate 5's own comment says an unlisted XML is exactly
  the bug it exists for; the fix hardcoded three more rows instead of enumerating the
  directory.

  > **WITHDRAWN — wrong when written.** `src/protocol/PINNED:3` named the file and gave its
  > reason (`f63532a`, two days before this sweep), and gate 8 already read
  > `(directory "src/protocol/*.xml")` — it enumerated the directory at the time this bullet
  > said the fix had failed to. The file is there *for* gate 8: wayflan generates the core
  > protocol into its own package, so nothing compiles it, but the gate checks that every event
  > we listen for exists on the interface we listen for it on, and we listen to `wl_output` for
  > the name and scale `river_output_v1` does not carry. Gate 5 pins interface/request/event/
  > enum counts for the six *river* protocols and has nothing to count here. No commit was
  > needed and none was made.

- **Tags and named scratchpads do not survive a restart.** `serialize-node`
  (`state.lisp:115-118`) writes `:window` and `:label` only. `tags.lisp:5-8` opens by
  arguing that a dead slot in a *persisted* state file is worse than one in memory;
  `window-tags` has never been in that file. Two lines fix it.

  > **LANDED — `6f6e8c2`.** A `:windows` section on the same identifiers. Put a terminal away
  > under the name `music`, restart, and it came back as an anonymous window in the tree: the
  > name gone, the putting-away undone, and the one command for getting it back keyed on the
  > name. Adding a key is not a shape change, so no version bump.

- **`Makefile:129-136`** redirects `make surface` output with `> doc/X.txt 2>/dev/null`. The
  shell truncates the target before the command runs and the diagnostic goes nowhere. If
  `cli.lisp` fails to load, the shipped extension surface, command list, option list and
  keymap all become empty files, the target exits 0, and `install.sh` installs them.

  > **LANDED — `6f6e8c2`.** Write to a temporary, keep stderr, replace only on success and
  > non-empty. A generated document cannot go stale, which is its whole argument — but it can
  > go blank, and blank is worse than stale because nothing about it looks wrong.

- **Five dead public functions in `model/`** — `tree-insert-at`, `weight-at`, `set-weight`,
  `normalized-weights`, `axis-of` — plus `replace-child`, which is **exported at
  `src/package.lisp:152` and never defined anywhere**. A `defgeneric` that does not exist,
  on the advertised container protocol.

  > **LANDED — `6f6e8c2`** (`replace-child`) **and `228f77d`** (the five, deleted rather than
  > tested). `axis-of`'s whole docstring was "alias for `direction-axis`, for call sites that
  > read better this way", and there were no call sites; `tree-insert-at` is twenty lines with
  > an error branch on a published surface, never executed — an untested error path on a
  > published surface is worse than an absent function, because the absent one fails in the
  > reader's editor. **There was already a next one**: `world-props`, left behind when that
  > accessor was renamed to `props`, exported and denoting nothing for the life of the project
  > — and `export` interns the symbol, so `(c:world-props *world*)` in a config file reads
  > perfectly and dies at run time, reading as a bug in the user's config. **Gate 16** asks the
  > two published packages whether an export names anything at all and whether anything reaches
  > it. Its first draft failed gate 3's own rule — the five dead names in its preamble came back
  > reported as reached by a build tool — so a string in `tools/` counts only when the entire
  > string is one package-qualified name.

- **`defaults-lifecycle.lisp`** is four methods on the same protocol as `lifecycle.lisp`,
  sorted by topic rather than by the engine/answers split its header implies. Merge it. The
  *motion* split is real and earns its file; this one doesn't.

  > **LANDED — `ffdbc94`, the last item in the sweep to be answered.** Merged; nothing moved but
  > text. **Gate 18** is the part that is not the merge: a file whose name begins `DEFAULTS-`
  > must sit beside a file of the same stem, and that file must define no methods. `DEFAULTS-`
  > was decoration — no check, nothing written down, and the one place it *was* written down,
  > the file list at the top of `policy/conventional.lisp`, had itself drifted to "the methods
  > are in five files" over a list of seven. It deliberately does **not** judge decompositions
  > in general: `input-policy`'s shipped answers live in six files for six good reasons, and no
  > rule this project could state would separate those from the lifecycle pair.

---

## Found on the way, not in this document

Answering the sweep turned up work it did not name. Recorded here so the ledger above is not
mistaken for the whole diff:

- **`306ea13`** — `latticewm --extension-surface | head` printed five lines and a sixteen-frame
  SBCL backtrace. SBCL turns EPIPE into a condition rather than letting the default SIGPIPE
  disposition kill the process. It applied to all six listing flags — the six documents whose
  entire purpose is to be piped — so the failure landed on the first interaction with the flags
  the extension guide and the man page both tell people to run.
- **`1a316df`, `d1e458a`, `c014ccf`, `da72dc9`** — river moved `river_window_manager_v1` from 4
  to 5 in a *patch* release, so `nix build` was producing a session entry that could not start;
  it succeeded because nothing in that build had ever connected to the river the same derivation
  pins, until the integration suite (grown by `7d25623`) moved into the build phase and
  something did. Re-vendored at 0.4.6 rather than pinned back. `shell.nix` claimed to pin the
  pair and took the ambient `<nixpkgs>`; the NixOS module defaulted `river` to the *user's*
  nixpkgs and had never been evaluated by anything, since `nix flake check` evaluates a module's
  options and not a system that enables it. Gate 5 now asserts that the version we bind is the
  version the XML declares.
- **`210381a`** — river 0.4.6's two `capture_sessions` events, which `PINNED` had recorded as
  "a feature we do not have rather than a protocol we cannot speak". That is also the argument
  for building it: the one thing you cannot see by looking at your own screen is that somebody
  else can see it too.

---

## What I tried to design better and couldn't

**The sequence discipline.** `src/wire/sequence.lisp` plus the deferred-work channels drained
in `runtime/sequence.lisp:164-182`. Every window-management request became state the emitter
reconciles rather than something a command sends, so a command behaves identically from a
keybinding and from a REPL. I went looking for the cheaper version and there isn't one: the
alternatives are a global lock (which deadlocks the desktop, and `request-manage` hanging
off-thread is in FINDINGS.org's list of things that actually happened) or per-call-site
checks, which is what this replaced. `close-window-later`'s docstring is the receipt —
Super+q was refused on every machine, silently, until this existed — and FINDINGS.org records
it paying off three separate times on first contact with a real compositor, each as a named
continuable error at the point of use instead of a dead connection to bisect. This is the
best thing in the codebase and it is not close.

**`tests/test-surface.lisp:123`, `every-option-is-reachable-from-a-config-file`.** It checks
`(eq (find-symbol name '#:latticewm/user) variable)` — symbol *identity*, not presence. It
found 24 of 31 runtime options silently unreachable from a config file while every other
check in the build was happy, including the ones that printed those options as working. I
would not have thought of it, and the shape generalizes: check the relationship between two
independently-maintained artifacts, because that is the thing no human ever verifies. (It is
also the direct template for the missing readers gate in claim 3.)

**The container protocol's totality.** `copy-node-slots` as a `progn` generic that walks
`container-addresses` and nothing else, so a sparse grid copies correctly and is never named
— with gate 10 standing over it to stop the third `typecase` being written. I would have
written the `typecase`. The docstring at `node.lisp:262-274` records that exact bug already
shipping once, and the fix is the good version of the idea rather than a patch.

**Core edit 3.** Defaults on `policy`, `conventional-policy` left deliberately empty so
`call-next-method` works for the shape every real extension takes. I would have made that
mistake, it is invisible until someone writes the obvious thing, and the error message when
it bites is about CLOS rather than about the mistake. It was found by writing four extensions
the way a stranger would and running them — which is the qualitative check PLAN.org §honest
says there is nobody available to run, run anyway.

---

## The question the repo can't answer

Half of what makes a design wrong is the change it is about to face, and that is not in the
tree. So: **what is the next thing that gets built on this?**

The answer changes the ranking above substantially. If it is a second extension by somebody
else, claim 4 is the top item and the container-protocol surface document is the highest-value
work in this document. If it is daily use on two monitors, claim 5 is — the multi-output
paths are the largest zero-coverage block and the only one where the README's own Status
section admits the hardware hasn't been sat in front of. If it is a public release, claim 6
goes first, because `EXTENDING.org:403` is the first command a new contributor runs and it is
wrong.

Gate 6 stays at the top regardless, because it is the only item that is actively shaping
where new code gets filed.

> **How it was actually answered.** The question was never put to the repo, and the ranking
> turned out not to be needed: all six claims were worked, in roughly reverse order of the
> ranking above. Gate 6 went first (`f74cd07`) as the section asked, but claim 4's container
> surface — the item that would have been top if the answer were "a second extension" —
> landed first of all (`6f6e8c2`), because it was the one whose *absence* the other work kept
> tripping over.
>
> The ranking's premise held anyway. Each claim, once worked, produced a defect that only the
> work could find: the container surface found `node-signature` missing from the lattice and
> undo silently reverting the tracks; the integration rebuild found four; gate 14 found two on
> its first run; gate 12 found two errors in the prose written to introduce it. The three
> gates that passed the day they were written — 15, 17 and 18 — say so in the gate, in `PLAN`
> and in `README` rather than dressed up, because what they defend is not a bug that was found
> but the meaning of a name.
