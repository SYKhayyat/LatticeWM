# Eighteen checks that a thing is there, and none that anything wants it

**LatticeWM, swept 2026-08-07 at `204d0a3`.** Whole repo, twelve regions, every tracked
file in exactly one. This argues with the intent, not with the bugs — `/code-review` owns
those, and the handful noted below are noted in one line and left.

The previous sweep (`the-ratio-and-the-untested-axis-2026-08-05.md`) closed on 2026-08-06
with all six claims landed. Reproducing it would be a coverage bug, not corroboration, so
every region was briefed with its findings and told to go around them. Nothing below
repeats it. Two claims below are *about* what it landed.

---

**CLOSED in the commits 2026-08-07 at `befe584`, marked on this document 2026-08-11.**
Twenty-nine commits, all on the day the sweep landed. Every claim and every smaller thing
below carries its disposition inline, marked at the head of its section. Five dispositions
are used and they are not the same thing:

| | meaning |
|---|---|
| **LANDED** | done substantially as prescribed |
| **LANDED, DIFFERENTLY** | the observation was accepted and the prescribed remedy replaced; the replacement is named |
| **RULED AGAINST** | the project ruled the other way on the record, and the ruling is cited |
| **WITHDRAWN** | the observation was wrong when written |
| **OPEN** | not done, and not ruled against — the reason is named and the item is still live |

Score: 7 claims, 6 of the lens-3 remainder, 31 of 33 smaller things, and all 7 items of the
all-Linux list **LANDED**; 3 **LANDED, DIFFERENTLY**; 1 **WITHDRAWN**; 3 **OPEN**. Gates
went eighteen to **twenty-two**. The unit suite went 1,904 to 1,302 — the deflation is the
point, not a regression: 21 lint tests whose loops could not fail were collapsed — and then
to **1,345** as the second batch was written. And `make integration` ran for the first time
in this project's life, on river 0.4.6 on two distributions, which is where the last two
bugs came from.

**A second batch closed on 2026-08-11**, after the questions this report could not answer
were put to the author. Six items: the on-demand SWANK path documented, `*window-rules*`
given a door, a timer facility, `wl_surface.frame`, and the per-output work — which is the
one that changed a disposition rather than adding one. See the postscript.

**And one row of this report is WITHDRAWN on the evidence.** I called per-output workspaces
an architectural gap and the `(prop output :workspace)` mechanism a workaround. Both are
rulings, argued at length, in the docstrings of the generic and the function that implement
them. I had not read either. The correction is in the postscript, in full, because a sweep
that marks its own bad call as landed is worth less than one that says where it was wrong.

**A named gap in the closing, and then it was closed.** The dispositions below were first
read off the commits and the tree, with nothing executed — the machine marking this document
had no SBCL and no river, so a disposition certified that a change was *in the tree*, which
is the same class of check this report spends 1,300 lines complaining about.

That is no longer the state. The second batch was built and run under WSL: **Ubuntu with
SBCL 2.6.0 and Fedora 44 with SBCL 2.6.6, twenty-two gates and 1,345 unit checks on both,
and `make integration` against river 0.4.6 on the headless wlroots backend — 233 checks,
twice in a row, exit 0.** Which means the two mechanisms in this batch that only a
compositor can answer — that river releases a buffer, and that river answers a frame
callback — were asked of one rather than asserted about bookkeeping. The second of those is
a new line in `tools/integration.lisp` beside the first, for the reason this report gave
about gate 8: both are inbound obligations and neither is visible to an instrument that
faces outward.

**The report's central claim, answered.** Six instruments were pointed at existence and are
now pointed at truth or at reach: gate 12 reads docstrings and man pages and fingerprints
the image around every `#+CLAIM:`; gate 16 stopped treating the word "window" in a sentence
as documentation for the export `WINDOW`; gates 17 and 18 grew floors under their own
populations; gate 22 refuses a request named outside `src/wire/`; `install-check.sh` stopped
asking only whether a file exists and started asking how big it is; and CI regenerates the
six documents and diffs them. Four new gates, and the class of check the project knows how
to write is wider than it was.

---

## The sketch, committed before reading any implementation

Written from `README.org`, both `.asd` files, the Makefile targets, per-file line counts,
and 230 test names. **Declared contamination:** I had also read the previous sweep, which
describes parts of this implementation. So this is not a virgin sketch, and I have marked
below exactly where that shows.

> **The want, laddered.** *What does it do* → tiles windows on river. *Who asked* → the
> author, for their own desktop. *What did they actually want* → not a tiler. Wayland
> killed the one property X had that nobody replaced: **the window manager as a live,
> editable system**. StumpWM and sawfish let you redefine your desktop from inside it;
> every Wayland compositor is a C binary with a config file and a SIGHUP. The want is the
> Emacs property — the running system is the editable system — and a tiler is merely the
> thing you have to build first to have somewhere to put it. *Minimum that satisfies it* →
> a Lisp process speaking river's WM protocol, layout held as inert data, decision points
> as generics, and a socket into the live image. **Four to six thousand lines.**
>
> **What I'd have built.** Four things. `model/` — the tree as data plus a *total* surgery
> API, every operation total over container kinds so a foreign kind can't fall off an edge.
> `wire/` — a pure translator, river events in, model ops out, zero decisions. `policy/` —
> one protocol, generics dispatching on policy × container, shipped answers as ordinary
> methods. And a socket. That is the whole program.
>
> **What I'd have refused, and the bold version of the refusal: there is no UI.** A window
> manager does not own the screen — that's the entire point of it. Emacs has a minibuffer
> because Emacs *is* the screen; a WM has `waybar`, `wofi`, `mako`, and the terminal
> already open in front of you. So no shm surfaces, no PSF font parsing, no blitter, no
> minibuffer with a history ring, no help overlay, no which-key, no welcome screen.
> `latticewm --eval` is the command surface and somebody else's status bar is the display.
>
> **The prediction I'm staking this run on** — and it is not the prior report's — **is that
> the governance outweighs the program.** `tools/gates.lisp` + `tools/integration.lisp` =
> 2,553 lines of build-time policing, 11.5% of all Lisp, larger than the entire
> `src/model/` layer that the thesis lives in. Eighteen gates. And the prior sweep says out
> loud what produced them: *"in almost every case the finding was right and the remedy was
> the **deletion of a mechanism**, and in almost every case what actually landed was a
> **check** instead."* A codebase that answers every finding with a new gate has a
> **ratchet with no pawl**.
>
> **And the second stake: the prose is a second product.** ~486 KB of `.org` against
> 22,110 lines of code.
>
> **Where I expect to be wrong.** `src/runtime/input.lisp`, 711 lines. I want to call that
> bloat and I think input on Wayland genuinely is that bad.

### Scoring the sketch, because that is the point of writing it down

**I made the same accounting error twice, in the same direction, and it is the error I came
here to accuse the code of.**

*Error one, the governance stake.* I compared 2,553 lines of governance to `src/model/` at
2,170 — **the smallest layer in the tree**. Governance does not govern the model; it
governs the extension surface, and `src/policy/` is 6,337 lines. Measured against what it
actually governs the ratio is **40%**, which for a project whose only reviewer is a
Makefile is defensible. The gates region caught this and named it correctly: it is old
gate 6's disease — shopping for the denominator that makes the number say what you already
decided — performed by the critique instead of by the code. The arithmetic I did report
was at least internally consistent (code lines throughout, 2,553 / 22,110; on total lines
it is 4,152 / 31,716 = 13.1%, so the published figure was the conservative one). The
comparand was not.

*Error two, the prose stake.* I compared 486 KB of bytes to 22,110 lines of code. Like
against like: **9,452 prose lines against 20,553 lines of `src/` + `lattice/` — 0.46
lines of prose per line of Lisp, and half the bytes.** That is not a corpus out of control,
and the aggregate claim is dead. What survives is the *distribution*: `README` + `INSTALL` +
`EXTENDING` are 17% of the prose and have readers who exist; of the other 83%, one document
has a live reader and three do not.

**The ratchet has a pawl. It was used once, and I was wrong to say it didn't exist.**
`tools/gates.lisp:369-440` is seventy lines killing old gate 6, with three named tells and
the arithmetic shown, including *"Ninety percent of the change the message denied being
accounting was accounting."* That is a project that can delete a mechanism. What the region
found in place of my claim is better than my claim: **855 of `gates.lisp`'s 2,235 lines are
comment, and that prose is the project's only record of six shipped bugs.** There is no
`FINDINGS` entry that survives deleting gate 13 — the bug story lives *in* the gate.
Deleting a gate deletes the history. That is the real ratchet, and it is architectural
rather than psychological, which is why it is pulled once and not twice.

**The "no UI" refusal was half right and I concede the half that matters.** The premise —
"a WM has a terminal already open" — is falsified by this codebase's own field evidence.
`appearance.lisp:471-483` records the first cold user verbatim: *"i have no clue how to
close windows or how to pick which splits"*. There was no terminal open, because opening one
was the thing she could not do. `surface.lisp`, `font.lisp`, `cursor.lisp` and `echo.lisp`
survive on bootstrap alone — ~900 lines, and D18 forces the first of them regardless, since
a focused *empty* pane has no window to hang a border on. What does not survive is the
second half, and the killer is that the alternative surface already shipped: `ipc.lisp` is
a 0600 Unix socket, on by default, one form per line.

**The `input.lisp` prediction was half right, which is worse than being wrong**, because it
is the wrong half that gets defended. It is 922 lines, not 711 — it grew 30% while I was
writing the sketch. ~430 lines are irreducible and the loss is real: a 30-clause
`_support`/`_current` event handler is one line per protocol event, and `double-bytes`
exists because the protocol says `array` and means IEEE 754 in host byte order. Wayland
input configuration genuinely is that bad. But ~290 lines are not wire code at all — 133
lines of `defcommand` and a pretty-printer, and 155 lines of a `mkstemp`/`unlink`/`lseek`
subprocess keymap compiler, which is a *build tool*. They are there because the file is
named after the subsystem instead of after the layer.

### Coverage

Twelve regions, 115 tracked files assigned, each to exactly one, all twelve read.
Excluded and named: seven vendored protocol XMLs + `PINNED`, twelve PNGs, `LICENSE`,
`OFL-TERMINUS.txt`, `flake.lock`, and the previous report — 24 files. 115 + 24 = 139 =
every tracked file.

**Named gaps.** No SBCL and no river on this machine, so nothing was executed: no gate
ran, no test ran, no wall-clock was measured. Every runtime claim below is read off source
and labelled. Three regions read their largest documents structurally rather than line by
line and said so: `PLAN.org`'s 1,690-line session log, `DESIGN.org`'s frozen D1–D22
rationales, and the alphabetical middle of `doc/EXTENSION-SURFACE.txt`. `tools/gates.lisp`
was read in full, all 2,235 lines, because a report whose thesis is about the gates cannot
skim them.

**My line counts were non-blank-non-comment; several regions reported total lines.** Both
are used below and each is labelled. Five files had grown 30–45% between the sketch and the
read.

---

## The claim, stated once

**Every instrument in this project checks that an artifact *exists* in the place the
instrument was pointed at. Not one checks that it is *true*, that it is *used*, or that it
is reached by the thing that actually needs it. There are eighteen of them and they share
one blind spot, so the defects have all migrated into it.**

Six independent readers, who could not see each other's regions, found the same shape:

| instrument | checks that the artifact **exists** | cannot see |
|---|---|---|
| gate 2 | a docstring is there | whether it is **true** |
| gate 5 | a wrapper is there | whether anything **calls** it |
| gate 8 | events we handle exist on their interface | events we are **required** to handle and don't |
| gate 11 / 16 | *something* reads it | whether the **right** thing does — a test counts |
| gate 12 | `.org` sentences are true | the same claim in a **docstring** or a `.lisp` string |
| gate 9 | `installPhase` delegates to one installer | `buildPhase`, the second list, ten lines above it |
| all eighteen | the fully-loaded image | any **other** image |

This is not a list of oversights. It is one design decision, taken eighteen times: a check
is cheap when it can be written as "find X and assert it is present," and expensive when it
must be written as "find everything that would need X and assert one of them arrived." The
project took the cheap one every time, and the reason is visible in the history — nine of
the eighteen were written in a single day, 2026-08-06, in response to the previous sweep.

That day, in thirteen commits: **10,980 insertions, 1,506 deletions — 7.2 : 1.**
`tools/gates.lisp` went **504 → 2,235 lines, 4.4×**. The sweep whose own summary said the
remedy was *"the deletion of a mechanism"* produced eleven thousand lines and deleted
fifteen hundred.

I want to be precise about what I am and am not saying, because the previous sweep's work
was good and most of it landed correctly. I am not saying the gates are worthless — ten of
them are real invariants and gate 11 is the best idea in the file. I am saying that the
*class* of check the project knows how to write has a hole in it, that the hole is where
every finding in this report lives, and that nine more instruments of the same class did
not narrow it.

---

## Lens 1 — What was built

### 1. Gate 6 replaced a metric that could be satisfied by moving files with a metric that is satisfied by one extension written by the protocol's own author, and it sits exactly on its floors

`rewrite` — ten lines, and print the number twice.

> **LANDED — `fd0f9b9`.** Both prescriptions, in the order given. The floors come from
> `lattice/` alone now — **12 generics and 13 methods**, at the number and still a ratchet —
> and gate 6 goes on *loading* `examples/`, because the load is the check that has caught
> something and the count was the freeze. The tutorial is unfrozen: merging 01 into 02 is a
> pedagogical decision again rather than a red build.
>
> And the number is printed twice. The second one is **7 generics and 9 methods** from
> everything outside `src/` that is not the lattice, sitting next to the union figure, with a
> comment saying that a single union figure reads as breadth and is not.
>
> Item 3 — freeze `src/`, hand a second implementer a spec for a layout model the author did
> not design — is **OPEN** and is the report's own most expensive item. See the postscript's
> barrier 2: `latticewm-contrib` is the artifact that would make it affordable, and it does
> not exist. a policy÷runtime line-count ratio that
four commits had moved files to satisfy. Its replacement asks the right question: **how
many policy generics have ever been answered from outside `src/`.** Floors at
`tools/gates.lisp:441,457` — 15 generics, 22 methods — under a docstring that is proud of
them:

> SET AT THE NUMBER, NOT COMFORTABLY BELOW IT, and that is a deliberate break with how the
> ratio's floor was set. A floor with slack under it is an invitation to spend the slack;
> this one is a ratchet. It is safe to be a ratchet because the number cannot fall by
> accident — there is no rearrangement of the tree that lowers it.

That last sentence is true and it is not the problem. Here is the problem. Count where the
fifteen come from. `lattice/` supplies 13 of the 22 entries and 12 of the 15 generics.
`examples/` supplies 9 entries and exactly **3** generics nothing else answers —
`pointer-focus`, `window-rule-for`, `on-window-open`. Union: 15 and 22. **The floors are
set at precisely the number the tree produces, and there is zero slack on either.**

Two consequences, and the second is the one nobody has said out loud.

**(a) Every worked example fails the deletion test in the strongest available sense: the
build does not survive removing any of them.** Delete `01` → 14 generics → gate 6 fails.
Delete `02` → 13 → fails. Delete `03` → 20 entries → fails. Delete `04` → 20 → fails. An
example you cannot delete is not an example; it is load-bearing infrastructure with a
docstring. If the right pedagogical move is ever "merge 01 into 02, they are both tier 1,"
the build says no. **The ratchet is now defending the tutorial's line count.**

**(b) The measurement that stands in for the thesis is measuring n = 1.** Remove `lattice/`
and the numbers are 7 generics and 9 methods — failing both floors by 8 and 13. So 8/15ths
of "the behaviour of this system is answerable from outside the core" is one extension,
written by the person who designed the protocol it is testing, in the same fortnight.

And the experiment is structurally unfalsifiable, which the project states in its own
protocol. `FINDINGS.org:22`: *"if step 4 turns out to require core surgery, /that is the
finding/."* Under that rule the experiment cannot come out the other way — when the lattice
needed something the core lacked, the core was edited, six times, and each edit was
recorded as *output*. That is excellent engineering hygiene and it is the opposite of a
falsification test. Failure has been redefined as a datum. A test you cannot fail is a
punch list in a lab coat.

The project already knows the missing ingredient and has been substituting for it.
`FINDINGS.org:72`: *"the extension surface was correct for the author and incomplete for a
stranger, in ways that only being a stranger revealed."* Look at how each core edit was
actually found — edit 3 by *"writing the worked examples"*, edit 4 by *"using the thing for
an afternoon"*, edit 6 by *"asking what the project checks about itself"*. Every one is a
simulated second party.

**Steelman, and I built it properly.** Not every project gets a second implementer, and
refusing to measure until you have one means measuring nothing. `lattice.asd`'s two rules
are genuine and gate 3 does enforce the letter of them; the lattice really does call
`divide-rect`, `tree-move`, `tree-swap` and `repair-path` without redefining them, really
does use `r:defcommand` for all its commands, and really did get motion across a cell
boundary for zero lines. `FINDINGS.org:389-406` is a real and surprising result and it is
worth the whole project. Gate 6's floors also genuinely cannot be gamed by rearrangement,
which is the specific failure they were built to prevent, and they succeed at it.

**Where it dies.** The claim in `lattice.asd:3` is *"the purpose is falsifiability."* The
evidence supports a smaller claim: **the container protocol plus the layout and motion
generics are sufficient for a second tree-shaped, rectangle-subdividing layout model,
including a sparse, unbounded, two-dimensional, persistent one.** That is a real result.
It is not falsifiability, and the gap is one collaborator wide. Meanwhile the gate that
certifies it has no slack, so the first person who tries to simplify the teaching material
gets a red build and no explanation of why.

**The change.**

1. **Print the number twice — with `lattice/` and without.** Ten lines; gate 6 already
   enumerates per-generic which outside files answer each one. The second number is the one
   anyone actually cares about, and it is **7 of 65 generics, 9 methods**. Do this first,
   because it converts a comfortable 15 into an uncomfortable 7 and makes everything else
   in this section unavoidable.
2. **Source the floors from `lattice/` alone** and keep gate 6 *loading* `examples/` — the
   load is the check that has actually caught something. This unfreezes the tutorial.
3. **The real experiment, if it is ever affordable.** Freeze `src/` at a tag. Hand a second
   implementer a spec for a layout model *the author did not design and would not have
   chosen*, with three rules: no commits under `src/`, no issues filed, no help from the
   author. Every place they stop is a datum, and it has force precisely because nobody is
   allowed to fix it. The output is not "what was missing" (a wishlist) but "what could not
   be done" (a boundary). Pick a model that breaks the core's actual assumptions, not
   another tree of rectangles — **a pinning model where one window is visible in two
   cells** is the sharpest, because `index-placements` (`src/runtime/emit.lisp:133-139`) is
   an `EQ` hash from node to rect and therefore structurally cannot represent a node in two
   places. My prediction, recorded before anyone runs it: that one fails, and it fails in
   the *runtime* rather than in the policy layer.

**Cost.** (1) is an afternoon. (2) is one line and it removes a constraint rather than
adding one. (3) is a day to write the spec plus two to four weeks of somebody else's time,
and the genuinely expensive part is the discipline of not repairing the core while it runs.
The lattice cost more than that.

---

### 2. The examples are the only place the working vocabulary appears, and 438 of 671 published names are in no document at all

`rewrite` — the surfaces document where to hang a method and none of what goes in the body.

> **LANDED — `db81ce8`, completed `2026-08-11`.** All three prescriptions, and the first one
> is the one that was worth the day.
>
> `examples/05-status-line.lisp`: the time, the load average, and how many windows are in a
> workspace you are *not* looking at — twenty lines, no second process. It is the first
> example that uses a hook at all, the first that must be undone, the first that reserves
> screen space, and the first that composes with a peer rather than replacing it. The third
> segment is a fact about the layout, which is the one thing a general-purpose status bar
> cannot produce, and is therefore the argument for the whole seam.
>
> `EXTENSION-SURFACE.txt` is grouped by protocol. Its preamble spent a paragraph on the six
> protocol classes and then sorted 2,459 lines by `string<`.
>
> The vocabulary landed in two halves, and the first half was not the finding. `db81ce8`
> added "The words, once" — nine *concept* words, world/node/pane/path/address/cursor. Useful,
> and not what was missing: the finding was about ordinary *functions*, and `divide-rect` was
> still in no file under `doc/` four days later. "The calls, once" is the other half —
> geometry, paths, the world, the pointer, the hooks — eighteen names with what each is for
> and which are load-bearing. It exists because the generated surfaces enumerate registries,
> and a vocabulary is not a registry. totalling 4,871 committed lines, and stakes a
great deal on them: *"All three are generated from the running image, so none of them can
drift"* (`README.org:381`). The extension surface lists 66 generics with their docstrings,
their methods, and which option each shipped answer reads. It is genuinely undriftable and
the set-difference against `src/policy/` is empty in both directions. I checked.

Now set-difference the *other* way — every non-generic symbol the four worked examples call,
against all six documents plus `EXTENDING.org`:

```
divide-rect   rect-center      resolve-path    world-cursor    prop
make-rect     rect-contains-p  repair-path     warp-pointer    leaf-paths
rect-right    direction-sign   tree-replace-at workspace-path  resolve-chain
window-preferred-size          direction-horizontal-p          remove-hook
```

**`divide-rect` is exported at `src/package.lisp:140`, is the load-bearing call in both
tier-3 examples (`03:59,62` and `04:64`), and appears in no file under `doc/` at all.**
Verified by hand. Broaden it: of ~671 distinct exports in `src/package.lisp`, **438 are
named by no file in `doc/`.**

So the six surfaces exhaustively document *where to hang a method* and document *none of
the vocabulary you need to write its body*. The examples are not a supplement to the
documents — they are the only place the working vocabulary exists. That is why they carry
the whole burden of proof for the project's central claim, and it is not by design.

This is the same shape as claim 1 seen from the other end. The reason the experiment is
n = 1 is not only that no second person has tried; it is that a second person could not
succeed from the documents. `EXTENDING.org` is 728 lines and earns them, and it is prose
that a human maintains — the generated half, the half that *cannot* drift, is the half that
does not contain a usable vocabulary.

**And zero of the four examples uses a hook.** `grep -rn "add-hook" examples/` returns
nothing. `doc/HOOKS.txt` is 270 lines documenting 18 seams, with a preamble teaching a
footgun the project says bit it twice (*"Pass a symbol, not `#'a-function`"*) and a
four-line status-bar snippet for `:RESERVE-SPACE`. The extension surface has two
mechanisms; `examples/` covers one of them 4/4 and the other 0/4.

**Steelman.** The generated surfaces are generated precisely because a hand-maintained list
rots, and the project has the receipts — `EXTENSION-SURFACE.txt` caught `serialize-node`
and `deserialize-node` being declared in the wrong package. A *vocabulary* document cannot
be generated the same way: `divide-rect` is an ordinary function, and "every exported
function, printed" is `apropos`, which SBCL ships and a live-editable system's user has
open anyway. `CONTAINER-SURFACE.txt` also shows the project can do this well when the set
is small enough to triage.

**Where it dies.** The stranger the project is built for cannot run `apropos`, because
running it requires a built image, and the entire argument for committing 4,871 lines of
generated documentation to git is that a reader on the forge does not have one. The
documents were shaped by what was easy to enumerate from the image — registries — and the
vocabulary is not a registry, so it fell out. Nobody noticed because the author has never
needed it.

**The change.**

1. **Write the status-bar example.** It is the one that closes four gaps at once: it is the
   first example that uses hooks at all; the first that must be *undone* (`remove-hook` —
   exported, named in zero documents); the first that reserves screen space, the one hook
   whose contract is compositional rather than fire-and-forget; and the first that composes
   with a second policy rather than replacing it. Four hooks in `HOOKS.txt` say "for a
   status bar" in so many words. The pieces are sitting there.
2. **Add a vocabulary section to `EXTENDING.org`** — the twenty-odd names above, grouped by
   what you reach for them *for* (geometry, paths, the world, the cursor). It is prose,
   it will need maintaining, and gate 16's outward-reachability half already tells you when
   a name is orphaned.
3. **Group `EXTENSION-SURFACE.txt` by protocol.** It is 2,459 lines in one flat alphabetical
   run — and the protocol each generic belongs to is *already printed* in every `methods:`
   line as `(layout-policy t t)`. The document knows the grouping and sorts by `string<`
   anyway, while its own preamble tells the reader that writing a mixin for a single
   protocol is "a real thing you can write." A reader who wants to do that has to read
   2,459 lines and take notes.

**Cost.** (1) is a day and it is the highest-value day in this report. (2) is an afternoon
of prose. (3) is a sort key.

---

### 3. `protocol.lisp` opens by citing a build gate that does not exist, and gate 12 cannot see it because the claim is in a docstring

`rewrite` — point the prose gate at the prose.

> **LANDED — `025d14b`, `9a4ece0`, `e3e198c`.** All six, and the fifth is the one that closed
> the class rather than the instances.
>
> **Gate 20 is the gate `protocol.lisp` claimed.** It also prints the census of everything
> else in that file, because the opening clause was already stretched: 66 generics, 7 classes,
> 6 functions, 2 specials, **0 methods**. Writing it beat deleting the sentence, because the
> rule was good.
>
> **Gate 21 holds `flake.nix`'s `buildPhase`** to running `make check image` rather than
> hand-typing five `sbcl --load` lines three above the `installPhase` gate 9 exists to keep
> honest.
>
> **Gate 12 reads docstrings now** (`9a4ece0`), and the discriminator is case: this tree
> writes code in capitals and emphasis in lowercase, which is org's `~...~` distinction
> already spelled in a convention docstrings follow. 86 names across 1,420 docstrings. Of 131
> earmuffed mentions, three turned out to be shouted emphasis and were rewritten rather than
> exempted. The man pages are read whole with roff escapes undone — 42 names, including the
> forty `latticewm-config.5` named and the check that exists *because of* `*smart-gaps*`
> validated none of.
>
> **`make surface && git diff --exit-code doc/` is a CI job** (`e3e198c`), and regenerating
> immediately found the committed surface already stale — four options had gained a reader
> nobody had recorded. The documents were made reproducible first, because they could not
> otherwise pass: `/home/shaul` out of `OPTIONS.txt`, the author's build tree out of four
> `EXTENSION-SURFACE.txt` lines, `who-references` output sorted rather than left in
> compilation order, and `value:`/`attached:` printed only when they say something — never in
> the generating image, which is the one where they are guaranteed meaningless.
>
> `*generated-documents*` is enumerated against `doc/*.txt`, with `HOOKS.txt` in it. The
> `config.lisp:337` SWANK paragraph is dealt with in the postscript, where the fix inverted. of the file the project calls
the deliverable and points every stranger at:

> This file contains generic functions and their docstrings. **It contains no methods, and
> that rule is enforced by a build gate.** The shipped behaviour is in conventional.lisp.

`grep -rn "protocol.lisp" tools/ tests/ Makefile .github/` returns **nothing.** There is no
such gate. Nothing in the build reads that file's shape. And the file's opening clause is
already stretched: it carries 7 `defclass`es, 6 `defun`s, 2 `defvar`s and ninety lines of
MOP introspection alongside the generics.

This is `*SMART-GAPS*` — a documented rule wired to nothing — the exact defect gate 11 was
written to abolish, in the one file the project hands to strangers. It is invisible because
**gate 12 enumerates `*.org`, `doc/*.org`, `doc/*.1`, `doc/*.5`** (`gates.lisp:1199-1203`)
and this claim lives in a `.lisp` comment. The instrument was pointed at prose files; the
load-bearing prose is in docstrings.

Three more of exactly this, found by three different readers:

- **`config.lisp:337`.** The previous sweep found `doc/EXTENDING.org:403` telling new
  extension authors to `M-x slime-connect` to port 4005, which `swank.lisp:22` makes `nil`
  by default. It fixed the `.org` file. **The identical sentence is still live in
  `sample-config`** — the text `--write-config` writes into every user's home directory.
  The same false claim, in the copy the gate could not see. And the sample config never
  mentions `--eval`, the control socket, or `Super+;` — the three things that work.
- **`gates.lisp:1032`.** `*generated-documents*` is a `defparameter` with a five-line
  docstring, **read by nothing** — one grep hit, its own definition. The gate file has grown
  its own dead option. It is also wrong: it names five files and `make surface` generates
  six. `doc/HOOKS.txt` is generated, installed, advertised at `README.org:379`, and listed
  nowhere.
- **`doc/OPTIONS.txt:1`** — *"Set any of these in **/home/shaul**/.config/latticewm/init.lisp"*.
  The author's home directory, committed to git, in the file the corpus calls the
  un-driftable reference.

And the cleanest instance in the repository, because the fix and the blind spot were
written in the same commit. `flake.nix:155-160`:

```nix
# THERE IS ONE INSTALLER AND THIS IS NOT IT.  This phase used to
# hand-roll a second list of what ships, beside install.sh's, and
# the two had drifted apart everywhere below.
installPhase = ''
  ./install.sh --prefix $out --no-config --river ${pkgs.river}/bin/river
'';
```

That is correct, and gate 9 (`gates.lisp:694-763`) parses `flake.nix` looking for
`installPhase = ''` and asserts that it delegates rather than naming artifacts. Directly
**above** it, `flake.nix:100-112`, `buildPhase` is still a hand-typed copy of five
`sbcl --load` lines headed *"The same steps `make check` runs."* Six references to
`installPhase` in `gates.lisp`; **zero to `buildPhase`.** Same file, same commit: one phase
healed, the second list left standing directly above it, and the gate written to enforce
"one list" aimed at the half that no longer needed it.

That last one generalises into the sharpest version of this claim. **"Generated, so it
cannot drift" is a claim about the generator, not about the repository.** `make surface`
is not in `all:`, not in `check:`, and not in `.github/workflows/check.yml`. Nothing
regenerates and diffs. The 232 KB of committed `.txt` is a snapshot of whichever machine
last ran the command, and its freshness is maintained by somebody remembering to type it —
which is precisely the discipline generated documentation exists to replace. The Makefile
comment at `:159` states the belief plainly — *"A generated document cannot go stale, which
is its whole argument"* — in the same block that records the target once truncating all six
files to empty and exiting 0, and draws the conclusion "blank is worse than stale" without
noticing it had just conceded that stale is possible.

And one committed document contradicts itself on a single page. `doc/HOOKS.txt:29-31`:
*"Every hook below is attached to and watched firing by the test suite or the integration
run; gate 14 fails the build on one that is not."* Body: **16 of the 18 entries print
`attached: 0`.** Both statements are true — `attached` counts the *generating* image, which
loads no tests — and a reader has no way to know that. The one image where that number is
guaranteed meaningless is the one whose output got committed.

**Steelman, and it is the best one here.** Gate 12 is a genuinely excellent instrument and
`#+CLAIM:` is a real invention: an org keyword holding a form, read with `*read-eval*` nil,
evaluated against the loaded image, failing the build with the file, the line, the form and
the sentence above it. It caught two errors in the prose written to introduce it. The
freeze-and-append discipline on `DESIGN.org` is also correct and verified — three commits,
`+2066/−0`, `+89/−0`, `+5/−2`, effectively append-only — and `DESIGN.org:101`'s "*Overturned
by D22, in this document*" is what a record that has not been laundered looks like. A repo
that had edited those rows into agreement with what shipped would have destroyed its only
evidence. That is discipline, and I came in expecting neglect.

**Where it dies.** Gate 12's *automatic* half covers four documents out of ten, covers none
of the six generated ones, and covers no docstring — and nothing anywhere says so. A
stranger reads `DESIGN.org:146` — "*current* to gate 12 — every option they name has to
exist" and "they cannot drift" — and believes two things about the man pages and the `.txt`
files that are not true. Two further holes are structural rather than incidental:
`marked-tokens` requires a *pair* of `~` or `=` on a line, so `doc/latticewm-config.5`
names **40 earmuffed options including `*smart-gaps*`** and the check that exists *because
of `*smart-gaps*`* validates zero of them; and `earmuffed-p` accepts only alphanumerics and
hyphens while roff escapes hyphens, so `*swank\-port*` would fail even if a delimiter were
there. The four hand-written `.\" CLAIM:` lines in that file are the author manually
plugging a hole he may not have known was structural — two of them about `*smart-gaps*`.

**The change.**

1. **Delete the sentence at `protocol.lisp:4`, or write the gate.** The gate is four lines
   (`code-lines` for `(defmethod ` over one file) and the project has the helper. Writing it
   is better than deleting the claim, because the claim is a good rule.
2. **Fix `config.lisp:337`** and replace the SWANK paragraph with `--eval`, the socket, and
   `Super+;`. It is the first thing every user reads.
3. **`make surface && git diff --exit-code doc/` in the check job.** Two lines. It converts
   the largest unbacked claim in the corpus into a fact, and it is the single cheapest item
   in this report.
4. **Make the generated documents reproducible.** Print `~/.config/latticewm/init.lisp`
   rather than the expanded path; drop `value:` from `OPTIONS.txt` and `attached:` from
   `HOOKS.txt`, or print them only when they differ from the default — a per-image
   measurement has no business in a committed file.
5. **Point gate 12(b) at docstrings**, or accept that it does not and say so in its own
   preamble. Extending `code-of` to yield string bodies rather than blank them gives you the
   corpus; the option registry is already the vocabulary.
6. Either delete `*generated-documents*` or make gate 12(a) enumerate `doc/*.txt` against
   it — and add `HOOKS.txt`.

**Cost.** (1)(2)(3)(6) are minutes each. (4) is an hour. (5) is the real work and the one
that closes the class rather than the instances.

---

## Lens 2 — Architecture

### 4. The extension surface ships two mutually incompatible idioms, and the flagship uses the one that cannot coexist with a peer

`rewrite`, narrow — one function.

> **LANDED — `86b8e20`.** The lattice's methods specialise on `lattice-mixin`, which inherits
> `policy` and supplies no defaults; `enable` composes it over the class of whatever policy is
> in force and `change-class`es the object, so a peer's slots come through the switch and
> `call-next-method` reaches its answers. `disable` restores the saved class rather than
> constructing a fresh `conventional-policy`. Two details that are the difference between the
> idea and the working version: the composed class is interned under a derived name, so
> enabling twice finds the same class instead of emptying every dispatch cache, and enabling
> twice at all is a no-op rather than the CLOS error for a mixin appearing twice in one
> precedence list. `lattice-policy` still exists and is what the composition collapses to over
> a plain `conventional-policy`, which is what every document, test and example already names.
>
> `install-vocabulary` shadowing-imports the names already spoken for *before* the
> `use-package`, so the collision is resolved before it can signal and costs one name and one
> log line rather than an unspecified prefix of the vocabulary.
>
> And `EXTENDING.org` now says which idiom to write and why — the sentence this section
> correctly said appeared in no document anywhere. `80d90c9` fixed the recipe it printed: the
> mixin example set a `*previous*` that did not exist. The saved class rides on the mixin,
> which is the entire argument for writing a mixin rather than a global. `(setf p:*policy* (make-instance 'lattice-policy))`.
`:468` — `disable` does `(setf p:*policy* (make-instance 'p:conventional-policy))`.

The four worked examples chose the other idiom: methods directly on `conventional-policy`.
That one composes and cannot carry per-policy slots. The class idiom carries slots and does
not compose.

So: load `examples/03-master-stack.lisp`, run `(master-stack)`, then `(lattice:enable)` —
**the master-stack policy is silently discarded.** Run `(lattice:disable)` afterwards and
you get a fresh `conventional-policy`, not what you had before. The teaching material and
the flagship demonstrate two idioms that cannot be used together, and nothing in any
document mentions it.

It gets worse at load time. `install-vocabulary` (`commands.lisp:492-498`) does
`(ignore-errors (use-package '#:lattice package))` under a `handler-bind` hunting for
`cl:continue` or `sb-impl::take-new`. If no restart is found the error escapes to
`ignore-errors`, the whole `use-package` is abandoned, and **how many names got imported
first is unspecified** — leaving the extension partially installed behind one `:warn` line.
The symbol at stake for any second extension is `enable`, which is the most obvious name an
extension exports. Collisions are resolved by load order. The function's own docstring
exists to prevent exactly the failure it can now produce: *"the whole extension appears to
be broken while working perfectly."*

**Steelman.** A policy object with slots is the right shape for the lattice specifically —
per-plane viewport state has to live somewhere, and `props` on the world is the escape hatch
this project correctly refuses to lean on. And "two extensions at once" is a want nobody has
expressed, in a program with one extension.

**Where it dies.** Claim 1 says the project's central open question is whether a *second*
party can extend this. The second party's first act is to load their extension next to the
one that ships, and the answer today is "one of you wins, silently, by load order." This is
not a hypothetical about a distant future; it is the immediate next step of the experiment
the project most wants to run.

**The change.** Make `enable` install a *mixin* over whatever policy is current —
`(make-instance (ensure-class (list 'lattice-mixin (class-of p:*policy*))))` — and make
`disable` restore the saved previous value rather than constructing a fresh
`conventional-policy`. That is the composable version of the class idiom and CLOS gives it
to you. Second: make `install-vocabulary` import name by name so a collision costs one name
and a log line rather than an unspecified prefix of the vocabulary.

**Cost.** ~30 lines and one saved slot. It changes no behaviour for anyone running one
extension, which is everyone.

---

### 5. `guarded` states its contract in one docstring and violates it in about sixty call sites, and the error path it protects cannot produce a backtrace

`rewrite` — two names and one word.

> **LANDED — `2d97a8c`.** Both, and the concession in the postscript was right to go.
>
> `handler-bind` runs its handler on the signalling stack: log there, then transfer. All
> three macros were built on `handler-case` and all three now bind. `with-abandon` invokes its
> own `abandon` restart from inside the handler, so the liveness guarantee is unchanged and
> the frames are real — it had been *shadowing* `install-debugger-hook`, the one place in the
> program that captured a true backtrace.
>
> Two names: `guarded` is the policy boundary — log loudly, with frames, keep going, per
> decision — and `best-effort` is the wire boundary, where a request against a proxy river has
> already destroyed is an ordinary outcome and gets a line and no backtrace. Our own calls get
> neither, and the three sites that keep a guard deliberately carry the argument at the site.
> The per-decision granularity is written down as the justification, which the docstring had
> never given.
>
> `*debug-on-error*` is new and is the postscript's point in one option: with it set, all
> three decline to handle, so a connected SLIME gets the debugger, the real stack and the
> restart. There had been no such thing, in a program whose thesis is that you edit it while
> it runs.
>
> `tests/test-boundaries.lisp` is the part that is not the fix — nothing had ever asked these
> macros a question, which is how all three came to be built on the wrong operator. The
> assertion was checked against the bug: reverting `guarded` to `handler-case` makes it fail.

> Used at every boundary where a *policy method* is called — that is, at every point where
> user code runs inside ours. […] Deliberately *not* used around our own internal calls:
> swallowing our own bugs would turn them into silent misbehaviour, which is much harder to
> find than a backtrace.

**127 call sites. At least 51 wrap a `w:`/`river:` wire call; roughly a dozen wrap our own
internal functions** — `(guarded "snapshot" ...)` (`history.lisp:107`), `(guarded
"save-state" ...)` (`state.lisp:261`), `(guarded "deferred manage work" ...)`
(`sequence.lisp:105`), `(guarded "ipc shutdown" ...)` (`main.lisp:497`). Every one is our
own code swallowing our own bugs, producing exactly the silent misbehaviour the docstring
forbids. Once a boundary marker means anything it means nothing, and the log now prints
`"window destroy: ..."` next to `"layout: ..."` with nothing to say which one is somebody's
bug.

And the word. `guarded` is `handler-case`, which **transfers control to the exit point
before running the handler**. The stack is gone. So the error report for user code — the one
class of error a live-editable system exists to let you fix — is one line of `~a`, no
frames, no restarts, no route to a debugger from a connected REPL, and no
`*debug-on-error*` anywhere in `src/`.

`with-abandon` is worse because it promises: `log.lisp:260` says *"on any error, log it with
a backtrace and carry on,"* and `:275` calls `(log-backtrace 20)` from a `handler-case`
clause — post-unwind, so it prints the frames of the `with-abandon` site, not of the error.
The **only** place in the program that captures a true backtrace is `install-debugger-hook`
(`log.lisp:317`), which runs from `*invoke-debugger-hook*` *before* unwinding — and
`with-abandon` handles the error first, so the working path is permanently shadowed by the
broken one.

**Steelman, and it is the reason `guarded` survives at all.** Delete it at the policy
boundary and an error reaches the debugger hook, which logs with 40 frames and invokes the
`ABANDON` restart that `with-abandon` establishes around every event handler. The desktop
does not freeze. But it abandons the whole *manage sequence* — `emit.lisp` alone has ~20
guarded policy calls in one — so one bad `clip-rect` costs the other nineteen windows their
placement. **Per-decision granularity inside a sequence that must complete is the real
justification, and it is not the one the docstring gives.**

**The change.** One word and one name. `handler-bind` instead of `handler-case`, log the
backtrace on the signalling stack, then `(invoke-restart 'abandon)` — same liveness
guarantee, real frames, two-line diff. Then split the name: `guarded` for the policy
boundary (a user's broken method: log it loudly with frames, keep going) and `best-effort`
for the wire boundary, where a destroyed proxy is expected and the log line is noise.

**Cost.** The `handler-bind` change is two lines and is the highest-leverage change in the
report for the stated want. The rename is 127 call sites, mechanical, and can be done
incrementally — new code uses the right one, old code is moved when touched.

---

### 6. Undo is bolted to `run-command`, so the three doors the project exists for have no undo at all

`rewrite` — move the hook one level down.

> **LANDED — `c520cae`, and the integration run found the rest of it in `befe584`.**
>
> Undo is taken at settle points now, not around calls. `note-layout-settled` compares the
> tree's signature against a baseline kept in the world and copies only when they differ, so
> the copy that becomes the undo step is one that was already made. `after-command` calls it,
> which covers the keys, `Super+;` and the control socket in one place because all three
> converge there; `drain-wm-queue` calls it for the REPL. `load-state` re-baselines against
> the layout it restored, so the first change after a restart is a step and not a diff against
> an empty tree. `with-undo`, `record-undo` and the nineteen-name `*undo-exempt-commands*` are
> gone — the deny-list disappeared exactly as predicted, because the test became "did the root
> change" instead of "which function ran". `note-layout-settled` is exported and documented as
> the call an extension makes when it opens a fifth door.
>
> The mutation ruling at `model/surgery.lisp:5` stands, and did not need overturning.
>
> **And then the compositor said something 1,302 unit checks could not.** The first
> `make integration` run in this project's life found that undo through three of the four
> doors went back to the *beginning of the session*. `*undo-coalesce-seconds*` merges
> consecutive steps sharing a label — right for "resize left", where the label repeats because
> the gesture repeats, and wrong for "from a REPL", which every change through SWANK, the
> socket and `Super+;` carries. An entire session coalesced into one entry holding the oldest
> tree. A step is coalescible now only when its label came from a command, spelled as a
> defaulted parameter so a door that invents a new label is covered by construction rather
> than by being remembered. This section was right about the altitude and could not have found
> this: it needs four doors, a live compositor and a clock. `undo-command-wrapper` on `p:*command-wrappers*`, and the file
argues correctly that this beats seventeen verbs opting in. But the wrapper's reach is
exactly `p:run-command`, and **none of the live-system doors go through it**:

- `eval-expression` (`minibuffer.lisp:552`), bound to `Super+;` — `(eval form)`, directly.
- `evaluate-for-ipc` (`ipc.lisp:180`) — `(prog1 (eval form) (after-command))`, directly.
- SWANK — arbitrary.

So `Super+;` `(setf (c:world-root *world*) (c:make-leaf))` destroys your layout with no undo
entry, while `Super+h` records a snapshot for a cursor move that changed nothing. **The
mechanism that exists to make the tree recoverable is blind on precisely the surface the
project is about.**

The cost of the wrong altitude is visible in two places. First, `*undo-exempt-commands*`
(`history.lisp:111`) is a nineteen-name literal deny-list — not exported, not an option, not
printed by `--list-options`, not reachable from a config file — doing two unrelated jobs
(`undo`/`redo` for correctness, seventeen others for speed). Forty lines away,
`policy/commands.lisp:134` solves the identical problem correctly: `*not-repeatable*` is a
registered option backing a `command-repeatable-p` generic, whose docstring says outright
that *"a denylist of names cannot express 'not repeatable under these circumstances'."*
Second, see Lens 3 — the deny-list exists because the snapshot is taken before the test that
decides whether to keep it.

**Steelman, and I concede the ruling underneath it.** `model/surgery.lisp:5` rules that the
tree mutates — *"a window manager is not a good place for persistent data structures, and
StumpWM has mutated happily for twenty-five years."* That is defensible and I would not
overturn it: a persistent tree pushes copy-on-write into `insert-child`, `(setf child-at)`
and `remove-child`, three members of an **open** protocol that third parties implement, and
that obligation on extension authors is far heavier than one wrapper.

**Where it dies.** The ruling was made before undo existed and was never revisited when the
thing it costs got built. And the correction does not require persistence: **make the
snapshot a property of the world-root pointer rather than of `run-command`.** One `(setf
world-root)` chokepoint, snapshot on write. Every path including `eval` goes through it, and
the deny-list disappears because the test becomes "did the root change" instead of "which
function ran."

**Cost.** One chokepoint, ~20 lines, deletes a nineteen-entry deny-list, and it is the same
shape as `command-repeatable-p` which the project already ruled correct.

---

## Lens 3 — Implementation

### 7. The compositor is reading the buffer while we draw into it, and the comment states the violation as a design property

`rewrite` — one flip, ten lines.

> **LANDED, DIFFERENTLY — `7b49c07`.** A pool, not a flip. An overlay owns a pool of buffers;
> `wl_buffer.release` is handled — one `on-events` form, so **gate 8 sees it and now counts
> eighteen interfaces rather than seventeen**, which is the report's thesis closing on itself —
> and `ensure-overlay` hands back a buffer that is neither busy nor the one on screen, growing
> to a third if a compositor holds two at once and logging rather than growing without bound
> past that. A two-buffer flip assumes the compositor releases before the next redraw; the
> pool does not have to assume, which is the same reason this file vendors six XMLs.
>
> Two consequences that were separate findings in the section below, both forced by the pool:
> the record of what a frame drew moved from the overlay to the *canvas*, because with a pool
> the buffer handed back holds a frame from two redraws ago and clearing the wrong list leaves
> a stale label under a fresh one; and `overlay-commit` damages the union of what the new
> buffer holds and what the one on screen holds, rather than the whole surface.
>
> The unit suite says at the top of `tests/test-overlay.lisp` that it cannot reach the
> protocol half, and asserts the bookkeeping including a two-buffer alternation with the pixels
> read back. The half that needs a compositor is in `tools/integration.lisp`, which asserts
> that a redraw commits a different buffer and that river releases the one it finished with —
> and that half has now actually run.

> Pixels are ARGB8888, premultiplied, one 32-bit word each, row-major. DATA is a foreign
> pointer; **the compositor is looking at the same bytes we are, so a write is visible as
> soon as the surface is committed.**

The project's own vendored `src/protocol/wayland.xml:1448-1453`:

> The compositor may access the pixels at any time after the wl_surface.commit request.
> When the compositor will not access the pixels anymore, it will send the
> wl_buffer.release event. **Only after receiving wl_buffer.release, the client may reuse
> the wl_buffer.**

Verified by hand: **there is no `wl_buffer.release` listener anywhere in `src/`.** The only
`wl-buffer` reference is `.destroy` at `surface.lisp:99`. There is no `wl_surface.frame`
callback either. One canvas per overlay, attached, committed, then written into again on
the next redraw, forever.

`surface.lisp:21` — *"it worked first time"* — is not evidence; it is the symptom of a race
that has not lost yet. The failure mode is tearing or a half-drawn status line under load,
which is precisely what a project with no way to observe its own rendering will never
reproduce.

This is the sharpest instance of the report's thesis in a single defect. **Gate 8 checks
that every event we handle exists on the interface we handle it for. Nothing checks that
every event we are required to handle is handled.** The instrument faces outward from our
code toward the protocol; the defect is on the inbound side.

**Steelman.** Single-buffered shm is what every simple Wayland client starts with, the
overlays are small, redraws are rare, and on a compositor that copies to a texture
immediately after commit you will never see it. The project has run this on real hardware
for a fortnight without a report.

**Where it dies.** "Works on the compositor I tested" is exactly the claim this project
refuses to accept anywhere else — it vendors six XMLs, pins a river version to the patch
release, and wrote gate 5 because a protocol moved 4→5 underneath it. The one place it
takes the compositor's undocumented timing on trust is the one place the spec is explicit
and quoted in its own tree.

**The change.** A two-buffer flip: two canvases per overlay, draw into the back one, attach
and commit, swap. Cheaper than a release listener and it needs no new event handling. ~10
lines in `ensure-overlay` and `overlay-commit`.

**Cost.** Doubles overlay memory. Which brings up the next one, because the memory
arithmetic in this region is already wrong by 8×.

### The rest of lens 3, ranked by ratio and honest about which are cheap

> **All six LANDED — `7b49c07`, `c520cae`, `3fe2de3`, `10c6681`.**
>
> *Damage computed and discarded*, and *`*overlay-buffer-idle*`'s 8× arithmetic*: both in
> `7b49c07`, above. The option is a list of overlay kinds defaulting to `(:help)` instead of a
> `t` that had the drawn map doing `mkstemp`, `ftruncate(33MB)` and `mmap` twice per wobble of
> a zoom — the arithmetic was wrong in its own favour and the default was exactly backwards
> for the one overlay a continuous gesture toggles.
>
> *Every arrow key deep-copying every workspace*: gone with claim 6's chokepoint. The
> mechanism was upside down — it copied first and threw the copy away when the signature came
> back equal — and the deny-list existed to suppress what the arithmetic still let through.
>
> *`container-addresses` on a grid growing for the length of the session* — `3fe2de3`, and it
> is the one place the lattice genuinely fights a core assumption, so it is worth the
> paragraph. `*tidy-on-leave*` (default `T`) drops the cell you leave when arriving there is
> all that ever happened to it, and refuses three: a cell with anything in it, a cell somebody
> named, and the last cell of a plane. What turning it off costs is in its docstring, because
> footprints are a real thing to want and that should be a decision rather than a discovery.
> The consumer's per-iteration closure went with it.
>
> *The cheap ones*, all seven, in `3fe2de3`: `output-contents` asks each output once — which
> also made the answer *consistent*, since the second call used to happen after
> `p:*solo-windows*` was set — `do-windows` walks the table, `emitted` stopped consing a key to
> ask a question whose answer is almost always "nothing changed", `render-order` partitions
> instead of `stable-sort`ing a `copy-list`, `default-address` stopped building the whole index
> list to take its head, and the status line stopped allocating three lists over every leaf to
> produce an integer.
>
> *`request-manage` has no debounce* — `10c6681`. `server-manage-requested` is true exactly
> over the window between asking and being given a sequence, cleared at the top of
> `run-manage-sequence` and cleared *before* the body, because an ask made inside a sequence is
> a genuine ask for the next one. The `setf` is after the request and inside the
> `best-effort`, so a send that signals leaves the flag alone and the next caller tries again:
> the other direction — a flag stuck true — is a window manager that has gone deaf, and no log
> line would say so.

**The damage tracking computes the damage and then throws it away.** `cursor.lisp:71-76` and
`lattice/overlay.lisp:98-103` both go to real trouble — *"a full-screen clear is two million
writes for a few outlines"* — recording the rectangles they drew on `(c:prop overlay
:dirty)` so the next frame clears exactly those. Then `overlay-commit`
(`surface.lisp:415`) does `wl-surface.damage-buffer surface 0 0 (canvas-width) (canvas-height)`,
unconditionally, for every overlay. Full surface. The incremental clear saves *our* memset
and then tells the compositor the entire surface changed, forcing a full texture upload
anyway. The word "damage" in `damage-buffer` is what `:dirty` was computed for. **Ten lines,
and it makes the two incremental drawers actually incremental.**

**`*overlay-buffer-idle*`'s own arithmetic is off by 8× on the machine it was written for.**
`appearance.lisp:304-312` says *"A full-screen ARGB buffer is about four megabytes...
Keeping their buffers costs eight megabytes."* `make-canvas` multiplies by `scale` before
computing stride, so on a 1920×1080 panel at scale 2 the help canvas is 3840×2160×4 =
**33 MB**, and two are 66 MB. The default `t` therefore does `mkstemp` + `ftruncate(33MB)` +
`mmap` + `create_pool` on every `Super+/`. For help that is right. For `:lattice/map`,
toggled by a *continuous* zoom crossing a threshold, it is exactly backwards.

**Every arrow-key press deep-copies every workspace and hashes the result twice, to record
nothing.** The six focus verbs are not in `*undo-exempt-commands*`, so `with-undo` fires:
`copy-node` of the whole root — every workspace, including all forty if the user ever
pressed `Super+x workspace 40` — then `node-signature` conses a parallel structure of the
entire tree, the body changes only `world-cursor`, `record-undo` conses the signature
*again*, both match, everything is discarded. Meanwhile `image.lisp:52` shrinks
`bytes-consed-between-gcs` to 8 MB specifically because *"a GC pause during a keystroke is
input latency, directly and visibly."* **The one file that argues GC pressure matters is
undermined by the one file that generates it per keystroke, and the two never met.** The
`*undo-coalesce-seconds*` mechanism does not help — it compares labels *after* both copies.
Fix is claim 6's chokepoint, or `command-undoable-p` as a ten-line mirror of
`command-repeatable-p`.

**`container-addresses` on a grid returns every cell ever navigated through, and the
consumer walks all of them with a linear `member` inside.** `lattice/grid.lisp:139-156`
returns visible cells first and justifies it — *"the common case touches the front of the
list and stops."* `src/policy/layout.lisp:203-210` does not stop: it walks the whole list,
does `(member address seen :test (lambda (a b) (c:address-equal node a b)))` per element —
**consing a fresh closure per iteration**, since the lambda closes over the loop-invariant
`node` with no `dynamic-extent` — and then recurses into the full subtree of every offscreen
cell. `step-address` calls `ensure-cell` on arrival and `tidy` is deliberately manual, so N
grows monotonically with navigation, is sorted on every relayout, walked on every relayout,
and serialised to the state file forever. **This is the one place the lattice genuinely
fights a core assumption** — the core assumes containers are finite *and small* — and it
does not fight it, it pays, at a cost proportional to session length.

**Cheap and free, listed as cheap.** `p:output-content` is called twice per output per
relayout (`emit.lisp:73` and `:118`, same policy, same output — five lines). `all-windows`
conses a fresh list of every window, called twice per manage sequence and once per drag
event (four lines for a `do-windows` macro). `emit.lisp:30-33` keys last-emitted state on a
freshly-consed `(cons window property)` in an `equal` table — ~10 allocations per window per
relayout in the steady state where *nothing changed*, and it forces `forget-window-state` to
walk the entire table to drop one window. `appearance.lisp:583` does three list allocations
over every leaf in the world to produce an integer, on every status-line draw. `layout.lisp:433`
uses `stable-sort` over a `copy-list` to do a two-bucket partition. `default-address` on a
split conses the entire index list to return `0`, on the hottest path in the model — two
lines on `sequential-container` removes it.

**And one that is not cheap and is not urgent: `request-manage` has no debounce.**
Every caller sends `manage_dirty` unconditionally, a request the wrapper's own docstring
calls *"a full manage round trip — it is not a frame clock."* River sends one `dimensions`
event per resized window; `windows.lisp:59` calls `mark-dirty` for each. **One real layout
change costs N+1 manage round trips.** The emit diff is what makes it converge instead of
oscillate. A `manage-requested` boolean cleared at `:manage-start` is six lines.

---

## Smaller things, stated once

> **31 of 33 LANDED, 2 LANDED DIFFERENTLY, 1 OPEN.** Thirty-three one-paragraph dispositions
> would bury the section they are about, so they are one table. The four that are not a plain
> **LANDED** carry a note at the item itself.
>
> | # | item | | where |
> |---|---|---|---|
> | 1 | `restart-wm` does not restart | LANDED | `f9bee4d` |
> | 2 | `RIVER` is one name for two types | LANDED | `eb6264e` |
> | 3 | `flake.nix` declares `licenses.bsd3` | LANDED | `75d5908` |
> | 4 | every source install ships 190 MB | LANDED | `d8ce0ac` |
> | 5 | `bootstrap.sh` unpinned, checksum on the wrong file | LANDED | `bdd00c4` |
> | 6 | the state file's version gate destroys the file | LANDED | `f9bee4d` |
> | 7 | `--eval` exits 0 on failure | LANDED | `f9bee4d` |
> | 8 | `motion-is-involutive` asserts a false property | LANDED | `025d14b` |
> | 9 | `with-example` leaks everything it loads | LANDED | `025d14b` |
> | 10 | the hand-copied `defclass` restoring `c:leaf` | LANDED | `025d14b` |
> | 11 | 21 lint tests whose loops cannot fail | LANDED | `025d14b` |
> | 12 | three exported functions with zero callers | LANDED | `f9bee4d` |
> | 13 | gate 5's 462 lines read by gate 5 | **LANDED, DIFFERENTLY** | `db81ce8` |
> | 14 | gate 3 holds old gate 6's metric | LANDED | `fd0f9b9` |
> | 15 | gates 15, 16, 17, 18 escapable by rename | LANDED | `81af94a` |
> | 16 | gate 12 `EVAL`s into the image seven gates read | LANDED | `9a4ece0` |
> | 17 | `gates.lisp` has no error containment | LANDED | `025d14b` |
> | 18 | `appearance.lisp` keeps a dead metric's shape | LANDED | `db81ce8` |
> | 19 | the keymap is not in `policy/` | LANDED | `db81ce8` |
> | 20 | `border-color`'s closed `cond` | LANDED | `d9fafd8` |
> | 21 | `*input-rules*` is written twice | **OPEN** | — |
> | 22 | `*lattice-border-parity*` superseded and alive | LANDED | `d9fafd8` |
> | 23 | `map.lisp` argues from hold-to-peek | LANDED | `e24ceea` |
> | 24 | `cursor-grid` inner, `cursor-cell` outer | LANDED | `f6b830a` |
> | 25 | `ASSESSMENT.org` contradicts the README | LANDED | `db81ce8` |
> | 26 | 83 dead org cross-file links | **LANDED, DIFFERENTLY** | `025d14b` |
> | 27 | `tools/bench.lisp` compiled by nothing | LANDED | `025d14b` |
> | 28 | `make -j check` races three SBCLs | LANDED | `abc6eb3` |
> | 29 | `:serial t` encodes a narrative | LANDED | `db81ce8` |
> | 30 | four functions inside a Python string | LANDED | `db81ce8` |
> | 31 | two help texts pinned to line numbers | LANDED | `abc6eb3`, `bdc16f3` |
> | 32 | the version in four places, tag matching none | LANDED | `58cd86a` |
> | 33 | `INSTALL.org`'s river table; roff asterisks | LANDED | `a8bfd2c`; `2026-08-11` |
>
> **On 13.** The 80 identity wrappers were not deleted, which is what this section asked for
> by implication. What landed instead is the discipline being made real: thirteen requests
> went round the wire layer and every one was a teardown, so eleven aliases were added, every
> call site moved, and **gate 22** refuses a request named outside `src/wire/` — while still
> allowing the protocol package to name an *interface*, which is how a global gets bound and
> has nothing to wrap. The circle this section found closing inside one file — `wm-manage-finish`
> saying *prefer `with-manage-sequence`* while `with-manage-sequence` sent `manage_finish` raw —
> is closed the other way now. The wrappers are cheap; the ungoverned call sites were the
> defect, and the report named the wrong one of the two.
>
> **On 26.** 17 links in the *current* documents were rewritten. The 67 in the frozen records
> were deliberately left: `DESIGN.org` and `PLAN.org` are append-only by ruling, and editing
> them to fix a link is the laundering that ruling exists to prevent. A dead link in a frozen
> record is a smaller cost than a record that can be edited.
>
> **On 33.** `INSTALL.org`'s table said `river 0.4+` twenty-five lines above a sentence saying
> exactly 0.4.6; it now says *Exactly* 0.4.6 in the table, and gained the two version
> constraints river itself pins and that nothing named — Zig 0.16, wlroots 0.20, both learned
> the slow way on Fedora. The roff bold was still literal four days later and is fixed now:
> `.B` on its own line, which is what every other emphasis in that file uses.

- **`restart-wm` does not restart.** `verbs.lisp:605`, bound to `Shift+Super+r`. It clears
  the running flag and exits; `install.sh:121` is `exec river -c latticewm`, which runs the
  client once. Nothing relaunches it. The user is left with their windows, no keymap, and a
  docstring saying *"Relaunch it and the layout comes back"* addressed to somebody who no
  longer has a key that opens a terminal. Recovery is a tty. Three lines fix it and both
  ingredients exist — `executable-directory` resolves argv0, `spawn` runs detached.

- **`nix-shell` + `make install` writes a session launcher that execs a directory.**
  `shell.nix:108` and `flake.nix:207` export `RIVER = "${pkgs.river}"` — the store
  *directory*. It has exactly two consumers: `session.lisp:63` uses it as a directory
  (`cp $RIVER/share/river-protocols/...`), correctly; `install.sh:49` does
  `river=${RIVER:-river}` and `:121` writes `exec $river -c "$bin/latticewm $*"` into
  `latticewm-session`. One variable, two incompatible types, and the mismatch lands in the
  file that runs at a login screen — where it fails with "Is a directory." Every check
  routes around it: `install-check.sh:42` and `flake.nix:159` both pass `--river` explicitly,
  so the env-var fallback is exercised by nothing. Two lines: export `RIVER_BIN` and read
  that. (`RIVER_VERSION`, exported by both nix files, is read by nothing at all.)

- **`flake.nix:164` declares `licenses.bsd3`.** `LICENSE` is GPLv3, 674 lines. Both `.asd`
  files say `"GPL-3.0-or-later"` — and `PLAN.org:1562` records fixing them: *"`:license` in
  both `.asd` files said `BSD-3-Clause` and was simply wrong."* Both were corrected; the
  third copy, in the packaging metadata that becomes what `nix search` shows every user, was
  not. Twenty lines of comment in the same file argue that a licence text must travel with a
  copy.

- **Every source install puts a 190 MB executable in `$PREFIX/bin`.** `Makefile:130` —
  `image: LATTICEWM_COMPRESS=0`. `Makefile:136` — `release:`, no such variable, which
  `:134` calls *"the shipping image: zstd-compressed, 13 MB against 190."* `install:`
  depends on `image`, and `INSTALL.org`'s canonical five-line install is `make image`.
  `make release` is referenced by zero documents, zero CI jobs and zero gates. The nix path
  gets 13 MB; everybody else gets 190. `install-check.sh` cannot see it — it checks
  existence.

- **`bootstrap.sh`'s central claim is false, and the checksum is on the wrong file.**
  Lines 17-21: *"Quicklisp's 2026-01-01 dist carries the same tree. So the two build paths
  are not merely equivalent, they compile the same source."* Line 172 calls
  `quicklisp-quickstart:install` with no dist pin — it takes whatever is newest on the day
  it runs. What *is* pinned is `QUICKLISP_SHA256`, on the bootstrapper, which the comment two
  lines above describes as byte-identical for years. **The checksum is on the file that
  cannot move and absent from the one that does**, so CI's `plain` job compiles a different
  wayflan from the `check` job on a schedule nobody controls. This is the same species as
  the `shell.nix` "pins nothing" finding the previous sweep closed, in the file it did not
  open.

- **The state file's version gate destroys the file it refuses.** `load-state` sees a wrong
  `+state-version+`, logs, returns nil — then `main.lisp:467` calls `mark-dirty`,
  `*last-save*` is still 0, and the first pass of the event loop writes `:if-exists
  :supersede` over it. Roll *forward* and you lose your layout once, which the docstring
  anticipates. Roll *back* — test a build, hit a bug, reinstall the packaged version — and
  the file is already gone. One line inside the `unless`: rename to `state.lisp.v~d` first.
  A format you invite people to hand-edit and then silently truncate on version skew
  punishes its own affordance.

- **`--eval` exits 0 on evaluation failure.** `ipc-evaluate` returns `T` whenever the socket
  was reachable; the wire protocol is `(:ok ...)`/`(:error ...)` and the client prints the
  distinction and discards it. `latticewm --eval '(car 5)'` prints an error and exits 0 —
  against `main.lisp:730`'s own contract that *"every branch that is not run the window
  manager exits with a status, because these are things a script may check."* `--eval` is
  the flag most likely to be in a script.

- **`motion-is-involutive` asserts a property at the one configuration where it holds, and
  the property is false.** `tests/test-motion.lisp:54` builds `make-split` with no weights,
  so every child is equal. `best-aligned-address` picks the child whose extent *contains*
  the centre of the rect you left. Give the columns weights `'(1 3)` and `'(3 1)`: from
  L-top, `:right` → centre 135 lies in R-top's 0..810 → R-top; from R-top, `:left` → centre
  405, L-top is 0..270 (distance 136) and L-bottom is 270..1080 (contains it) → **L-bottom.**
  Right-then-left does not return you. `defaults-motion.lisp:57` states it as a general
  guarantee: *"This is what makes Right-then-Left return you exactly where you started."*
  A generator over random weights finds this in under ten draws — and the suite already
  contains a generator, `divide-rect-tiles-exactly` (`test-geometry.lisp:28`), which is
  property-based testing written as nested `dolist`s. They know how. They did it once, in
  the 78-line file.

- **`with-example` documents the hazard and does not implement the remedy.**
  `tests/test-examples.lisp:39` promises to *"clean up after it… a test that does not undo
  them leaks into every test that runs afterwards"* and then rebinds two specials and calls
  `load`. No `remove-method`. `suite.lisp:78` runs examples *before* the lattice suite in
  the same image, so the whole plane suite runs with `*focus-follows-mouse*` set to `T` by
  `examples/01:27` and with example 02's `on-window-open` on `conventional-policy`,
  consulting a rule list that floats anything under 400×300 and routes `"firefox"`
  elsewhere. **It is correct today by string coincidence** — no fixture window is named
  `"firefox"` and `win` builds no preferred size. This is the identical defect the previous
  sweep found at `test-surface.lisp:603-608` and deleted, at four times the scale, in a file
  that sweep did not reach. Ten lines: record `generic-function-methods` before the load,
  `remove-method` the difference in an `unwind-protect`.

- **And one screen below its own obituary.** `test-surface.lisp:1007-1043` is now correct —
  `remove-method` and nothing else, under 24 lines explaining why the old restore was
  catastrophic. Then `:1045-1057` "restores" `c:leaf` by **hand-copying the shipped
  `defclass` into the test file**, and it is already lossy: `model/node.lisp:85-95` gives
  the slot and the class docstrings, both silently discarded for the rest of the image. Add
  a slot to `LEAF` and this deletes it at runtime, mid-suite, for every test that follows.
  The bodies matched last time too.

- **21 tests are lint rules, and at least 270 of their checks cannot fail.**
  `every-option-is-documented-and-has-a-default` cannot fail because `define-option` has
  `(check-type documentation string)` at macroexpansion and `default` is a required
  positional. `the-surface-is-what-takes-a-policy-and-nothing-else` is `(is (f x))` for every
  `x` in `(remove-if-not #'f universe)` — `policy-generics` is *defined* as the fixed point
  of `policy-generic-p`. Roughly **600 of the ~1,469 unit checks come from 21 tests**, and
  the loops that inflate the headline are exactly the ones that cannot fail. Add a 98th
  option and the number gains three checks and zero coverage. By *test* count the suite is
  91% behavioural, which is the honest headline and better than I expected; by the number
  the project reports, it inverts. One `is` over collected failures — the way
  `every-symbol-the-core-exports-names-something` already does it — says the same thing
  without the inflation.

- **Three exported functions with zero callers, one of which is a visible bug.**
  `output-of-window` (`server.lisp:390`, exported `package.lisp:516`) is called by nothing —
  which is why `emit.lisp:251` fullscreens using `current-output`, *the cursor's* output, so
  fullscreening a window on monitor 2 while the cursor is on monitor 1 fullscreens on
  monitor 1. `window-destroy` and `node-destroy` (`wrappers.lisp:250,263`) are called by
  nothing, and river's XML says *"The client should destroy this object… to free up
  resources"* — so **every closed window leaks a compositor object and a proxy for the life
  of the session.** `detach-output`, `detach-seat` and `forget-input-device` all destroy
  theirs; the teardown that runs most often is the one that forgot.

- **Gate 5 produced 462 lines of code whose only reader is gate 5.** `%request-class`
  returns `:any` for everything outside two lists, and an `:any` wrapper emits
  `(defun w:foo (a0 a1) (river:foo a0 a1))` — verbatim identity. 43 of 123 requests are
  classified; **the other 80 wrappers exist so `all-wrapped-requests`' count satisfies the
  gate.** And nothing goes through them: 19 raw `river:` calls in `runtime/` bypass the
  discipline entirely. The circle closes perfectly — `wrappers.lisp:191` defines
  `wm-manage-finish` with the docstring *"Prefer WITH-MANAGE-SEQUENCE"*, and
  `wire/sequence.lisp:100`, inside `with-manage-sequence` itself, calls
  `river:river-window-manager-v1.manage-finish` **raw.** The wrapper points at the macro and
  the macro bypasses the wrapper.

- **Gate 3 still contains the metric old gate 6 was executed for.** `*lattice-line-budget*`
  is 2,600 **raw** lines — `(loop for line = (read-line in nil) while line count t)`,
  comments and blanks included. `lattice/` sits at 2,142: 293 blank, 265 comment, 1,584 code.
  So the budget is measuring, at 26%, *how much the author explains himself*, and deleting
  comments buys headroom. It survives because the previous sweep was aimed at gate 6's
  **name** rather than at the class of defect. Four lines above it, a comment congratulates
  itself for having fixed exactly this species in the adjacent scan.

- **Gate 17's failure message documents how to escape gate 17.** Its population is options
  whose bare name matches a generic's, and the remedy it prints ends: *"If it is not,
  **rename one of them.**"* Rename the option and the gate enumerates nothing, prints
  `options named after a generic 0`, and passes — with the user in precisely the state the
  gate's preamble calls the failure. Gate 18 is the same shape and worse: its population is
  files literally named `defaults-*.lisp`, the previous sweep's merge took it from 2 to 1,
  and deleting the prefix from the last one makes it govern the empty set forever. Gate 15's
  `mentions` is `(string= (symbol-name form) name)` with no package check and no
  evaluation-position check, so `'(call-next-method)` quoted, or `(when nil
  (call-next-method))`, satisfies it. **Gate 16 Q2 treats space as a token boundary, and
  `src/package.lisp` exports 32 ordinary English words** — `window`, `close`, `float`,
  `focus`, `move`, `split`, `tab`, `tag`, `undo`, `key`, `node`, `world` — so any `.org`
  sentence containing "window" keeps the export `WINDOW` alive, and the *failing* half of
  the gate is structurally unreachable for roughly thirty published names.

- **Gate 12 `EVAL`s document-supplied forms into the image that gates 15, 16 and 17 then
  inspect.** `*read-eval*` is bound to `nil` for the **read**; nothing constrains the
  **eval**, and `claim-vocabulary-complaint` requires only that the form *name* a symbol the
  program owns. So `#+CLAIM: (progn (defmethod latticewm/policy:gaps ((p t) n c) 0) t)` in
  `README.org` passes both checks and **adds a method to a policy generic before three
  later gates read the method list.** The file's own ordering comment says gate 12 runs late
  so *"none of them should have to reason about what a claim left behind"* — and then asserts
  *"Gate 13 follows it and is the one gate that does not care."* True of 13. Not true of 15,
  16 or 17. A gate that can be told it passes, by a sentence in the README.

- **`gates.lisp` never received the fix `integration.lisp` wrote down.**
  `integration.lisp:131` — *"Run one section, and do not let it take the rest of the run with
  it"* — is the file's answer to having once been one 250-line `LET` that stopped growing at
  eighteen checks. `gates.lisp` is **eighteen bare top-level forms with no error containment
  between them**; gates 3, 7, 10, 14 and 18 do raw `with-open-file` with no handler, while
  gates 13 and 16 wrap theirs *precisely because "a file this cannot read is a gate that
  cannot run, not a gate that passes."* The reasoning was applied to two gates and not to
  the file. ~15 lines.

- **`appearance.lisp` opens with 34 lines confessing it exists because gate 6 was a
  line-count ratio.** Gate 6 was deleted three commits later. The file kept the shape the
  dead metric forced — 832 lines holding a 112-line string library, a 137-line font library
  and ~250 lines of actual appearance policy. The ratchet leaves residue after the pawl.

- **The keymap is not in `policy/`.** `commands.lisp:306-311` moved `*terminal*`, `*editor*`,
  `*browser*`, `*file-manager*` and `*modifier*` out of `runtime/config.lisp` on the
  argument that they are *"the five values a person is most likely to change on their first
  day"* — and left `install-default-keymap`, 97 lines of `define-key`, in that same file.
  The most-customised object in any window manager. Nothing notices, because gate 18
  polices a filename prefix.

- **`border-color` is a five-branch `cond` closed at three states by construction, forty
  lines from `font-for`, which got the same problem exactly right.** `font-for` puts the
  state in a *dispatch position* as a keyword, so `(defmethod font-for (policy (role (eql
  :map))) ...)` extends without copying, and its docstring celebrates that *"an extension can
  invent one."* `border-color` encodes `focusedp ∈ {T, :cursor, NIL}` in a `cond`, so a
  policy wanting a fourth border state — urgent, tagged, recording — copies the branch. The
  same file contains both patterns and chose the wrong one for the decision that runs per
  window per frame.

- **`*input-rules*` already is the tier-0 table, and `devices.lisp` writes it twice.**
  `option-settings` transcribes 15 special variables into the same `(matcher . plist)` shape
  the rules use, and then needs a six-boolean special case because `NIL` means both "off"
  and "leave alone" once you flatten options into a plist. A rule that does not mention a key
  does not set it, so the ambiguity cannot arise in the table form. Ship `*input-rules*` with
  a first entry of `(t :tap-to-click t ...)` and 22 globals become one.

  > **OPEN — declined at `e24ceea`, and the reason is recorded in `devices.lisp`'s header
  > rather than left to be rediscovered.** It works. What it costs is fifteen docstrings that
  > `--list-options` prints, the first line of the tier table, and **gate 11** — which
  > certifies that every option is read, from cross-references SBCL recorded, and would go
  > blind on all fifteen the moment they were reached through `symbol-value`. That is a real
  > objection and it is not a refutation: the diagnosis — one decision written twice, with a
  > six-boolean special case as the seam — stands, and nothing about the diagnosis was
  > answered.
  >
  > The bug underneath it was found and fixed on the way. `apply-device-settings` dropped
  > every pair whose value was false, so `(setf *tap-to-click* nil)` sent nothing and the
  > touchpad went on tapping. Everything above that line was right and said so out loud —
  > `option-settings` goes out of its way to send the six meaningful booleans either way and
  > explains why; two tests assert the plist carries `:natural-scroll nil` when a rule asks
  > for it. Every instrument agreed the plist was correct. It was. **The last reader of it
  > threw those pairs away, and no instrument looked there** — which is this report's claim,
  > found by going to fix something else.

- **`*lattice-border-parity*`'s own docstring says *"Superseded by `*COORDINATE-TINT*`"*** and
  the branch is kept alive in `border-color` to satisfy it. Gate 11 certifies it as read.
  A superseded knob surviving the build that exists to find dead knobs.

- **`map.lisp` argues for its own existence from a feature the core has already declared
  dead.** `map.lisp:20`: *"the map is free: nothing resizes to enter or leave it, so
  hold-to-peek is instant."* There is no hold-to-peek — zoom is bound to discrete presses.
  `wrappers.lisp:307` wraps `modifiers_watch` *"for hold-to-peek"* and `seats.lisp:57-63`
  records that the event *"never fired… Removed rather than wired up."* The core says the
  feature is dead; the map still argues from it in the present tense, six relayouts deep.

- **`cursor-grid` returns the innermost grid; `cursor-cell` ten lines below and `grid-path`
  return the outermost.** So `goto-cell` calls `ensure-cell` on the inner plane and
  `jump-cursor` to a path resolved against the outer one. `on-focus-change` gets nesting
  right by looping over every grid in the chain, which proves the author knew the shape and
  applied it in one of three places. `FINDINGS.org` lists plane-inside-a-plane among the
  things that came for free; the suite tests plane-inside-a-*split*.

- **`ASSESSMENT.org` is a 43 KB fossil whose headline contradicts the README.** One inbound
  prose link in the entire repo — `DESIGN.org:151`, which links to it in order to say it is
  *not* checked — and absent from README's own documentation list. It says the program is
  *"not ready to be somebody's daily driver"* against `README.org:698`'s *"Working, and used
  on bare metal."* The guard is one sentence at `:10` that has to carry 760 lines, including
  a §Verdict a stranger browsing the repo root hits first.

- **83 org cross-file links are dead on any forge**, six of them on `README.org` itself. The
  `[[file:X.org::#anchor]]` search-target is Emacs org-link syntax; GitHub's org-ruby
  resolves the whole string as a path. The README prints its own GitHub URL at `:49`.

- **`tools/bench.lisp` is compiled by nothing — the third live instance of the bug fixed
  three commits ago.** `tools/build.lisp:29`'s `*loose-files*` names only
  `hardware-check.lisp`; `make bench` has zero references outside the Makefile; no gate, no
  CI, no document. `3311f0b` is titled *"A file nothing compiled, for the second time."*
  This is the third. Its own header records that it measured the wrong half of the program
  for seven sessions. **`tools/test-lattice.lisp` is simply dead** — five lines referenced
  by nothing; `tools/test.lisp:15` already loads and runs `lattice/tests` and its header
  records the commit where it learned to.

- **`make -j check` races three SBCLs onto one fasl cache.** `test:` and `integration:`
  depend on `toolchain`, not on `build`, and `check: build gates test integration`. With
  `-j` all three `asdf:load-system` into the same `~/.cache/common-lisp` with no locking;
  `make -j install install-check` both write `./latticewm`. There is no `.NOTPARALLEL:`.
  One line. And CI has no `actions/cache` at all and no `concurrency:` block, so the `plain`
  job recompiles every dependency from cold on every push and two pushes run two full builds.

- **`:serial t` inside `runtime` makes file order be the dependency graph, and the file says
  so.** Editing `src/policy/protocol.lisp` — the published extension surface, third of
  sixteen — recompiles **14,838 lines, 78% of `src/`**. Editing `model/node.lisp`
  recompiles 17,709. Two of the ~twenty ordering comments explicitly disclaim being
  dependencies: *"the wrapper is consulted at call time — but reading them in this order is
  how the relationship is meant to be understood"* (`:157`), and *"reading them in this order
  is how the second is meant to be understood"* (`:182`). That is `:serial t` encoding a
  **narrative**. The genuine constraints, written as prose four lines away, are exactly the
  `:depends-on` graph that would have been machine-checked. The modules deserve `:serial t`;
  thirty files in a hardcoded total order over a demonstrable partial one is where the cost
  is.

- **`tools/psf-to-lisp.py` has four of the program's functions inside a Python string.**
  Lines 84-112 of `FONT_TEMPLATE` are `current-font`, `glyph-row`, `text-width`,
  `text-height` — the entry points every text-drawing path calls — compiled by nothing,
  checked by no gate. The two copies agree today. `src/runtime/font.lisp`'s header says
  "GENERATED", which is exactly the instruction that guarantees the next person edits the
  Python. Split it: `font-table.lisp` generated, `font.lisp` hand-written.

- **Two help texts pinned to line numbers, both already off by one, in opposite directions.**
  `install.sh:59` is `sed -n '2,40p' "$0"` and the header runs to line 41, so
  `./install.sh --help` ends mid-clause. `Makefile:221` is `sed -n '1,20p' Makefile` and the
  target list ends at 18, so `make help` spills `LISP ?= sbcl` into its output.

- **The version is in four places and one git tag, and no gate reads any of them.**
  `latticewm.asd:16`, `lattice.asd:22`, `flake.nix:53`, `doc/latticewm.1:1` all say `0.1.0`;
  the sole tag is `v0.1`, matching none of them. `main.lisp:761` gets it right — it reads
  `asdf:component-version` — and is the only place that does. Meanwhile `flake.nix:263` and
  `shell.nix:65` both read *river's* version out of `src/protocol/PINNED` with
  `builtins.match` rather than duplicating it. **The pattern is invented, used twice, and
  never turned inward on the file's own version string 200 lines above.** Gate 9's own
  comment (`gates.lisp:672`) says: *"a number repeated in two files is the FINDINGS §census
  failure one level up."* There is also no `CHANGELOG`, no `make dist`, and no `VERSION` —
  for a project whose stated requirement is surviving without its author, the one act it has
  no machinery for is handing a version to somebody else.

- **`INSTALL.org:26`** lists `river 0.4+` in the requirements table; `:51`, twenty-five lines
  later, says *"LatticeWM needs river 0.4.6. Not 0.4 or newer."* The table is what people
  read. And **`doc/latticewm-config.5:21`** renders `*This page is a map, not a reference.*`
  with literal asterisks — org bold syntax in a roff file, on the page's own thesis
  statement.

---

## What I tried to design better and couldn't

**The container protocol, and I came in expecting to find an OO reflex.** I designed two
replacements and both lose. A data-driven table (`kind → {addresses-fn, child-fn, …}`) is
the same dispatch with a worse type story and no method combination — and
`copy-node-slots`' `:most-specific-last progn` is load-bearing, not decoration:
`sequential-container` supplies addressing and `split` adds weights *after*, and a table has
no way to say "run the base contribution first." A single `walk` with a callback loses to
`child-at`/`insert-child` on the sparse case, because the grid has no children list to walk.
The evidence settles it: the grid answers six methods for an infinite plane, and
`examples/04` gets niri's scrolling strip in sixty lines with two overrides. I was wrong and
the measurement is the reason.

**CLOS generic dispatch as the variation mechanism, for a reason I did not anticipate.** A
policy struct of function slots is cheaper per call and makes the registry trivial — the
struct *is* the surface, and `policy-generic-p`, `policy-lineage-p`, `classp` and
`+policy-protocols+` all delete. Two things kill it. No `CALL-NEXT-METHOD`, so "extend the
shipped answer" becomes "close over the old function value," which is exactly the snapshot
trap `add-hook`'s docstring records biting this project — *"the help overlay drew itself
twice, once through each generation of the same function."* And, decisively: **the leverage
is on the second argument, not the first.** `gaps` for a `STACK` vs a `SPLIT` vs a `GRID`;
`layout-children` for three container kinds. A struct of functions gives you one axis of
variation and this program needs two. I also designed the `handler-bind` + restarts version —
the Lisp-native intervention primitive, and the only one that can express "use this layout
*for the duration of this operation*" — and it loses on discoverability, which is the
project's whole thesis: a `handler-bind` surface has no method list to print, so the
surface would be a maintained list of condition classes, which is the drift the generated
document exists to prevent. `policy-generics` works *because* CLOS keeps the registry for
you.

**`divide-rect`'s accumulated rounding** (`geometry.lisp:99-113`). One `reduce` and one
`round` per child, no per-child error, and the docstring is right that the naive version
"visibly frays after four or five splits." It is the shape the rest of the region should be
measured against, and I went looking for a cheaper one and there isn't.

**`poll-until`'s failure reporting** (`integration.lisp:149-186`). It keeps the last
signalled condition and prints it on timeout, under a docstring recording that swallowing
it cost a day and produced a whole section of `FINDINGS.org` about a window manager that
always quit. Twenty lines, and it is the difference between a flaky integration suite and a
useful one.

**`integration.lisp`'s hook ledger** (`:388-459`, `:1775-1807`). A recorder on every declared
hook, `+CANNOT-FIRE-HEADLESS+` checked in **both** directions — a hook that neither fired
nor is excused fails, *and* an excuse for a hook that no longer exists fails — with arity
checked against the declaration on the firings that actually happened. That second direction
is the thing nobody remembers to write, and it is the one place in this entire report where
an instrument checks that its own exemption list is still earned.

**`tools/prelude.lisp`.** Seventy lines that make "there are three legitimate ways for a
library to be present" true rather than aspirational, with the `missing-systems` check
running *before* quicklisp is loaded rather than after. I looked for the shorter version and
every one I wrote either loads quicklisp unconditionally or fails on a machine where the
libraries are already there. It is also, with `bootstrap.sh`, the reason "delete the flake
and the project still builds" is a command someone can run rather than a sentence.

**And the honest one about my own method: the gates region beat me on my own headline.** I
claimed a ratchet with no pawl. It showed me the pawl, dated and argued at seventy lines,
and then replaced my claim with a better one — that the gates are the project's only record
of six shipped bugs, so deleting one deletes the history. That is a structural reason for
what I had diagnosed as a habit, and it is more useful because it suggests a different
remedy: move the bug stories to `FINDINGS.org` *first*, and the gates become deletable
afterwards.

---

## The question the repo can't answer

Half of what makes a design wrong is the change it is about to face, and that is not in the
tree. **What is the next thing that gets built on this?**

- **A second extension, by somebody else** → claims 1 and 2 are the whole report, and the
  ranking is: print gate 6's number without `lattice/` (an afternoon), write the status-bar
  example (a day), add the vocabulary section (an afternoon), fix `enable` to compose (~30
  lines). Nothing else matters until a stranger can get off the ground.
- **Daily use, on real hardware** → claim 7 goes first, because a single-buffered shm surface
  under a compositor that reads whenever it likes is the defect whose symptom is
  intermittent and unreproducible, and the two arithmetic errors around it (33 MB not 4,
  damage computed and discarded) are in the same ten lines. Then `restart-wm`, then the state
  file's `:supersede`.
- **A public release** → the smaller things go first, and there are more of them than the
  claims. `restart-wm` is bound to a default key and does not do what it is named. The state
  file destroys itself on a downgrade. `--eval` exits 0 on failure. The nix-shell install
  writes a launcher that execs a directory. The flake tells `nix search` the licence is
  BSD-3 when it is GPLv3. Every source install ships 190 MB where the nix path ships 13.
  `/home/shaul` is in a committed document, and `config.lisp:337` tells every new user to
  connect to a port that is not listening. **And "what CI runs" is defined in three places
  that disagree**: `Makefile:11` claims `check` is it; CI runs `check` *and*
  `install-check`; the flake's `buildPhase` is a fourth hand-typed copy. A contributor who
  follows the Makefile's own instruction skips the only check in the project that asks what
  a user actually gets — which is the exact class the previous sweep spent its whole budget
  on. Two more lines: CI runs `nix build` and `nix flake check` never, and `nix build` is
  the only thing that has ever caught a river protocol break.
- **Nothing — the project is done, and the record is the deliverable** → then claim 3 is the
  only one that matters, and the two-line CI change (`make surface && git diff
  --exit-code doc/`) is the highest-value work in the report, because it is the difference
  between a record that is true and a record that was true when somebody last remembered.

I do not know the answer, and the ranking genuinely turns on it. What does not turn on it is
claim 1's first item: **print gate 6's number twice.** Ten lines, no behaviour change, and
until somebody sees the 7 sitting next to the 15, every other conversation about this
project's central claim is being had with the wrong number on the table.

---

## Postscript: the want, answered — and the re-ranking it forces

The question above was put to the author after this sweep closed. The answer, verbatim:

> *many design decisions here were made unawares, so your claims are even more important. in
> short, this needs to be feature rich, but also something the lisp community can rally
> around.*

That is a want I did not have while sweeping, and it is not one I would have reconstructed
from the tree — nothing in the repository is shaped for it. It changes the ranking
substantially and it kills one of my own remedies. Both are recorded here rather than
edited into the sections above, so the sweep stays what it was when it ran.

### What "made unawares" does to the claims above

It removes the hidden business context that a steelman exists to protect, so most claims get
stronger. Three of mine dissolve outright, and I withdraw the softening:

- **Claim 5, `guarded`.** I wrote that per-decision granularity inside a sequence that must
  complete "is the real justification, and it is not the one the docstring gives." If nobody
  chose it, there is no justification — only a marker that spread from "user code runs inside
  ours" to "anything that might signal." The remedy stands and the concession goes.
- **Claim 4, the two extension idioms.** I wrote that a slot-carrying policy class "is the
  right shape for the lattice specifically." If it is what got written first rather than what
  was chosen, then build the composable version and stop defending the incumbent.
- **Claim 1, gate 6's floors.** The *ratchet* was deliberate — the docstring argues for it at
  length. The **n = 1 consequence** plainly was not, which makes printing the number twice
  more urgent, not less.

Two rulings survive as genuinely deliberate and on the record: the mutation ruling at
`model/surgery.lisp:5`, and `:serial t`'s ordering comments, two of which say "reading order"
in so many words.

### The sentence that is now false

`swank.lisp:22`, defending SWANK off by default:

> **Nothing is lost by having it off.** The control socket in runtime/ipc.lisp is on by
> default and is reachable only by the user who owns the session, so scripting still works
> out of the box.

Airtight for scripting; wrong for the audience just named. A Lisper's whole relationship with
a running Lisp program is `M-x slime-connect`, `C-c C-c` on a `defmethod`, watch the window
move. That is the demo, it is the one thing no other Wayland compositor can offer, and it is
behind a flag that does not appear in the README's first code block. A Unix socket that takes
one form per line is a scripting interface; nobody rallies around a socket.

The security argument was correct about **unauthenticated TCP on a session binary** and was
never an argument against SLIME. `ipc.lisp` already solved the identical trust problem —
`$XDG_RUNTIME_DIR`, mode 0600, owner-only, chmod before listen — and SWANK can take the same
model.

**This reverses claim 3's remedy for `config.lisp:337`.** I said the `slime-connect to port
4005` line in the sample config was a stale falsehood to delete. It is the sentence that
should be made **true**. The finding stands; the fix inverts.

> **LANDED, DIFFERENTLY — `025d14b`. The Unix-socket SWANK is OPEN, and is written down at
> the point of decision.** The false sentence is gone: the starter configuration written into
> every user's home directory now names the three doors that *are* open and the one line that
> opens the fourth, in the paragraph a Lisper reads first. `*swank-port*`'s docstring concedes
> the argument in as many words — it used to say "nothing is lost" and that was wrong about
> the audience the project names two paragraphs earlier.
>
> What did not land is the part this section actually asked for: SWANK on `ipc.lisp`'s terms —
> `$XDG_RUNTIME_DIR`, mode 0600, owner-only, chmod before listen — which could then be **on by
> default** with none of the security argument applying. The docstring says why: `create-server`
> takes a port, so it is work in SWANK's socket layer rather than a flag here. That is a
> statement of cost, not a ruling, and it is recorded under the heading THE STANDING QUESTION
> IS A UNIX SOCKET so the next person meets it at the option rather than in a plan nobody
> reads. The default is still off, and "nobody rallies around a socket" is still true.
>
> **And on 2026-08-11 the premise got checked, which nobody had done.** I wrote that a
> Lisper's whole relationship with a running Lisp program is `M-x slime-connect`, and implied
> this project was behind the field on it. StumpWM — the program this one is most like —
> **ships no SWANK at all**: the word does not appear in its manual, and there is no swank
> module in `stumpwm-contrib`. Its manual's answer to *Interacting with the Lisp process* is
> `C-t :`, an eval prompt, which is exactly `Super+;`. SWANK lives on a wiki page and the
> instruction is to type `(ql:quickload :swank)` and `(swank:create-server)` **into that
> prompt, when you want it**.
>
> So the field's practice is on-demand, and this program was already a form ahead of it —
> `start-swank` is exported, so `Super+;` `(start-swank 4005)` is the whole thing, with no
> Quicklisp step. What was missing was anybody saying so: the starter configuration sent
> every new user to `(setf *swank-port* 4005)`, which needs a restart, and is the worse of
> the two paths this program ships. The one-form path is now the first thing the starter
> config says about SWANK and has a section of its own in `README.org`. **The finding was
> right that the door was invisible and wrong about which door it was.**

### The three barriers to the stated want, ranked

> **Barrier 1 — LANDED for the build, OPEN for the dist.** `bootstrap.sh` pins the Quicklisp
> dist to 2026-01-01 — the same tree the nixpkgs wayflan package pins, which is what makes
> "the two paths compile the same source" a mechanism rather than an intention — and the
> checksum moved onto the file that moves (`bdd00c4`). An SBCL floor is declared once and
> checked in three places (`a8bfd2c`). `make dist` and one `VERSION` exist (`58cd86a`). Both
> `.asd` files carry dist metadata, because Quicklisp, ocicl and qlot read a system definition
> and none of them read a README (`025d14b`). **What is still OPEN is the act itself:** the
> project is not *in* Quicklisp, ocicl or qlot. Everything that made submitting it possible
> landed; submitting it did not.
>
> **Barrier 2 — OPEN, and it is the one that gates claim 1.** Gate 6's floors come from
> `lattice/` alone now, so the examples are unfrozen and breadth *could* accumulate. There is
> still nowhere for it to accumulate: `latticewm-contrib` does not exist, and until it does
> the extension count stays at n = 1 for the reason StumpWM's would have.
>
> **Barrier 3 — LANDED — `025d14b`, `db81ce8`.** `CONTRIBUTING.md`, two issue templates and a
> PR template; `.github/` had held `workflows` and nothing else. That does not make twenty-two
> gates a smaller wall — it makes the wall documented, which is the difference between a
> contributor who gives up and one who knows what the build is asking for.

**1. Nobody can get in.** No Quicklisp, no ocicl, no qlot, no dist metadata — verified, the
repo has none. And `bootstrap.sh` *vendors Quicklisp in order to bootstrap itself*, which is
backwards for adoption. `git clone && ./bootstrap.sh && make` is a C project's install story
wearing Lisp. This gates everything else and it is about a day.

**2. There is nowhere for the second extension to live, and that is why it is n = 1.**
StumpWM's answer is `stumpwm-contrib`: a separate repo, a low bar, no gates. This project has
`examples/` — four files that claim 1 shows gate 6's zero-slack floors have **frozen**, so they
can be neither deleted nor merged. That is a fixture, not a commons. `latticewm-contrib` is the
missing artifact, and it resolves claim 1 as a side effect: source the floors from `lattice/`,
let breadth accumulate where breadth belongs.

**3. The gate regime and the community want are in direct conflict, and it is not close.**
This is the cost of the ratchet, priced in the currency that turns out to matter — and I
priced it in the wrong one the first time. A first contributor's PR must survive gate 6's
floors (zero slack: simplifying an example is a red build), gate 16 (no exporting a helper
before its caller, test or document exists), gate 10 (no `typecase` on a container class
anywhere in `src/`, including a debug printer), and gate 12 (every new `.org` classified inside
`gates.lisp`). Every failure message is correct, well-argued, and 855 lines of comment deep —
and there is no `CONTRIBUTING.md`, no issue template and no PR template to warn them.
`.github/` contains `workflows` and nothing else. **Eighteen gates is a superb instrument for
one author and a wall to the nineteenth.**

### Feature-rich is blocked by architecture, not by effort

Feature-richness in a window manager does not come from writing features. StumpWM's core is not
feature rich; `stumpwm-contrib` is. So **claim 4 is now the top structural item rather than the
fourth**: until `enable` installs a mixin over the current policy instead of replacing it,
there is no third-party ecosystem — only a queue into core, and every feature fights the last
one.

The four features a new user asks for first are already blocked, and the sweep found all four:

| want | blocker |
|---|---|
| per-output workspaces | `world` has one cursor and one root stack; the workaround already lives in `(prop output :workspace)` |
| a modal layer | `capture-wanted-p` (`seats.lisp:237`) is a hardcoded `or`, no generic — and for an Emacs-shaped WM this is request number one |
| urgency / tagged / recording borders | `border-color`'s `cond` is closed at three states by construction, forty lines from `font-for`, which solved it correctly |
| animations, fades, drag previews | no `wl_surface.frame` callback and no `wl_buffer.release` — claim 7, which the same fix closes |

> **Three of four LANDED; one OPEN, and it is the architectural one.**
>
> *A modal layer* — **LANDED, `cadb09d`**, and it was half-built in a way that looked whole.
> `capture-keys` had been made a policy generic; `capture-wanted-p` was a hardcoded `or` in
> `seats.lisp` with no method anywhere near it. So a modal layer could declare F1–F12
> readable, be handed two hundred bindings, see its generic listed in `--extension-surface`
> with its own method under it, and **never receive a key**. `p:capture-wanted-p` is that
> other half. It also fixed churn nobody had noticed: `arm-capture` compares the answer with
> `EQ` against the last one, and the old `or` could return the pending submap *object*, so two
> chords in a row disabled and re-enabled two hundred bindings to arrive back where they
> started — in the one path that runs before the compositor can process input.
>
> *Border states* — **LANDED, `d9fafd8`.** `border-state` says which state a border is in;
> `border-color-for` says what that state is drawn in, with the state in dispatch position.
> Two generics rather than one, because an extension that could only override the second would
> have no way to make anything say `:urgent`. A state nobody gave a colour gets the unfocused
> one rather than a `no-applicable-method` in the middle of a frame — which is `font-for`'s
> generosity and the half of the pattern that is easy to leave out.
>
> *Animations* — **LANDED, `7b49c07` and `2026-08-11`.** The buffer pool came first;
> `wl_surface.frame` is handled now, `draw-when-ready` paces a drawer to the compositor
> rather than to the event that asked, and the empty-pane outlines — which redraw on every
> relayout, including the ones a pointer drag produces — use it. The wait is bounded, because
> a surface that is occluded is never told to draw again and a redraw deferred behind that
> callback would wait forever.
>
> *Per-output workspaces* — **WITHDRAWN, and this row was wrong twice over.** I wrote that
> `world` has one cursor and one root stack and called the `(prop output :workspace)`
> mechanism a workaround. Both halves are rulings with arguments in the tree, and I had not
> read either.
>
> `output-content`'s docstring: *"PLAN.org's fiat rules multi-monitor as 'one model, one
> viewport per output', and this generic is that ruling. […] The index lives on the output's
> PROPS rather than in a slot, because which workspace a monitor is showing is exactly the
> kind of state that should not require a core class to grow a field — and because an
> extension that wants a different rule needs somewhere to put its own answer."* It is a
> *policy generic*, so per-output content is an extension point, which is more than most
> compositors offer. And it is finished work, not a stub: `show-workspace-on` as the single
> writer, `ensure-workspaces-for-outputs`, `rehome-orphaned-workspace` for an unplug,
> `restore-output-workspaces` keyed on the monitor's name so it survives a dock cycle.
>
> `show-workspace-on`'s docstring on the other half: *"the cursor is a place in a model
> rather than a consequence of which screen is active, and a command that moved it to another
> monitor would be answering a question nobody asked."*
>
> **What was actually missing was in the verbs, and this report did not find it: eighty
> commands and not one of them named an output.** No focus the other screen, no send this
> there, no swap. The only door was `workspace N`, which puts N on the screen you are already
> looking at and trades away what was there — a real workflow, and the only one. Landed
> `2026-08-11` as three additions and no model change, because the rulings do not obstruct
> the feature, they *define* it: the cursor is one place in one model, so crossing to the
> other monitor means going to the workspace that monitor displays. Motion crosses the screen
> boundary on its own for no key at all; `focus-output` and `send-to-output` are for when you
> do not want to walk; and `*remember-place*` is the piece that was genuinely absent, because
> landing on a workspace's first pane every time is what made crossing not worth doing.
>
> The lesson is the one this report keeps finding and did not apply to itself: **a mechanism
> that looks like a workaround may be a ruling nobody wrote down where you were looking.** It
> was written down. In the docstring of the generic.

### Two things to fix before any of the above

> **Both LANDED — `75d5908`, `db81ce8`.** The licence was corrected in the same commit that
> committed this report: `flake.nix` says `licenses.gpl3Plus`, which is what `nix search`
> prints and what a distribution packager reads.
>
> The front door: five `.org` files out of the repository root and into `doc/`, with all 140
> org links still resolving, and `ASSESSMENT.org` says at the top what it is rather than
> telling a visitor the program is not usable. That move immediately produced a bug worth the
> line — `install.sh` reused the source path for both ends of the copy, so an installed copy
> got `doc/doc/FINDINGS.org` — and `make install-check` caught it against a real prefix
> (`8970cf1`). The check that installs to a scratch prefix and asserts every artifact the
> documents promise is the one that noticed the documents had moved.

**`flake.nix:164` declares `licenses.bsd3` and `LICENSE` is GPLv3.** As a metadata nit it is a
one-line fix. As a community project it is what `nix search` shows, it stops a distro packager
cold, and it makes the licence of an incoming contribution genuinely ambiguous. Fix it today.

**The repo root is seven `.org` files and half a megabyte**, and `ASSESSMENT.org` — reachable
by no documented path — tells a visitor the program is *"not ready to be somebody's daily
driver"* against `README.org:698`'s *"Working, and used on bare metal."* That is the front
door. Move everything but `README` and `INSTALL` into `doc/`: nothing is deleted, the freeze
holds, and the entrance stops arguing with itself.

### And a second constraint, arriving with the first: *all* of Linux, not just nix

This is the same blind spot as the report's central claim, restated in the currency that
turns out to matter most: **every instrument in the build is pointed at the path the author
already stands on.**

**The only CI job that runs the installer runs under nix.** `.github/workflows/check.yml`'s
`plain` job — the one whose whole reason for existing is *"the same build with no nix at
all… because a test nobody runs is a sentence"* — is `./bootstrap.sh && make build gates
test`. It stops there. No `make image`, no `install.sh`, no `install-check`. Meanwhile the
`check` job carries the comment *"THE INSTALL IS PART OF `check' NOW, and it is the one thing
here that asks what a **user** gets rather than what the source says"* — and asks it under
`nix-shell`. **The path every non-nix Linux user takes is never run end to end by anything.**

**The program cannot be packaged by any distribution, for four independent reasons.**

| blocker | evidence |
|---|---|
| **No `DESTDIR`** | the string appears in neither `install.sh` nor `Makefile`. Arch, Debian, RPM, Gentoo, Alpine and Void all build with `make DESTDIR=$pkgdir install`; `--prefix` alone cannot express "bake `/usr` into the paths, write the files to a staging root" |
| **The installer writes into `$HOME`** | `install.sh:206-213` writes a starter `init.lisp` into `${XDG_CONFIG_HOME:-$home/.config}` at install time. Under the documented `sudo ./install.sh --prefix /usr/local` that lands in root's `.config`. No postinstall may touch a user's home. `--no-config` exists and is not the default when it needs to be |
| **No release artifact** | no `make dist`, no tarball, no `CHANGELOG`; the version is hand-written in four files and the sole tag `v0.1` matches none of them |
| **No SBCL floor** | `bootstrap.sh:110` prints `sbcl --version` and checks nothing, while `tools/image.lisp:36` pokes `sb-alien:extern-alien "gc_coalesce_string_literals"` — a symbol in SBCL's C runtime — gate 11 is built on `sb-introspect` xref, and `main.lisp:723` uses `sb-int:broken-pipe`. Debian and Ubuntu LTS ship SBCL 2.2.9; Arch ships current. Whether this builds on the older one is an empirical question nobody has asked |

**And the two things that are backwards.** `make install` depends on `image`, which is
`LATTICEWM_COMPRESS=0` — **190 MB.** `make release` produces the 13 MB image and is referenced
by zero documents, zero CI jobs, zero gates, so the nix path gets the good binary and every
other Linux user gets the 14× one. Likewise **the non-nix path is the *unpinned* one**: nix
users get a locked flake, everyone else gets `quicklisp-quickstart:install` with no dist pin,
under a `bootstrap.sh:17` comment claiming the two paths "compile the same source."

**The one that is genuinely hard, and is half right.** Pinning `river 0.4.6` exactly and
refusing rather than misbehaving is the correct call — a protocol that moves inside a patch
release makes anything else a silent corruption, and I would not overturn it. But under "all
of Linux" it means that on most distributions at most times the packaged river will not
match, so `INSTALL.org:95-121`'s four-step re-vendor recipe is not a footnote — **it is the
install path for the majority.** It ends *"Nothing about this needs the original author,"*
which is exactly right, and **nothing tests it.** Gate 5 asserts the version we bind matches
the XML we vendored; nothing has ever re-vendored against a *different* river and checked
that the recipe still works. That is a `plain`-job step.

**The change, in order.**

> **All seven LANDED.** This is the list the day was spent on, and it is the one the stated
> want turns on.
>
> 1. **`plain` goes all the way** — `e3e198c`. Image, install to a scratch prefix,
>    `install-check`, a staged `DESTDIR` install and `make dist`. A `flake` job runs
>    `nix build` and `nix flake check`, which had run never, and `nix build` is the only thing
>    that has ever caught a river protocol break. A `surface` job regenerates and diffs.
>    Plus `actions/cache` and a concurrency group, so two pushes in a minute stop racing two
>    cold builds. `plain` is also this project's only empirical answer to whether the declared
>    SBCL floor is real: it builds against whatever Ubuntu packages rather than what nixpkgs
>    pins.
> 2. **`DESTDIR`** — `bdc16f3`. Two sets of paths, and the difference *is* the feature: the
>    undecorated names are where the program will live and are what gets written *into* files;
>    the d-prefixed names are where the bytes go. `DESTDIR` implies `--no-config`, which was
>    already wrong without it — under this project's own documented
>    `sudo ./install.sh --prefix /usr/local` the starter `init.lisp` landed in *root's*
>    `.config`. `install-check.sh` asserts the one thing only the staged path can get wrong:
>    that the launcher and the session entry name the prefix and not the staging root.
> 3. **One image target** — `d8ce0ac`. The ship value is the default and `image-fast` is the
>    development one. `install-check.sh` grew the assertion that would have caught it: every
>    check in that file asked whether a file *exists*, which is this report's whole thesis, so
>    it could not see a binary fourteen times too large sitting at exactly the path it was
>    looking for.
> 4. **The Quicklisp dist is pinned** — `bdd00c4`, and the pin is *asserted* rather than
>    assumed: the installed `distinfo.txt` must name the dist that was asked for, because a
>    pin that silently did not take reads as reproducible and is not.
> 5. **The SBCL floor is declared and checked** — `a8bfd2c`. 2.2.6, and the reason is named
>    rather than guessed: core compression became zstd there and its levels became 0..22, and
>    `tools/image.lisp` dumps at 22 — so below the floor the build passes and `make image`
>    cannot, which leaves nothing to install.
> 6. **`make dist`, one version** — `58cd86a`, read from `VERSION` by both `.asd` files and by
>    `flake.nix`, with gate 19 holding the two roff pages to it and refusing a version literal
>    in `flake.nix`. `.gitattributes` pins LF, which is what makes `VERSION` a value rather
>    than nearly one — and matters more for the shell scripts, since a CRLF checkout fails with
>    `/bin/sh^M: bad interpreter`, which names neither the file that is wrong nor what is wrong
>    with it, on a contributor's first command.
> 7. **The re-vendor recipe is tested** — `db81ce8`. `make revendor-check` asserts that a river
>    we cannot speak to is refused *by name with both numbers*; `make revendor-check RIVER_SRC=…`
>    runs the four steps for real. CI runs the first.
>
> And the item that was not on the list and should have been: **`bootstrap.sh` failed on a
> stock Fedora and told the user to go read a backtrace** (`988d1b2`). One dependency grovels
> a C header, Fedora's gcc reads RPM hardening specs, and those specs live in a package a
> stock install does not have — so the first thing a Fedora user met was forty frames of SBCL
> naming neither this project nor the package. The script extracts the cause, says what it
> means where it knows, and names the package through the per-distro table it already had.
> That is what "all of Linux" costs, and it was found by running the non-nix path somewhere
> other than the author's machine for the first time.

1. **Make `plain` go all the way** — `make image install` into a scratch prefix, then
   `install-check`. Four lines, and it is the only way the majority path gets tested at all.
2. **`DESTDIR` support**, with `--no-config` implied when it is set. Without this no
   distribution ever ships it, and "the Lisp community rallies around it" and "it is in your
   package manager" are the same sentence.
3. **`make install` uses the compressed image.** One variable.
4. **Pin the Quicklisp dist** in `bootstrap.sh`, and move `QUICKLISP_SHA256` off the
   bootstrapper that never changes onto the dist that does.
5. **Declare and check an SBCL floor**, in `bootstrap.sh` and in `latticewm.asd`. Reaching
   for a named C symbol in the runtime is a version constraint whether or not it is written
   down.
6. **`make dist`, and one version read from the `.asd`** the way `main.lisp:761` already
   does — and the way `flake.nix:263` and `shell.nix:65` already read *river's* version out
   of `src/protocol/PINNED` rather than duplicating it. The pattern exists in this repository,
   twice, and was never turned inward on the project's own version string.
7. **Test the re-vendor recipe** against a river the program does not accept, and assert it
   either builds or fails with the honest message.
