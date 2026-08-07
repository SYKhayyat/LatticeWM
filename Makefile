# LatticeWM.
#
# Plain ASDF is the primary build.  Nix is a packaging target, not a
# dependency: nothing under src/ knows that nix exists, and if shell.nix is
# deleted the project still builds.  That is the test.
#
#   ./bootstrap.sh   fetch the dependencies (any Linux, no nix, once)
#   make             build and run the gates
#   make test        the unit suite
#   make integration a real river, headless, driven as a client
#   make check       all of the above -- what CI runs, and what to run before
#                    pushing.  Fails rather than skips when river or a terminal
#                    emulator is missing; REQUIRE_INTEGRATION=0 to forgive that
#   make image       dump ./latticewm, one executable -- compressed, 13 MB
#   make image-fast  the same, uncompressed: 190 MB, ~350 ms faster to start
#   make run         run nested inside the current Wayland session
#   make surface     regenerate the generated documents under doc/
#   make install     ./latticewm, a session entry and a man page into $(PREFIX)
#   make install-check  install to a scratch prefix, check it, take it out again
#   make dist        dist/latticewm-$(VERSION).tar.gz, from the git index

LISP        ?= sbcl
PREFIX      ?= $(HOME)/.local

# ONE VERSION, READ, NOT REPEATED.  It used to be written out in latticewm.asd,
# lattice.asd, flake.nix and the .TH line of both man pages, and the sole git
# tag was `v0.1' and matched none of them.  Both .asd files read this file with
# ASDF's :read-file-line, flake.nix reads it with builtins.match, and gate 19
# holds the man pages to it.  `sed 1q' rather than `cat' so a second line in
# the file is a comment rather than part of the version.
VERSION     := $(shell sed -n '1s/[[:space:]]*//gp' VERSION)

# A CONTRIBUTOR'S FIRST COMMAND SHOULD NOT BE THE ONE THAT FAILS.  `make test'
# used to end in `sbcl: command not found' with no further comment, including
# inside this project's own nix shell when it had not been entered — which is
# the single most likely state for somebody who has just cloned it.
#
# So: check for the compiler once, and if it is missing say the three things
# that actually help.  This costs one `command -v' per make invocation.
HAVE_LISP := $(shell command -v $(LISP) 2>/dev/null)
ifeq ($(HAVE_LISP),)
define MISSING_LISP

  $(LISP) is not on your PATH, so nothing here can build.

  LatticeWM needs SBCL.  Three ways, in order of least surprise:

    nix-shell                  this project pins SBCL *and* river
    sudo apt install sbcl      or dnf / pacman / zypper -- see INSTALL.org
    make LISP=/path/to/sbcl    if you have one somewhere else

  Everything else, including `make test' and `make gates', works once one
  of those is true.

endef
endif

# Three ways for the dependencies to be present, and the build cares about
# none of them: WAYFLAN_SRC is exported by shell.nix, ./.deps is what
# bootstrap.sh fills, and a distro package or a personal quicklisp is already
# on ASDF's path.  tools/prelude.lisp sorts out which and says so if none.
ifdef WAYFLAN_SRC
REGISTRY    := CL_SOURCE_REGISTRY="$(WAYFLAN_SRC)//:$(CURDIR)//"
else
REGISTRY    :=
endif
RUN         := $(REGISTRY) LATTICEWM_ROOT="$(CURDIR)" $(LISP) --noinform --non-interactive \
                 --load tools/prelude.lisp

.PHONY: all deps toolchain build gates test integration check image image-fast \
        release bench run run-bare surface config install install-check \
        uninstall dist clean distclean help

all: build gates test

# Every target that runs Lisp depends on this, so the diagnostic above is what
# a person without SBCL sees rather than a shell error from four levels down.
toolchain:
ifeq ($(HAVE_LISP),)
	@$(info $(MISSING_LISP))
	@exit 1
else
	@:
endif

# Idempotent, and skipped entirely when the dependencies are already visible —
# so this is safe to run first on a strange machine and free on a familiar one.
deps:
	@./bootstrap.sh

build: toolchain
	@$(RUN) --load tools/build.lisp

gates: build
	@$(RUN) --load tools/gates.lisp

test: toolchain
	@$(RUN) --load tools/test.lisp

# THE ONE TEST THAT RECEIVES STATE RATHER THAN CONSTRUCTING IT.  Runs river on
# a headless backend, connects to it as the ordinary Wayland client this
# program is, opens windows, drives the real commands, and asserts on what
# comes back.  No screen, no graphics card.
#
# It found a real bug on its first run — pointer bindings enabled through the
# wrong interface, silent, Super+drag doing nothing — and three more when it was
# grown to cover the runtime rather than the connection: a window that left the
# tree was never hidden, `new-workspace' moved the cursor somewhere it never put
# on screen, and op_start_pointer was sent outside the one sequence that may
# carry it.  That is the whole argument for it existing.
#
# Skips with a message when river is absent; set REQUIRE_INTEGRATION=1 to make
# that a failure instead.
REQUIRE_INTEGRATION ?=

integration: toolchain
	@LATTICEWM_REQUIRE_INTEGRATION="$(REQUIRE_INTEGRATION)" \
	  $(RUN) --load tools/integration.lisp

# What CI should run, and what to run before pushing.
#
# A GREEN `check' THAT VERIFIED NOTHING IS WORSE THAN A RED ONE.  This used to
# run `integration' without that variable, so on a machine with no river — which
# is the default state of a CI container — the one test that receives state
# printed a paragraph, exited 0, and the build was green.  Worse, three of the
# things this project's README says are verified on every run of `make check'
# were skipped when no terminal emulator was on PATH, which is also the default
# state of a CI container.
#
# So `check' demands them.  Target-specific variables are inherited by
# prerequisites, so this reaches `integration' without a second target.  Use
# `make integration' for the forgiving version, or `make check
# REQUIRE_INTEGRATION=0' if you want the old behaviour back and know why.
check: REQUIRE_INTEGRATION = 1
check: build gates test integration

# save-lisp-and-die does not cost live redefinition: the dumped core retains
# the compiler, so SWANK connects and DEFMETHOD still works at runtime.  This
# is StumpWM's shipping model and it is proven.
#
# THE SHIPPING IMAGE IS THE DEFAULT NOW, AND IT WAS THE EXCEPTION.  There were
# two targets for one artifact: `image' forced LATTICEWM_COMPRESS=0 and
# produced 190 MB, `release' took the default and produced 13.  `install'
# depended on `image', INSTALL.org's canonical five-line install is `make
# image', and `release' was referenced by zero documents, zero CI jobs and zero
# gates -- so the nix path, which runs tools/image.lisp with no variable at
# all, shipped 13 MB and every source install on every other distribution
# shipped 190.  A 14x difference decided by which of two targets a document
# happened to name.
#
# One target, one variable, and the ship value is the default.  The fast image
# is `make image-fast', which is what the development loop below uses; it is
# the same 350 ms of one-time startup, paid once per session by a program that
# then runs for weeks, against a rebuild you do fifty times an hour.
LATTICEWM_COMPRESS ?= 22

image: toolchain
	@LATTICEWM_COMPRESS=$(LATTICEWM_COMPRESS) $(RUN) --load tools/image.lisp
	@ls -lh latticewm

# Uncompressed, for the loop where the image is rebuilt constantly.  Not for
# installing: nothing here or in any document sends it to a prefix.
image-fast: LATTICEWM_COMPRESS = 0
image-fast: image

# `make release' was the name of the target that shipped the good binary, and
# nothing named it.  Kept as an alias so the name still works, and deliberately
# not as a second recipe, because a second recipe is how this started.
release: image

bench: toolchain
	@$(RUN) --load tools/bench.lisp

# River is a wlroots compositor and wlroots has a Wayland backend, so river
# runs *inside* the session you are already in, in a window.  That is the whole
# development loop: your editor on one side, river-in-a-window on the other,
# SLIME between them, and no second machine.
run: image-fast
	@echo "starting river with LatticeWM inside the current session..."
	@river -log-level info -c "$(CURDIR)/latticewm --log-level debug"

# The same thing, but from a TTY, with river taking the whole screen.
run-bare: image-fast
	@river -c "$(CURDIR)/latticewm"

# THIS TARGET USED TO TRUNCATE THE SHIPPED DOCUMENTATION AND EXIT 0.
#
# It was `$(RUN) ... > doc/X.txt 2>/dev/null'.  The shell truncates the target
# *before* the command runs and the diagnostic went nowhere, so if cli.lisp
# failed to load, the extension surface, the command list, the option list and
# the keymap all became empty files, make said nothing, and install.sh
# installed them.  A generated document cannot go stale, which is its whole
# argument -- but it can go blank, and blank is worse than stale because
# nothing about it looks wrong.
#
# So: write to a temporary, keep stderr, and only replace the shipped file when
# the command succeeded and produced something.
define GENERATE
	@printf '  %s\n' "$(2)"
	@$(RUN) --load tools/cli.lisp -- $(1) > $(2).new || \
	  { echo "make surface: $(1) failed; $(2) left alone" >&2; \
	    rm -f $(2).new; exit 1; }
	@test -s $(2).new || \
	  { echo "make surface: $(1) produced nothing; $(2) left alone" >&2; \
	    rm -f $(2).new; exit 1; }
	@mv $(2).new $(2)
endef

surface: build
	@mkdir -p doc
	$(call GENERATE,--extension-surface,doc/EXTENSION-SURFACE.txt)
	$(call GENERATE,--container-surface,doc/CONTAINER-SURFACE.txt)
	$(call GENERATE,--hooks,doc/HOOKS.txt)
	$(call GENERATE,--list-commands,doc/COMMANDS.txt)
	$(call GENERATE,--list-options,doc/OPTIONS.txt)
	$(call GENERATE,--list-keys,doc/KEYS.txt)
	@wc -l doc/EXTENSION-SURFACE.txt doc/CONTAINER-SURFACE.txt doc/HOOKS.txt \
		doc/COMMANDS.txt doc/OPTIONS.txt doc/KEYS.txt

config: image
	@./latticewm --write-config

# install.sh does the rest — the session entry, the man pages, the licences and
# the starter config — because those are the parts that differ between a system
# install and a home-directory one, and a shell script can ask.
#
# IT IS ALSO THE ONLY INSTALLER.  flake.nix runs this same script rather than
# keeping a second list of what ships; gate 9 checks that it still does.
install: image
	@./install.sh --prefix "$(PREFIX)"

uninstall:
	@./install.sh --uninstall --prefix "$(PREFIX)"

# THE CHECK THE GATES CANNOT MAKE.  Gate 9 reads install.sh and flake.nix as
# text, because the gates run before there is an image to install.  This runs
# the installer for real, against a scratch prefix, asserts every artifact the
# documents promise, and then asserts that uninstalling gives all of it back.
install-check: image
	@./tools/install-check.sh

# A RELEASE TARBALL, WHICH THIS PROJECT HAD NO WAY TO PRODUCE.
#
# Every distribution's packaging starts by fetching one, and the stated
# requirement is that this survive without its author -- so the one act it had
# no machinery for was handing a version to somebody else.  There was no `make
# dist', no CHANGELOG, no VERSION, and the sole tag matched none of the four
# places the version was written out.
#
# `git archive' rather than `tar', so what ships is exactly what is tracked:
# no .deps/, no fasl cache, no 190 MB image someone left in the tree, and no
# question about whether a file was included by accident.
#
# It refuses on a dirty tree and on a tag that disagrees, because a tarball
# whose contents are not the tag is the same defect as a version written in
# four places, one artifact further out.
dist:
	@test -n "$(VERSION)" || { echo "make dist: VERSION is empty" >&2; exit 1; }
	@git update-index -q --refresh
	@git diff-index --quiet HEAD -- || \
	  { echo "make dist: the tree is dirty; commit or stash first" >&2; exit 1; }
	@if git rev-parse -q --verify "refs/tags/v$(VERSION)" >/dev/null; then \
	   test "$$(git rev-parse "v$(VERSION)^{commit}")" = "$$(git rev-parse HEAD)" || \
	     { echo "make dist: tag v$(VERSION) exists and is not HEAD" >&2; exit 1; }; \
	 else \
	   echo "make dist: note -- no tag v$(VERSION) yet; see CHANGELOG.md" >&2; \
	 fi
	@mkdir -p dist
	@git archive --format=tar.gz --prefix="latticewm-$(VERSION)/" \
	  -o "dist/latticewm-$(VERSION).tar.gz" HEAD
	@ls -lh "dist/latticewm-$(VERSION).tar.gz"

clean:
	@rm -f latticewm
	@rm -rf dist
	@rm -rf $(HOME)/.cache/common-lisp/*/$(CURDIR)

# Everything bootstrap.sh downloaded.  Separate from `clean' because it costs
# a download to undo and nobody expects `make clean' to do that.
distclean: clean
	@rm -rf .deps

help:
	@sed -n '1,20p' Makefile
