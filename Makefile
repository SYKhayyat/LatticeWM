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
#   make image       dump ./latticewm, one executable
#   make run         run nested inside the current Wayland session
#   make surface     regenerate the generated documents under doc/
#   make install     ./latticewm, a session entry and a man page into $(PREFIX)

LISP        ?= sbcl
PREFIX      ?= $(HOME)/.local

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

.PHONY: all deps toolchain build gates test integration check image release bench \
        run run-bare surface config install uninstall clean distclean help

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
image: toolchain
	@LATTICEWM_COMPRESS=0 $(RUN) --load tools/image.lisp
	@ls -lh latticewm

# The shipping image: zstd-compressed, 13 MB against 190, for ~350 ms of
# one-time startup.
release: toolchain
	@$(RUN) --load tools/image.lisp
	@ls -lh latticewm

bench: toolchain
	@$(RUN) --load tools/bench.lisp

# River is a wlroots compositor and wlroots has a Wayland backend, so river
# runs *inside* the session you are already in, in a window.  That is the whole
# development loop: your editor on one side, river-in-a-window on the other,
# SLIME between them, and no second machine.
run: image
	@echo "starting river with LatticeWM inside the current session..."
	@river -log-level info -c "$(CURDIR)/latticewm --log-level debug"

# The same thing, but from a TTY, with river taking the whole screen.
run-bare: image
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

# install.sh does the rest — the session entry, the man page and the starter
# config — because those are the parts that differ between a system install
# and a home-directory one, and a shell script can ask.
install: image
	@./install.sh --prefix "$(PREFIX)"

uninstall:
	@./install.sh --uninstall --prefix "$(PREFIX)"

clean:
	@rm -f latticewm
	@rm -rf $(HOME)/.cache/common-lisp/*/$(CURDIR)

# Everything bootstrap.sh downloaded.  Separate from `clean' because it costs
# a download to undo and nobody expects `make clean' to do that.
distclean: clean
	@rm -rf .deps

help:
	@sed -n '1,20p' Makefile
