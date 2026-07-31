# LatticeWM.
#
# Plain ASDF is the primary build.  Nix is a packaging target, not a
# dependency: nothing under src/ knows that nix exists, and if shell.nix is
# deleted the project still builds.  That is the test.
#
#   ./bootstrap.sh  fetch the dependencies (any Linux, no nix, once)
#   make            build and run the gates
#   make test       the unit suite
#   make image      dump ./latticewm, one executable
#   make run        run nested inside the current Wayland session
#   make surface    regenerate doc/EXTENSION-SURFACE.txt
#   make install    ./latticewm, a session entry and a man page into $(PREFIX)

LISP        ?= sbcl
PREFIX      ?= $(HOME)/.local

# Three ways for the dependencies to be present, and the build cares about
# none of them: WAYFLAN_SRC is exported by shell.nix, ./.deps is what
# bootstrap.sh fills, and a distro package or a personal quicklisp is already
# on ASDF's path.  tools/prelude.lisp sorts out which and says so if none.
ifdef WAYFLAN_SRC
REGISTRY    := CL_SOURCE_REGISTRY="$(WAYFLAN_SRC)//:$(CURDIR)//"
else
REGISTRY    :=
endif
RUN         := $(REGISTRY) LATTICEWM_ROOT="$(CURDIR)" $(LISP) --non-interactive \
                 --load tools/prelude.lisp

.PHONY: all deps build gates test image release bench run run-bare surface config install uninstall clean distclean help

all: build gates test

# Idempotent, and skipped entirely when the dependencies are already visible —
# so this is safe to run first on a strange machine and free on a familiar one.
deps:
	@./bootstrap.sh

build:
	@$(RUN) --load tools/build.lisp

gates: build
	@$(RUN) --load tools/gates.lisp

test:
	@$(RUN) --load tools/test.lisp

# save-lisp-and-die does not cost live redefinition: the dumped core retains
# the compiler, so SWANK connects and DEFMETHOD still works at runtime.  This
# is StumpWM's shipping model and it is proven.
image:
	@LATTICEWM_COMPRESS=0 $(RUN) --load tools/image.lisp
	@ls -lh latticewm

# The shipping image: zstd-compressed, 13 MB against 190, for ~350 ms of
# one-time startup.
release:
	@$(RUN) --load tools/image.lisp
	@ls -lh latticewm

bench:
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

surface: build
	@mkdir -p doc
	@$(RUN) --load tools/cli.lisp -- --extension-surface \
		> doc/EXTENSION-SURFACE.txt 2>/dev/null
	@$(RUN) --load tools/cli.lisp -- --list-commands \
		> doc/COMMANDS.txt 2>/dev/null
	@$(RUN) --load tools/cli.lisp -- --list-options \
		> doc/OPTIONS.txt 2>/dev/null
	@$(RUN) --load tools/cli.lisp -- --list-keys \
		> doc/KEYS.txt 2>/dev/null
	@wc -l doc/EXTENSION-SURFACE.txt doc/COMMANDS.txt doc/OPTIONS.txt doc/KEYS.txt

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
	@sed -n '1,16p' Makefile
