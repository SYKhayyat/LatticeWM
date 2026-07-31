# LatticeWM.
#
# Plain ASDF is the primary build.  Nix is a packaging target, not a
# dependency: nothing under src/ knows that nix exists, and if shell.nix is
# deleted the project still builds.  That is the test.
#
#   make            build and run the gates
#   make test       the unit suite
#   make image      dump ./latticewm, one executable
#   make run        run nested inside the current Wayland session
#   make surface    regenerate doc/EXTENSION-SURFACE.txt
#   make install    ./latticewm plus a sample config into $(PREFIX)

LISP        ?= sbcl
PREFIX      ?= $(HOME)/.local
REGISTRY    := CL_SOURCE_REGISTRY="$(WAYFLAN_SRC)//:$(CURDIR)//"
RUN         := $(REGISTRY) $(LISP) --non-interactive

.PHONY: all build gates test image release bench run run-bare surface config install clean help

all: build gates test

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

install: image
	@install -Dm755 latticewm $(PREFIX)/bin/latticewm
	@install -Dm644 doc/EXTENSION-SURFACE.txt \
		$(PREFIX)/share/latticewm/EXTENSION-SURFACE.txt
	@echo "installed $(PREFIX)/bin/latticewm"

clean:
	@rm -f latticewm
	@rm -rf $(HOME)/.cache/common-lisp/*/$(CURDIR)

help:
	@sed -n '1,15p' Makefile
