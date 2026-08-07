#!/bin/sh
# tools/install-check.sh --- install to a scratch prefix, check it, take it out.
#
# GATE 9 CHECKS THAT THE INSTALLER SAYS THE RIGHT THINGS.  This runs it.
#
# The gate can only read text, because the gates run before there is an image to
# install; its own comment has said so since it was written.  That was tolerable
# while the text and the install were the same size, and it stopped being
# tolerable when the project turned out to have *two* installers whose lists had
# drifted apart in six places -- none of which any check could see, because the
# only thing that ever ran either list end to end was a person installing.
#
# So: a scratch prefix, the real script, and an assertion per artifact.  It is
# the one check in the project that answers "what does a user actually get?"
# rather than "what does the source say they get?".
#
# IT CHECKS THE UNINSTALL TOO, and that is not symmetry for its own sake.  The
# removal list is a hand-written `rm -f' beside an install that grows, which is
# a list that stops being complete silently: latticewm-config.5 was already
# missing from it, so `--uninstall' left a man page behind on every machine that
# ever ran it.  Nothing could notice, because nobody uninstalls twice.
#
#   ./tools/install-check.sh              needs ./latticewm; `make image' first
#   make install-check                    builds the image, then runs this

set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

[ -x ./latticewm ] || {
    echo "install-check: no ./latticewm.  Run \`make image' first." >&2
    exit 1
}

prefix=$(mktemp -d "${TMPDIR:-/tmp}/latticewm-install-check.XXXXXX")
trap 'rm -rf "$prefix"' EXIT

# A prefix that is not under $HOME, so this exercises the system-install branch
# -- the one a package uses and the one nobody runs by hand.  --no-config keeps
# it out of the real home directory: this must not write an init.lisp.
river="/nowhere/bin/river-under-test"
./install.sh --prefix "$prefix" --no-config --river "$river" >/dev/null

failures=0
fail() { printf 'install-check: %s\n' "$*" >&2; failures=$((failures + 1)); }

present() {
    [ -e "$prefix/$1" ] || fail "missing after install: \$PREFIX/$1${2:+  ($2)}"
}

# Everything INSTALL.org promises, one line per row of its table.
present bin/latticewm                            "the binary"
present bin/latticewm-session                    "the launcher"
present share/wayland-sessions/latticewm.desktop "the session entry"
present share/man/man1/latticewm.1               "man latticewm"
present share/man/man5/latticewm-config.5        "man 5 latticewm-config"

# The lattice, which is the artifact gate 9 exists for: the sample configuration
# loads it by name and ASDF finds a system by finding its .asd on a path.
present share/latticewm/lattice.asd              "gate 9's own artifact"
present share/latticewm/lattice/policy.lisp      "the lattice sources"
present share/latticewm/examples                 "the worked examples"

# The generated references and the guide.  EXTENDING.org is here by name because
# it is the one document users needed and the one the install loop missed, back
# when the loop matched *.txt and the guide is an .org file.
present share/latticewm/doc/EXTENSION-SURFACE.txt
present share/latticewm/doc/CONTAINER-SURFACE.txt
present share/latticewm/doc/HOOKS.txt
present share/latticewm/doc/EXTENDING.org        "the extension guide"

# The licences.  src/runtime/font.lisp says the OFL text must travel with any
# copy of the font table and the binary is a copy of it, so this one is a
# condition on distributing at all rather than a document somebody might want.
present share/latticewm/doc/LICENSE              "ours"
present share/latticewm/doc/OFL-TERMINUS.txt     "the font's, and it is required"

# The pinned protocol, so a version mismatch is diagnosable from an installed
# copy.  The failure this answers happens at a login screen, where there is no
# source tree and no terminal.
present share/latticewm/protocol/river-window-management-v1.xml
present share/latticewm/protocol/PINNED

# The launcher has to name the binary that was installed and the river that was
# asked for -- not the build tree, and not whatever `river' resolves to at
# login.  This is the half of the install that a file-existence check cannot
# see: a launcher pointing at the wrong river fails at a login screen.
# AND HOW BIG IT IS, WHICH IS THE ONE PROPERTY EVERY CHECK HERE WAS BLIND TO.
#
# `image' used to force LATTICEWM_COMPRESS=0 and produce 190 MB while `release'
# took the default and produced 13, `install' depended on `image', and
# `release' was named by zero documents, zero CI jobs and zero gates.  So the
# nix path -- which runs tools/image.lisp with no variable at all -- shipped
# 13 MB and every source install on every other distribution shipped 190.
# Nothing could see it: every assertion above asks whether a file *exists*.
#
# The ceiling is generous on purpose.  It is not a size budget and it must not
# become one; it is the difference between a compressed image and an
# uncompressed one, which is fourteen times, and anything that trips it is a
# packaging accident rather than a few kilobytes of growth.
size_ceiling_mb=60
if [ -f "$prefix/bin/latticewm" ]; then
    size_mb=$(( $(wc -c < "$prefix/bin/latticewm") / 1048576 ))
    if [ "$size_mb" -gt "$size_ceiling_mb" ]; then
        fail "the installed binary is ${size_mb} MB, over the ${size_ceiling_mb} MB ceiling;
  that is the uncompressed image.  \`make image' compresses by default --
  something installed the output of \`make image-fast', or set
  LATTICEWM_COMPRESS=0 on the way to a prefix."
    fi
fi

if [ -f "$prefix/bin/latticewm-session" ]; then
    grep -q "$prefix/bin/latticewm" "$prefix/bin/latticewm-session" ||
        fail "the launcher does not run the installed binary"
    grep -q "$river" "$prefix/bin/latticewm-session" ||
        fail "the launcher ignores --river, so a pinned compositor does not stay pinned"
    [ -x "$prefix/bin/latticewm-session" ] || fail "the launcher is not executable"
fi

if [ -f "$prefix/share/wayland-sessions/latticewm.desktop" ]; then
    grep -q "^Exec=$prefix/bin/latticewm-session" \
         "$prefix/share/wayland-sessions/latticewm.desktop" ||
        fail "the session entry does not run the installed launcher"
fi

# And out again.  Anything left behind is a file the removal list forgot.
./install.sh --prefix "$prefix" --uninstall >/dev/null

leftover=$(find "$prefix" -type f 2>/dev/null || true)
if [ -n "$leftover" ]; then
    fail "uninstall left files behind:"
    printf '  %s\n' $leftover >&2
fi

# AND THE PACKAGING PATH, WHICH IS THE ONE NOBODY HERE HAS EVER RUN.
#
# Every distribution builds with `make DESTDIR=$pkgdir PREFIX=/usr install',
# and until now the string DESTDIR appeared nowhere in this project -- so the
# path that every packaged copy of this program would be built through was not
# merely untested, it did not exist.  The two things that can only be checked
# here are that the bytes land under the staging root, and that the paths
# *inside* the launcher and the session entry are the prefix and not the
# staging root.  Getting the second wrong produces a package that installs
# cleanly and fails at a login screen with a path that was correct on the build
# machine, which is the worst shape a packaging bug has.
staged=$(mktemp -d "${TMPDIR:-/tmp}/latticewm-destdir-check.XXXXXX")
trap 'rm -rf "$prefix" "$staged"' EXIT

fake_prefix=/usr
DESTDIR="$staged" ./install.sh --prefix "$fake_prefix" --river "$river" >/dev/null

[ -x "$staged$fake_prefix/bin/latticewm" ] ||
    fail "DESTDIR install put no binary at \$DESTDIR$fake_prefix/bin"
[ -e "$staged$fake_prefix/share/wayland-sessions/latticewm.desktop" ] ||
    fail "DESTDIR install wrote no session entry under the staging root"

if [ -f "$staged$fake_prefix/bin/latticewm-session" ]; then
    grep -q "^exec $river -c \"$fake_prefix/bin/latticewm" \
         "$staged$fake_prefix/bin/latticewm-session" ||
        fail "the staged launcher does not run \$PREFIX/bin/latticewm;
  it has baked the staging root into a path that will not exist on the
  machine the package is installed on"
fi

if [ -f "$staged$fake_prefix/share/wayland-sessions/latticewm.desktop" ]; then
    grep -q "^Exec=$fake_prefix/bin/latticewm-session\$" \
         "$staged$fake_prefix/share/wayland-sessions/latticewm.desktop" ||
        fail "the staged session entry does not Exec= the prefix path"
fi

# --no-config is implied, and this is the assertion that says so.  Under `sudo
# ./install.sh --prefix /usr/local' the starter init.lisp used to land in
# root's .config; under a packaging build it must land nowhere at all.
[ -z "$(find "$staged" -name init.lisp 2>/dev/null)" ] ||
    fail "the DESTDIR install wrote an init.lisp; --no-config is not implied"

DESTDIR="$staged" ./install.sh --prefix "$fake_prefix" --uninstall >/dev/null
staged_leftover=$(find "$staged" -type f 2>/dev/null || true)
if [ -n "$staged_leftover" ]; then
    fail "DESTDIR uninstall left files behind:"
    printf '  %s\n' $staged_leftover >&2
fi

if [ "$failures" -gt 0 ]; then
    printf '\ninstall-check: %d problem(s).  A packaged install is not what the\n' "$failures" >&2
    printf 'documents say it is, and gate 9 cannot see this from text alone.\n' >&2
    exit 1
fi

echo "install-check: a clean prefix gets everything, and gives it all back."
