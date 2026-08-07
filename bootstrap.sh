#!/bin/sh
# bootstrap.sh --- get LatticeWM's dependencies on any Linux, without nix.
#
# PLAN.org §packaging: "Plain Linux is the primary build.  Nix is a packaging
# target, not a dependency: nothing under src/ knows that nix exists, and if
# shell.nix is deleted the project still builds.  That is the test."
#
# This script is that test made runnable.  It needs SBCL and an internet
# connection, and it puts everything else under ./.deps/ — nothing is written
# outside this directory, nothing is installed system-wide, and deleting
# .deps/ undoes all of it.
#
#   ./bootstrap.sh          fetch what is missing
#   ./bootstrap.sh --check  say what is missing and stop
#   make                    build, gates, tests
#
# WHY QUICKLISP.  The nix build takes wayflan's source from the nixpkgs
# package, which pins the sr.ht tree at 2026-01-01.  Quicklisp's 2026-01-01
# dist carries the same tree.  So the two build paths compile the same source
# — which is why there is no second set of version numbers to keep in step and
# no vendored copy to go stale.
#
# THAT SENTENCE WAS FALSE FOR THE WHOLE LIFE OF THIS FILE, AND THIS IS THE FIX.
# `quicklisp-quickstart:install' with no :dist-version takes whatever dist is
# newest on the day it runs, so the claim above described an intention rather
# than a mechanism, and the CI job whose entire purpose is "the same build with
# no nix at all" compiled a different wayflan from the nix job on a schedule
# nobody controlled.  QUICKLISP_DIST below is what makes it true.
#
# And the checksum was on the wrong file.  QUICKLISP_SHA256 pinned
# quicklisp.lisp, the *bootstrapper*, which the comment beside it correctly
# describes as byte-identical for years — a checksum on the one thing that
# cannot move, and none at all on the thing that moves monthly.  Both are
# pinned now; the bootstrapper's checksum stays because it costs nothing and a
# republished bootstrapper is still worth noticing.
#
# The nix path was the pinned one and the plain path was not, which is exactly
# backwards for a project whose primary build is plain Linux.

set -eu

root=$(cd "$(dirname "$0")" && pwd)
deps="$root/.deps"
quicklisp="$deps/quicklisp"

# The bootstrapper, not the library: this file has been byte-identical for
# years and is only the thing that installs the dist.  Override with
# QUICKLISP_SHA256= if upstream ever republishes it.
QUICKLISP_URL=${QUICKLISP_URL:-https://beta.quicklisp.org/quicklisp.lisp}
QUICKLISP_SHA256=${QUICKLISP_SHA256:-4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17}

# THE DIST, WHICH IS THE THING THAT ACTUALLY MOVES.  A quicklisp dist is dated
# and immutable, so naming one is the whole of reproducibility here: it fixes
# every library version at once, including wayflan's, which is the one the
# header's "the two build paths compile the same source" claim rests on.
#
# 2026-01-01 is the same tree the nixpkgs wayflan package pins, which is what
# makes the claim true rather than aspirational.  Moving it is a deliberate
# act, like `nix flake update': change the date, run `make check', and if the
# protocol moved underneath you the build says so instead of a user's login
# screen doing it.
#
# QUICKLISP_DIST= overrides, and QUICKLISP_DIST=latest opts out entirely for
# anyone who wants to find out early what the next dist does to this build.
QUICKLISP_DIST=${QUICKLISP_DIST:-2026-01-01}
# http, not https, and this is quicklisp's constraint rather than a choice:
# QL-HTTP is a hand-rolled socket client that answers "Unknown scheme" to
# anything else, and `quicklisp-quickstart:install' has always fetched over
# http for the same reason.  QUICKLISP_DIST_SHA256 below is what makes that
# acceptable -- the dist is verified after it lands.
QUICKLISP_DIST_URL=${QUICKLISP_DIST_URL:-http://beta.quicklisp.org/dist/quicklisp/$QUICKLISP_DIST/distinfo.txt}
# THE CHECKSUM ON THE FILE THAT ACTUALLY MOVES.  QUICKLISP_SHA256 above pins
# the *bootstrapper*, which the comment beside it correctly says has been
# byte-identical for years; this pins the dist, which is republished monthly
# and is what decides every library version in the build.  A dated quicklisp
# dist is immutable upstream, so a mismatch means the file was replaced or
# tampered with, and either is worth stopping for.
QUICKLISP_DIST_SHA256=${QUICKLISP_DIST_SHA256:-fc436eb7582c8b69f2ec87d77bdaa4087502328dec09f21c576d78fac8bf5e15}

# Every system latticewm.asd names, plus the two the build harness uses.
SYSTEMS="wayflan-client alexandria closer-mop bordeaux-threads fiveam swank"

check_only=no
[ "${1:-}" = "--check" ] && check_only=yes

say() { printf '%s\n' "$*"; }
die() { printf '\nbootstrap: %s\n' "$*" >&2; exit 1; }

# --------------------------------------------------------------- the distro

distro_id() {
    if [ -r /etc/os-release ]; then
        # shellcheck disable=SC1091
        . /etc/os-release
        printf '%s %s' "${ID:-}" "${ID_LIKE:-}"
    fi
}

# Package names differ per distro and getting them wrong is worse than not
# guessing, so this only speaks up for distros whose names are known.
install_hint() {
    package=$1
    case "$(distro_id)" in
        *arch*|*manjaro*|*endeavouros*) say "  sudo pacman -S $package" ;;
        *debian*|*ubuntu*|*mint*|*pop*) say "  sudo apt install $package" ;;
        *fedora*|*rhel*|*centos*)       say "  sudo dnf install $package" ;;
        *suse*)                         say "  sudo zypper install $package" ;;
        *alpine*)                       say "  sudo apk add $package" ;;
        *void*)                         say "  sudo xbps-install -S $package" ;;
        *gentoo*)                       say "  sudo emerge $package" ;;
        *nixos*)                        say "  nix-shell            # shell.nix has everything" ;;
        *)                              say "  install '$package' with your package manager" ;;
    esac
}

fetch() {
    url=$1; out=$2
    if command -v curl >/dev/null 2>&1; then
        curl -fsSL "$url" -o "$out"
    elif command -v wget >/dev/null 2>&1; then
        wget -q "$url" -O "$out"
    else
        die "neither curl nor wget is installed, and one of them is needed once."
    fi
}

verify() {
    file=$1; want=$2
    if command -v sha256sum >/dev/null 2>&1; then
        got=$(sha256sum "$file" | cut -d' ' -f1)
    elif command -v shasum >/dev/null 2>&1; then
        got=$(shasum -a 256 "$file" | cut -d' ' -f1)
    else
        say "  (no sha256sum; skipping the checksum)"
        return 0
    fi
    [ "$got" = "$want" ] && return 0
    die "checksum mismatch on $file
  expected $want
  got      $got
A dated quicklisp dist is immutable upstream, so this means the file was
republished or replaced.  Check it yourself, then re-run with whichever of
  QUICKLISP_SHA256=$got ./bootstrap.sh        # the bootstrapper
  QUICKLISP_DIST_SHA256=$got ./bootstrap.sh   # the dist
names the file above."
}

# ------------------------------------------------------------- the compiler

say "LatticeWM bootstrap"
say ""

if ! command -v sbcl >/dev/null 2>&1; then
    say "SBCL is not installed.  It is the only hard requirement:"
    install_hint sbcl
    die "no sbcl."
fi
say "sbcl        $(sbcl --version)"

# AND HOW OLD IT MAY BE, WHICH THIS USED TO PRINT AND NOT CHECK.
#
# "Needs SBCL" was never the whole requirement.  tools/image.lisp dumps the
# shipping image at zstd level 22, and SBCL used zlib with a maximum of 9
# before 2.2.6 -- so on an older one the build can succeed and `make image'
# cannot, which means there is nothing to install.  Debian and Ubuntu LTS ship
# 2.2.9 and Arch ships current, so the spread is real; this used to print the
# version and move on, and whether it built on the older one was an empirical
# question nobody had asked.
#
# The floor is READ OUT OF latticewm.asd rather than written here, the same way
# the river warning above reads the release out of src/protocol/PINNED, and for
# the same reason: a second copy is a thing that can come to disagree with the
# check the program actually performs.
sbcl_floor=$(sed -n 's/^(defparameter cl-user::\*latticewm-minimum-sbcl\* "\([0-9.]*\)".*/\1/p' \
                 "$root/latticewm.asd" 2>/dev/null || true)
sbcl_have=$(sbcl --version 2>/dev/null | sed -n 's/^SBCL \([0-9][0-9.]*\).*/\1/p')

if [ -n "$sbcl_floor" ] && [ -n "$sbcl_have" ]; then
    # `sort -V' is in GNU coreutils, busybox and toybox, which is every Linux
    # this is meant to run on.  If it is somehow absent the check is skipped
    # rather than guessed at -- latticewm.asd checks again from inside Lisp.
    if command -v sort >/dev/null 2>&1 &&
       [ "$(printf '%s\n%s\n' "$sbcl_floor" "$sbcl_have" | sort -V 2>/dev/null | head -1)" \
         != "$sbcl_floor" ]; then
        say ""
        say "  This SBCL is $sbcl_have and LatticeWM needs $sbcl_floor or newer."
        say ""
        say "  The binding constraint is core compression: the shipping image"
        say "  is dumped at zstd level 22, and SBCL used zlib with a maximum"
        say "  of 9 before $sbcl_floor.  'make build gates test' may well work"
        say "  here; 'make image' cannot, so there would be nothing to install."
        say ""
        install_hint sbcl
        die "sbcl $sbcl_have is older than $sbcl_floor."
    fi
fi

# ------------------------------------------------------------- the compositor

# A warning rather than an error, deliberately: everything except actually
# running the window manager — the build, all twenty-two gates, all the unit tests —
# works with no compositor at all, and refusing to bootstrap on a machine that
# is only ever going to compile would be wrong.
#
# THE VERSION IT WANTS IS READ FROM src/protocol/PINNED, not written here.
# river-window-management-v1 changes *within* a version number — 0.4.6 raised
# it from 4 to 5 in a patch release — so the startup check is an equality and
# "0.4 or newer" is the wrong sentence to print.  Keeping the release name in
# one file means this warning cannot drift from what the program will actually
# accept, which is exactly how it drifted before.
if command -v river >/dev/null 2>&1; then
    version=$(river -version 2>/dev/null || echo unknown)
    say "river       $version"
    # `river -version' answers "0.4.6 +xwayland": the build flags are part of
    # the line and not part of the version, and comparing the whole line
    # against the release name warned on every machine including the correct
    # ones, which is the fastest way to teach someone to ignore a warning.
    found=${version%% *}
    pinned=$(sed -n '1s/^river \([0-9][0-9.]*\).*/\1/p' \
                 "$root/src/protocol/PINNED" 2>/dev/null || true)
    case "$found" in
        unknown) ;;
        "${pinned:-$found}") ;;
        *) say ""
           say "  WARNING: this river is $found, and the protocol in"
           say "  src/protocol/ was vendored from river ${pinned:-0.4.6}."
           say "  LatticeWM checks the interface version at startup and"
           say "  refuses to run on a mismatch rather than misbehave -- river"
           say "  changes that protocol within a version number.  INSTALL.org,"
           say "  'Moving to a newer river', is four steps and needs nobody."
           ;;
    esac
else
    say "river       not found -- the build will work; running it will not."
    install_hint river
fi

if [ "$check_only" = yes ]; then
    say ""
    [ -f "$quicklisp/setup.lisp" ] && say "deps        .deps/quicklisp is installed" \
                                   || say "deps        missing -- run ./bootstrap.sh"
    exit 0
fi

# ---------------------------------------------------------------- quicklisp

mkdir -p "$deps"

if [ -f "$quicklisp/setup.lisp" ]; then
    say "quicklisp   already in .deps/quicklisp"
else
    say ""
    say "fetching quicklisp into .deps/quicklisp ..."
    say "  dist $QUICKLISP_DIST"
    fetch "$QUICKLISP_URL" "$deps/quicklisp.lisp"
    verify "$deps/quicklisp.lisp" "$QUICKLISP_SHA256"
    # THE DIST VERSION IS THE PIN.  Without :dist-version this took whatever
    # was newest on the day it ran, which made the header's claim that the two
    # build paths compile the same source an intention rather than a
    # mechanism, and made CI's `plain' job -- the one whose whole purpose is
    # the no-nix build -- compile a different wayflan from the nix job on a
    # schedule nobody controlled.
    #
    # --no-userinit so that a quicklisp the user already has in ~/ cannot
    # quietly take part; this tree is meant to be self-contained.
    #
    # :dist-url and not :dist-version, which is a trap: the latter builds
    # `.../dist/<version>/distinfo.txt' and the real path has the dist *name*
    # in it too, so it 404s and quicklisp reports it as an unhandled condition
    # twenty-eight frames deep.  The canonical URL is the one distinfo.txt
    # prints about itself.
    if [ "$QUICKLISP_DIST" = latest ]; then
        pin=""
        say "  (unpinned: QUICKLISP_DIST=latest)"
    else
        pin=":dist-url \"$QUICKLISP_DIST_URL\""
    fi
    sbcl --non-interactive --no-userinit \
         --load "$deps/quicklisp.lisp" \
         --eval "(quicklisp-quickstart:install :path \"$quicklisp/\" $pin)" \
         >"$deps/bootstrap.log" 2>&1 ||
        die "quicklisp install failed; see .deps/bootstrap.log.
If it names the dist, $QUICKLISP_DIST may have been withdrawn upstream --
pick another from https://beta.quicklisp.org/dist/quicklisp.txt and set
  QUICKLISP_DIST=YYYY-MM-DD ./bootstrap.sh
Changing the pin is a deliberate act: run \`make check' after it."
    rm -f "$deps/quicklisp.lisp"

    # Say back what was actually installed, because a pin that silently did
    # not take is worse than no pin: it reads as reproducible and is not.
    distinfo="$quicklisp/dists/quicklisp/distinfo.txt"
    installed=$(sed -n 's/^version: *\([0-9][0-9-]*\).*/\1/p' "$distinfo" \
                    2>/dev/null | head -1)
    [ -n "$installed" ] && say "  installed dist $installed"
    if [ "$QUICKLISP_DIST" != latest ]; then
        [ "$installed" = "$QUICKLISP_DIST" ] ||
            die "asked for quicklisp dist $QUICKLISP_DIST and got ${installed:-nothing}."
        # And the bytes, not only the label.  The fetch above is http because
        # quicklisp's own client speaks nothing else; this is what makes that
        # all right, and it is the check that used to be pointed at the file
        # that cannot move instead of the one that does.
        verify "$distinfo" "$QUICKLISP_DIST_SHA256"
    fi
fi

# ------------------------------------------------------------- the systems

say ""
say "fetching the lisp dependencies ..."
sbcl --non-interactive --no-userinit \
     --load "$quicklisp/setup.lisp" \
     --eval "(ql:quickload (list $(for s in $SYSTEMS; do printf '"%s" ' "$s"; done)))" \
     >>"$deps/bootstrap.log" 2>&1 ||
    die "could not fetch the dependencies; see .deps/bootstrap.log"

for s in $SYSTEMS; do say "  $s"; done

say ""
say "done.  Next:"
say ""
say "  make            build, twenty-two gates, the unit suite"
say "  make run        river nested inside this session"
say "  ./install.sh    install it, and offer it at the login screen"
