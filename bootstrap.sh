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
# dist carries the same tree.  So the two build paths are not merely
# equivalent, they compile the same source — which is why there is no second
# set of version numbers to keep in step and no vendored copy to go stale.

set -eu

root=$(cd "$(dirname "$0")" && pwd)
deps="$root/.deps"
quicklisp="$deps/quicklisp"

# The bootstrapper, not the library: this file has been byte-identical for
# years and is only the thing that installs the dist.  Override with
# QUICKLISP_SHA256= if upstream ever republishes it.
QUICKLISP_URL=${QUICKLISP_URL:-https://beta.quicklisp.org/quicklisp.lisp}
QUICKLISP_SHA256=${QUICKLISP_SHA256:-4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17}

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
If upstream republished the file, check it yourself and re-run with
  QUICKLISP_SHA256=$got ./bootstrap.sh"
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

# ------------------------------------------------------------- the compositor

# A warning rather than an error, deliberately: everything except actually
# running the window manager — the build, all six gates, all the unit tests —
# works with no compositor at all, and refusing to bootstrap on a machine that
# is only ever going to compile would be wrong.
if command -v river >/dev/null 2>&1; then
    version=$(river -version 2>/dev/null || echo unknown)
    say "river       $version"
    case "$version" in
        0.4.*|0.5.*|unknown) ;;
        *) say ""
           say "  WARNING: LatticeWM needs river 0.4 or newer.  It talks to"
           say "  river-window-management-v1, which does not exist before 0.4."
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
    fetch "$QUICKLISP_URL" "$deps/quicklisp.lisp"
    verify "$deps/quicklisp.lisp" "$QUICKLISP_SHA256"
    # --no-userinit so that a quicklisp the user already has in ~/ cannot
    # quietly take part; this tree is meant to be self-contained.
    sbcl --non-interactive --no-userinit \
         --load "$deps/quicklisp.lisp" \
         --eval "(quicklisp-quickstart:install :path \"$quicklisp/\")" \
         >"$deps/bootstrap.log" 2>&1 ||
        die "quicklisp install failed; see .deps/bootstrap.log"
    rm -f "$deps/quicklisp.lisp"
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
say "  make            build, six gates, the unit suite"
say "  make run        river nested inside this session"
say "  ./install.sh    install it, and offer it at the login screen"
