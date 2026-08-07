#!/bin/sh
# install.sh --- put LatticeWM where a Linux system expects to find it.
#
# Six things, and each of them is why somebody's first attempt at running a
# window manager from source fails, or why a copy of it is not one you may
# legally pass on:
#
#   the binary        $PREFIX/bin/latticewm
#   a session entry   so the login screen offers "LatticeWM" like any other
#                     desktop, instead of requiring a TTY and a shell command
#   a launcher        latticewm-session: river, told to run us as its client
#   man pages         $PREFIX/share/man/man{1,5}
#   the sources       the lattice, the examples and the documentation, under
#                     $PREFIX/share/latticewm/, on ASDF's registry at startup
#   the licences      ours, and the font's -- the bitmap font is Terminus under
#                     the OFL, and src/runtime/font.lisp says in its own header
#                     that the licence text must travel with any copy of it
#
# THIS IS THE ONLY INSTALLER.  It used to be one of two: flake.nix had an
# `installPhase' with a second, hand-maintained list of what ships, and the two
# lists had drifted apart -- including over lattice.asd, which is the exact
# artifact gate 9 was written to keep in the install.  The nix path is the one
# that reaches a user who never ran `make', so every divergence was a bug that
# only a packaged install could have.  `nix build' now runs this script, which
# is why the two flags below exist: they are the two things a build sandbox
# needs said differently, and they are the whole of the difference.
#
# It installs into the home directory by default and needs no root.  Give it
# --prefix /usr/local (with sudo) for a system install.
#
#   ./install.sh
#   sudo ./install.sh --prefix /usr/local
#   ./install.sh --uninstall
#
#   --no-config       do not write a starter init.lisp (a build sandbox has no
#                     home directory to write one into)
#   --river PATH      the river the launcher and the session entry run.  The
#                     default is whatever is on PATH at login, which is right
#                     for a system install and wrong for a store path, where
#                     the whole point is that the compositor is pinned.
#   --destdir PATH    write everything under PATH while baking --prefix into
#                     the paths inside the files.  $DESTDIR is read too.  This
#                     is what every distribution's packaging does and this
#                     script could not express it: --prefix alone cannot say
#                     "the program will live at /usr, put the bytes over
#                     here".  Implies --no-config, because no postinstall may
#                     touch a user's home directory -- and under `sudo
#                     ./install.sh --prefix /usr/local' the starter config
#                     landed in *root's* .config, which nobody wanted either.
#
# There is no DESTDIR for the *session entry* to argue about: it goes under
# --prefix like everything else once a prefix is not a home directory.

set -eu

root=$(cd "$(dirname "$0")" && pwd)
home=${HOME:-}
prefix=${PREFIX:-$home/.local}
destdir=${DESTDIR:-}
action=install
write_config=yes
river=${RIVER_BIN:-${RIVER:-river}}

while [ $# -gt 0 ]; do
    case "$1" in
        --prefix) prefix=$2; shift 2 ;;
        --prefix=*) prefix=${1#--prefix=}; shift ;;
        --destdir) destdir=$2; shift 2 ;;
        --destdir=*) destdir=${1#--destdir=}; shift ;;
        --river) river=$2; shift 2 ;;
        --river=*) river=${1#--river=}; shift ;;
        --no-config) write_config=no; shift ;;
        --uninstall) action=uninstall; shift ;;
        # THE HELP TEXT USED TO BE `sed -n "2,40p"' AND THE HEADER RAN TO 41,
        # so --help ended mid-clause.  A line number is a reference to a
        # position rather than to a thing, and it goes stale the first time
        # anyone writes a sentence above it.  The header is every leading
        # comment line, and it ends where the comments do.
        -h|--help)
            sed -n '2,/^[^#]/p' "$0" | sed -n 's/^# \{0,1\}//p'; exit 0 ;;
        *) printf 'install.sh: unknown argument %s\n' "$1" >&2; exit 2 ;;
    esac
done

# A staging root is a packaging build, and a packaging build has no business in
# anybody's home directory.  Implied rather than merely allowed, because the
# alternative is every packager remembering a second flag.
[ -n "$destdir" ] && write_config=no

say() { printf '%s\n' "$*"; }

# Where a wayland session entry goes depends on who is installing.  A system
# prefix means every display manager will see it; a home prefix means only the
# ones that honour XDG_DATA_DIRS will, which is most but not all of them —
# said out loud below rather than left to be discovered at the login screen.
#
# Written as a prefix test rather than a `case' pattern because a build sandbox
# has no HOME, and "$HOME"* with HOME unset is `*', which matches every prefix
# there is and would put a store path's session entry in a home directory.
if [ -z "$destdir" ] && [ -n "$home" ] && [ "${prefix#"$home"}" != "$prefix" ]; then
    sessions="$home/.local/share/wayland-sessions"; systemwide=no
else
    sessions="$prefix/share/wayland-sessions";      systemwide=yes
fi

# TWO SETS OF PATHS, AND THE DIFFERENCE IS THE WHOLE OF DESTDIR.
#
# The undecorated names are where the program *will live*, and they are what
# gets written *into* files: the launcher's path to the binary, the session
# entry's path to the launcher.  The d-prefixed names are where the bytes go
# now.  With no --destdir the two are identical and nothing changes.
#
# This is why --prefix alone could not express packaging.  A distribution
# builds with `make DESTDIR=$pkgdir PREFIX=/usr install' and needs /usr baked
# into every path the installed program will use while every file lands in
# $pkgdir.  Arch, Debian, RPM, Gentoo, Alpine and Void all do it this way, and
# the string DESTDIR appeared in neither this file nor the Makefile -- which is
# four of the reasons no distribution could have packaged this.
bin="$prefix/bin"
man="$prefix/share/man/man1"
man5="$prefix/share/man/man5"
share="$prefix/share/latticewm"

dbin="$destdir$bin"
dman="$destdir$man"
dman5="$destdir$man5"
dshare="$destdir$share"
dsessions="$destdir$sessions"

if [ "$action" = uninstall ]; then
    # EVERY FILE THE INSTALL WRITES APPEARS HERE, and tools/install-check.sh is
    # what keeps that true: it installs to a scratch prefix, then uninstalls,
    # then fails if anything is left behind.  A `rm -f' list maintained by hand
    # beside a growing install is a list that silently stops being complete --
    # latticewm-config.5 was already missing from it.
    rm -f "$dbin/latticewm" "$dbin/latticewm-session" \
          "$dsessions/latticewm.desktop" \
          "$dman/latticewm.1" "$dman5/latticewm-config.5"
    rm -rf "$dshare"
    say "removed LatticeWM from ${destdir:+$destdir (staged) }$prefix."
    say "your configuration in ~/.config/latticewm/ was left alone."
    exit 0
fi

[ -x "$root/latticewm" ] || {
    say "no ./latticewm yet.  Build it first:"
    say ""
    say "  ./bootstrap.sh   # once, if you are not on nix"
    say "  make image"
    exit 1
}

mkdir -p "$dbin" "$dman" "$dman5" "$dshare" "$dsessions"

install -m755 "$root/latticewm" "$dbin/latticewm"

# The launcher.  river runs one command as its init; ours is the window
# manager.  Written here rather than shipped as a file so that the path to the
# binary is the one actually installed — and, since --river, so that the path
# to the *compositor* is too.  A store path that named the bare word `river'
# would find whatever the user's PATH held at login, which is precisely the
# floating half of the pair flake.nix exists to pin.
cat > "$dbin/latticewm-session" <<EOF
#!/bin/sh
# Start river with LatticeWM as its window manager.
exec $river -c "$bin/latticewm \$*"
EOF
chmod 755 "$dbin/latticewm-session"

cat > "$dsessions/latticewm.desktop" <<EOF
[Desktop Entry]
Name=LatticeWM
Comment=An extensible window manager for river
Exec=$bin/latticewm-session
Type=Application
DesktopNames=river
EOF

[ -f "$root/doc/latticewm.1" ] && install -m644 "$root/doc/latticewm.1" "$dman/latticewm.1"
[ -f "$root/doc/latticewm-config.5" ] &&
    install -m644 "$root/doc/latticewm-config.5" "$dman5/latticewm-config.5"

# The generated references, and the extension guide.  EXTENDING.org was the one
# document users needed and the one document this loop did not copy, because it
# matched *.txt and the guide is an .org file.
mkdir -p "$dshare/doc"
for file in EXTENSION-SURFACE.txt CONTAINER-SURFACE.txt HOOKS.txt COMMANDS.txt \
            OPTIONS.txt KEYS.txt EXTENDING.org; do
    [ -f "$root/doc/$file" ] && install -m644 "$root/doc/$file" "$dshare/doc/$file"
done
for file in README.org INSTALL.org FINDINGS.org DESIGN.org; do
    [ -f "$root/$file" ] && install -m644 "$root/$file" "$dshare/doc/$file"
done

# THE LICENCES, AND THE FONT'S IS NOT OPTIONAL.  src/runtime/font.lisp is a
# generated table of Terminus glyphs, and its own header says the OFL text
# "must travel with any copy of this file".  The binary contains that table, so
# every install is a copy — and neither install path shipped the licence.  The
# nix one caught doc/OFL-TERMINUS.txt by accident, through a doc/*.txt glob;
# this one had a hand-written list that did not name it.  Our own LICENCE was
# shipped by neither, because it has no extension and both paths matched on one.
[ -f "$root/LICENSE" ] && install -m644 "$root/LICENSE" "$dshare/doc/LICENSE"
[ -f "$root/doc/OFL-TERMINUS.txt" ] &&
    install -m644 "$root/doc/OFL-TERMINUS.txt" "$dshare/doc/OFL-TERMINUS.txt"

# The pinned protocol, so a version mismatch can be diagnosed from an installed
# copy without the source tree.  LatticeWM refuses to start against a river
# whose river-window-management-v1 is not the version these XMLs declare, and
# the place it refuses is a login screen; the interface version is in here, and
# src/protocol/PINNED carries the argument for which river it came from.
if [ -d "$root/src/protocol" ]; then
    mkdir -p "$dshare/protocol"
    for file in "$root"/src/protocol/*.xml "$root/src/protocol/PINNED"; do
        [ -f "$file" ] && install -m644 "$file" "$dshare/protocol/$(basename "$file")"
    done
fi

# The lattice, as source, so that an installed system can load it.
#
# The binary already contains it, so this is not how it is normally loaded --
# it is here so that the sources are readable beside the machine that runs
# them, which is the whole argument for shipping a Lisp program, and so that
# `(load-extension "lattice")' works even in an image built without it.
# LATTICEWM/RUNTIME:DATA-DIRECTORIES is the other half: it puts this directory
# on ASDF's registry at startup.
if [ -f "$root/lattice.asd" ]; then
    install -m644 "$root/lattice.asd" "$dshare/lattice.asd"
    mkdir -p "$dshare/lattice"
    for file in "$root"/lattice/*.lisp; do
        [ -f "$file" ] && install -m644 "$file" "$dshare/lattice/$(basename "$file")"
    done
fi

# The examples, for the same reason: they are documentation that runs.
if [ -d "$root/examples" ]; then
    mkdir -p "$dshare/examples"
    for file in "$root"/examples/*.lisp; do
        [ -f "$file" ] && install -m644 "$file" "$dshare/examples/$(basename "$file")"
    done
fi

# The starter configuration, only if there is not one already.  Overwriting
# somebody's init.lisp during an upgrade would be unforgivable — and a build
# sandbox has no home directory to write one into, which is what --no-config is
# for.  It is skipped rather than attempted-and-forgiven so that a failure to
# write one on a real machine stays visible.
if [ "$write_config" = no ]; then
    :
elif [ -z "$home" ] && [ -z "${XDG_CONFIG_HOME:-}" ]; then
    say "no HOME and no XDG_CONFIG_HOME; skipped the starter configuration"
else
    config="${XDG_CONFIG_HOME:-$home/.config}/latticewm/init.lisp"
    if [ -f "$config" ]; then
        say "kept your existing $config"
    else
        "$dbin/latticewm" --write-config >/dev/null 2>&1 && say "wrote a starter $config"
    fi
fi

say ""
say "installed:"
say "  $dbin/latticewm"
say "  $dbin/latticewm-session"
say "  $dsessions/latticewm.desktop"
say "  $dman/latticewm.1, $dman5/latticewm-config.5"
say "  $dshare/           (the lattice, the examples, the documentation,"
say "                      the licences and the pinned protocol)"
say ""

# A staging root ends here.  There is no login screen to talk about, no PATH to
# check, and no session to try it in -- the package manager does all three
# later, on a different machine.
if [ -n "$destdir" ]; then
    say "staged under $destdir for a prefix of $prefix."
    say "The paths written into the launcher and the session entry are the"
    say "prefix, not the staging root, which is the point of --destdir."
    exit 0
fi

if [ "$systemwide" = no ]; then
    case ":$PATH:" in
        *":$bin:"*) ;;
        *) say "NOTE: $bin is not on your PATH." ;;
    esac

    say "The session entry is in your home directory.  GDM and SDDM read"
    say "\$XDG_DATA_DIRS, so they will find it; a few lighter greeters only"
    say "look in /usr/share/wayland-sessions.  If LatticeWM does not appear"
    say "at the login screen, install it system-wide instead:"
    say ""
    say "  sudo ./install.sh --prefix /usr/local"
    say ""
fi

say "Log out and pick LatticeWM, or try it inside this session first:"
say ""
say "  river -c latticewm      # nested, in a window"
say ""
say "The first thing to press is Super+/ -- it draws the whole keymap."
