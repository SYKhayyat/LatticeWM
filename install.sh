#!/bin/sh
# install.sh --- put LatticeWM where a Linux system expects to find it.
#
# Four things, and each of them is why somebody's first attempt at running a
# window manager from source fails:
#
#   the binary        $PREFIX/bin/latticewm
#   a session entry   so the login screen offers "LatticeWM" like any other
#                     desktop, instead of requiring a TTY and a shell command
#   a launcher        latticewm-session: river, told to run us as its client
#   a man page        $PREFIX/share/man/man1/latticewm.1
#
# It installs into the home directory by default and needs no root.  Give it
# --prefix /usr/local (with sudo) for a system install.
#
#   ./install.sh
#   sudo ./install.sh --prefix /usr/local
#   ./install.sh --uninstall

set -eu

root=$(cd "$(dirname "$0")" && pwd)
prefix=${PREFIX:-$HOME/.local}
action=install

while [ $# -gt 0 ]; do
    case "$1" in
        --prefix) prefix=$2; shift 2 ;;
        --prefix=*) prefix=${1#--prefix=}; shift ;;
        --uninstall) action=uninstall; shift ;;
        -h|--help) sed -n '2,20p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        *) printf 'install.sh: unknown argument %s\n' "$1" >&2; exit 2 ;;
    esac
done

say() { printf '%s\n' "$*"; }

# Where a wayland session entry goes depends on who is installing.  A system
# prefix means every display manager will see it; a home prefix means only the
# ones that honour XDG_DATA_DIRS will, which is most but not all of them —
# said out loud below rather than left to be discovered at the login screen.
case "$prefix" in
    "$HOME"*) sessions="$HOME/.local/share/wayland-sessions"; systemwide=no ;;
    *)        sessions="$prefix/share/wayland-sessions";      systemwide=yes ;;
esac

bin="$prefix/bin"
man="$prefix/share/man/man1"
share="$prefix/share/latticewm"

if [ "$action" = uninstall ]; then
    rm -f "$bin/latticewm" "$bin/latticewm-session" \
          "$sessions/latticewm.desktop" "$man/latticewm.1"
    rm -rf "$share"
    say "removed LatticeWM from $prefix."
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

mkdir -p "$bin" "$man" "$share" "$sessions"

install -m755 "$root/latticewm" "$bin/latticewm"

# The launcher.  river runs one command as its init; ours is the window
# manager.  Written here rather than shipped as a file so that the path to the
# binary is the one actually installed.
cat > "$bin/latticewm-session" <<EOF
#!/bin/sh
# Start river with LatticeWM as its window manager.
exec river -c "$bin/latticewm \$*"
EOF
chmod 755 "$bin/latticewm-session"

cat > "$sessions/latticewm.desktop" <<EOF
[Desktop Entry]
Name=LatticeWM
Comment=An infinite-grid window manager for river
Exec=$bin/latticewm-session
Type=Application
DesktopNames=river
EOF

[ -f "$root/doc/latticewm.1" ] && install -m644 "$root/doc/latticewm.1" "$man/latticewm.1"
for file in EXTENSION-SURFACE.txt COMMANDS.txt OPTIONS.txt KEYS.txt; do
    [ -f "$root/doc/$file" ] && install -m644 "$root/doc/$file" "$share/$file"
done

# The starter configuration, only if there is not one already.  Overwriting
# somebody's init.lisp during an upgrade would be unforgivable.
config="${XDG_CONFIG_HOME:-$HOME/.config}/latticewm/init.lisp"
if [ -f "$config" ]; then
    say "kept your existing $config"
else
    "$bin/latticewm" --write-config >/dev/null 2>&1 && say "wrote a starter $config"
fi

say ""
say "installed:"
say "  $bin/latticewm"
say "  $bin/latticewm-session"
say "  $sessions/latticewm.desktop"
say ""

case "$prefix" in
    "$HOME"*)
        case ":$PATH:" in
            *":$bin:"*) ;;
            *) say "NOTE: $bin is not on your PATH." ;;
        esac
        ;;
esac

if [ "$systemwide" = no ]; then
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
