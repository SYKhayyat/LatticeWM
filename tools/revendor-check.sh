#!/bin/sh
# tools/revendor-check.sh --- Run the re-vendor recipe, and check that it works.
#
# INSTALL.org's four-step re-vendor recipe is not a footnote.  This project
# pins river 0.4.6 exactly and refuses to start against anything else, which is
# the right call for a protocol that moves inside a patch release -- and it
# means that on most distributions at most times the packaged river will not
# match, so the recipe *is* the install path for the majority.  It ends
# "Nothing about this needs the original author," which is exactly right, and
# nothing tested it.  Gate 5 asserts that the versions we bind match the XML we
# vendored; nothing had ever re-vendored against a *different* river and asked
# whether the recipe still worked.
#
# TWO HALVES, AND ONLY ONE OF THEM NEEDS A SECOND RIVER.
#
#   ./tools/revendor-check.sh /path/to/river
#       The real thing: copy that river's protocol XML over src/protocol/,
#       write PINNED, and run the build and the gates.  Green means somebody
#       with a newer river can follow four steps in a document and have a
#       working window manager without asking anybody.
#
#   ./tools/revendor-check.sh
#       The half that needs nothing: synthesise a river this program does not
#       accept by bumping an interface version in a scratch copy of the XML,
#       and assert that the build *refuses*, by name, with a message that says
#       which interface and which two numbers.  A pin that fails silently is
#       worse than no pin, and that is the property being asserted.
#
# The working tree is restored either way, including after a failure.

set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
river_src=${1:-}
backup=$(mktemp -d)
trap 'cp -a "$backup/protocol/." "$root/src/protocol/"; rm -rf "$backup"' EXIT INT TERM

say() { printf '%s\n' "$*" >&2; }

mkdir -p "$backup/protocol"
cp -a "$root/src/protocol/." "$backup/protocol/"

if [ -n "$river_src" ]; then
    # ---------------------------------------------------------------- real
    if [ ! -d "$river_src/protocol" ]; then
        say "no $river_src/protocol/ -- point this at a river source tree"
        exit 2
    fi
    say "re-vendoring from $river_src"
    found=0
    for xml in "$root"/src/protocol/river-*.xml; do
        name=$(basename "$xml")
        if [ -f "$river_src/protocol/$name" ]; then
            cp "$river_src/protocol/$name" "$xml"
            found=$((found + 1))
        else
            say "  $name is not in that river -- left as it was"
        fi
    done
    if [ "$found" -eq 0 ]; then
        say "that tree has none of our six protocols; is it a river checkout?"
        exit 2
    fi
    version=$("$river_src/zig-out/bin/river" --version 2>/dev/null || echo unknown)
    printf 'river %s / re-vendored by tools/revendor-check.sh from %s\n' \
        "$version" "$river_src" > "$root/src/protocol/PINNED"
    say "copied $found protocol files; building"
    if (cd "$root" && make build gates); then
        say ""
        say "THE RECIPE WORKS against that river: the bindings regenerated, the"
        say "versions agree, and the gates pass.  Commit src/protocol/ and PINNED."
        exit 0
    fi
    say ""
    say "THE RECIPE DID NOT COMPLETE against that river.  That is a finding and"
    say "not a mistake on your part: read the gate 5 output above, which names"
    say "the interface and both numbers.  src/protocol/PINNED carries the"
    say "argument for the last time this happened."
    exit 1
fi

# ------------------------------------------------------- the synthetic half
say "no river given: checking that a river we do NOT accept is refused"

xml="$root/src/protocol/river-window-management-v1.xml"
before=$(grep -o 'name="river_window_manager_v1" version="[0-9]*"' "$xml" | head -1)
if [ -z "$before" ]; then
    say "cannot find the window manager interface version in the vendored XML"
    exit 2
fi
current=$(printf '%s' "$before" | sed 's/.*version="\([0-9]*\)".*/\1/')
bumped=$((current + 1))
say "vendored river_window_manager_v1 is version $current; pretending it is $bumped"

sed -i.bak "s/name=\"river_window_manager_v1\" version=\"$current\"/name=\"river_window_manager_v1\" version=\"$bumped\"/" "$xml"
rm -f "$xml.bak"

if (cd "$root" && make build gates >"$backup/out" 2>&1); then
    say ""
    say "THE PIN DOES NOT HOLD.  The XML now declares river_window_manager_v1"
    say "at version $bumped and the program binds $current, and the build passed"
    say "anyway -- so a river this program cannot speak to would be accepted at"
    say "build time and refused at a login screen, which is the failure mode"
    say "src/protocol/PINNED and flake.nix both call the largest threat to this"
    say "project.  Gate 5 is what should have caught this."
    exit 1
fi

if grep -qi "version" "$backup/out"; then
    say ""
    say "REFUSED, AND IT SAID WHY.  The relevant output:"
    grep -i -B 2 -A 6 "version" "$backup/out" | head -30 >&2
    exit 0
fi

say ""
say "The build failed, but nothing in its output mentions a version -- so it"
say "refused for some other reason and this check proved nothing.  The output:"
tail -30 "$backup/out" >&2
exit 1
