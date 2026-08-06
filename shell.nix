# Development shell for LatticeWM.
#
# Nothing under src/ may know that nix exists.  This file is a packaging
# convenience: it provides SBCL, the Lisp dependencies, and — critically — the
# river compositor, so that the window manager and the compositor it is
# generated against can be one versioned pair.
#
#   nix-shell            -> a shell with sbcl, river, and the deps
#   nix-shell --run make -> build + gates
#
# IT SAID "PINS" HERE FOR THE WHOLE LIFE OF THE PROJECT AND PINNED NOTHING.
# `<nixpkgs>' is the ambient channel, so the river in this shell was whatever
# the machine happened to hold — the same untruth flake.nix told about pinning
# a channel rather than a river, and it came due in the other direction: the
# day the protocol was re-vendored to river 0.4.6, a machine whose channel
# still held 0.4.5 got a refusal from `make check' and no account of why.
#
# So it pins now, and it pins the *same* nixpkgs the flake locks, read out of
# flake.lock by revision and hash.  That is the point: `nix-shell', `nix
# develop' and `nix build' are one river rather than two pinned ones and a
# third that moves under you.  A pair that is only pinned on one of three
# paths is not pinned, and this protocol changes inside a patch release.
#
# Three properties worth keeping when editing this:
#
#   * It costs nothing on a machine that has already built the flake — same
#     revision, same narHash, already in the store.
#   * If flake.lock is deleted it falls back to <nixpkgs>, so PLAN.org's "if
#     the flake is deleted the project still builds" survives.
#   * `pkgs' is still an argument, so passing your own overrides all of it,
#     and the shellHook then warns if that river is not the vendored one.
#
# Note on wayflan: nixpkgs' `sbclPackages.wayflan` umbrella derivation is
# broken (it tries to compile wayflan-client's sources into a read-only store
# path).  `sbclPackages.wayflan-client` builds fine but its output omits
# wayflan.asd, which it depends on.  So we take the *source* of the nixpkgs
# package — which is the real sr.ht tree, pinned by nixpkgs — and let ASDF
# compile it into the user's fasl cache.  Same pin, no fork, no curl.
let
  lockedNixpkgs =
    let
      lock = ./flake.lock;
      node =
        if builtins.pathExists lock
        then (builtins.fromJSON (builtins.readFile lock)).nodes.nixpkgs.locked
        else { };
    in
    if (node.type or "") == "github"
    then
      builtins.fetchTarball {
        url = "https://github.com/${node.owner}/${node.repo}/archive/${node.rev}.tar.gz";
        sha256 = node.narHash;
      }
    else <nixpkgs>;
in

{ pkgs ? import lockedNixpkgs { } }:

let
  wayflanSrc = pkgs.sbclPackages.wayflan.src;

  # The release the protocol XML was vendored from, read out of PINNED rather
  # than written here, so this warning cannot come to disagree with the check
  # the program actually performs at startup.
  pinnedRiver =
    let
      firstLine = builtins.head
        (builtins.split "\n" (builtins.readFile ./src/protocol/PINNED));
      matched = builtins.match "river ([0-9][0-9.]*).*" firstLine;
    in
    if matched == null then null else builtins.head matched;

  lisp = pkgs.sbcl.withPackages (ps: with ps; [
    cffi
    cffi-grovel
    alexandria
    closer-mop
    babel
    plump
    array-utils
    documentation-utils
    trivial-indent
    trivial-features
    trivial-gray-streams
    bordeaux-threads
    swank
    fiveam
  ]);
in
pkgs.mkShell {
  packages = [
    lisp
    pkgs.river
    pkgs.foot # a tiny terminal, for testing under nested river
    pkgs.grim # screenshots of the nested session -- see PLAN.org §looking
    pkgs.wayland-utils
    # xkbcli, for *XKB-LAYOUT*.  It compiles a keymap from layout names, which
    # is libxkbcommon's job and not a thing the protocol will do for us -- see
    # runtime/input.lisp on why this is the one place the program shells out.
    # It ships with libxkbcommon, so it is already on any machine running
    # river; it is named here so that the integration test can rely on it.
    pkgs.libxkbcommon.dev
    pkgs.git
  ];

  # Consumed by tools/ scripts and the Makefile.
  WAYFLAN_SRC = wayflanSrc;
  RIVER = "${pkgs.river}";
  RIVER_VERSION = pkgs.river.version;

  shellHook = ''
    export LATTICEWM_ROOT=$PWD
    export CL_SOURCE_REGISTRY="$WAYFLAN_SRC//:$PWD//"
  '' + pkgs.lib.optionalString
    (pinnedRiver != null && pkgs.river.version != pinnedRiver) ''
    echo "" >&2
    echo "  The river in this shell is ${pkgs.river.version}, and the protocol in" >&2
    echo "  src/protocol/ was vendored from river ${pinnedRiver}." >&2
    echo "" >&2
    echo "  Build, gates and unit tests are unaffected.  'make integration'" >&2
    echo "  and 'make check' will fail on the version check, by design --" >&2
    echo "  see src/protocol/PINNED.  'nix build' pins both and is green." >&2
    echo "" >&2
  '';
}
