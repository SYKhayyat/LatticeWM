# Development shell for LatticeWM.
#
# Nothing under src/ may know that nix exists.  This file is a packaging
# convenience: it pins SBCL, the Lisp dependencies, and — critically — the
# river compositor, so that the window manager and the compositor it is
# generated against are one versioned pair.
#
#   nix-shell            -> a shell with sbcl, river, and the deps
#   nix-shell --run make -> build + gates
#
# Note on wayflan: nixpkgs' `sbclPackages.wayflan` umbrella derivation is
# broken (it tries to compile wayflan-client's sources into a read-only store
# path).  `sbclPackages.wayflan-client` builds fine but its output omits
# wayflan.asd, which it depends on.  So we take the *source* of the nixpkgs
# package — which is the real sr.ht tree, pinned by nixpkgs — and let ASDF
# compile it into the user's fasl cache.  Same pin, no fork, no curl.
{ pkgs ? import <nixpkgs> { } }:

let
  wayflanSrc = pkgs.sbclPackages.wayflan.src;

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
    pkgs.wayland-utils
    pkgs.git
  ];

  # Consumed by tools/ scripts and the Makefile.
  WAYFLAN_SRC = wayflanSrc;
  RIVER = "${pkgs.river}";
  RIVER_VERSION = pkgs.river.version;

  shellHook = ''
    export LATTICEWM_ROOT=$PWD
    export CL_SOURCE_REGISTRY="$WAYFLAN_SRC//:$PWD//"
  '';
}
