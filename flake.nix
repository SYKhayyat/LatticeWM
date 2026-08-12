{
  description = "LatticeWM — an extensible window manager for the river Wayland compositor";

  # PLAN.org §packaging: "Nix is a packaging target, not a dependency. […]
  # Nothing in src/ may know that nix exists.  If the flake is deleted the
  # project still builds.  That is the test."
  #
  # It is here for one thing the plain build cannot do, and that thing is the
  # largest single threat the plan identifies to surviving without a
  # maintainer: river-window-management-v1 is young, and a pre-release protocol
  # can break within a version number rather than politely bumping to v2.  A
  # flake pins the compositor and the window manager as one versioned pair,
  # upgraded deliberately or never.
  #
  # The plain-Linux equivalent is crude and works: the known-good river commit
  # is recorded in DESIGN, and river is Zig and builds to a single file, so
  # keeping a copy of the working binary is a real fallback.

  # One input, deliberately.  Every flake input is a thing that can rot, and
  # this project's stated requirement is that it survive without a maintainer;
  # flake-utils saves six lines and adds a dependency, which is the wrong trade
  # here.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forEach = f: nixpkgs.lib.genAttrs systems f;

      # The project's version, read out of the one file that holds it.  Both
      # .asd files read the same file with ASDF's :read-file-line, and gate 19
      # holds the man pages to it too.  Same shape as pinnedRiver in shell.nix,
      # which reads river's version out of src/protocol/PINNED and carries the
      # argument for doing it: a copy that can disagree with the thing it
      # describes is worse than no copy at all.
      latticewmVersion =
        let
          line = builtins.head
            (builtins.split "\n" (builtins.readFile ./VERSION));
          matched = builtins.match "[[:space:]]*([0-9][0-9.]*).*" line;
        in
        if matched == null
        then throw "flake.nix: VERSION does not begin with a version number"
        else builtins.head matched;

      # Everything that varies by system, computed once per system and then
      # projected into the three output attrsets a flake wants.
      envFor = system:
        let
          pkgs = import nixpkgs { inherit system; };

          # nixpkgs' sbclPackages.wayflan umbrella derivation is broken: it
          # tries to compile wayflan-client's sources into a read-only store
          # path.  wayflan-client itself builds but its output omits
          # wayflan.asd, which it depends on.  So we take the *source* of the
          # nixpkgs package — the real sr.ht tree, nixpkgs-pinned — and let
          # ASDF compile it.  Same pin, no fork, no curl.
          wayflanSrc = pkgs.sbclPackages.wayflan.src;

          lisp = pkgs.sbcl.withPackages (ps: with ps; [
            cffi cffi-grovel alexandria closer-mop babel
            plump array-utils documentation-utils trivial-indent
            trivial-features trivial-gray-streams bordeaux-threads
            swank fiveam
          ]);

          latticewm = pkgs.stdenv.mkDerivation {
            pname = "latticewm";
            # READ, NOT REPEATED.  This said "0.1.0" and so did latticewm.asd,
            # lattice.asd and the .TH line of both man pages, and the sole git
            # tag said v0.1 and matched none of them.  Twenty lines below, this
            # file reads *river's* version out of src/protocol/PINNED rather
            # than duplicating it, for exactly this reason; the pattern was
            # used twice and never turned inward on the line above.
            version = latticewmVersion;
            src = ./.;
            # river and foot are build inputs because the integration test needs
            # a compositor to receive state from and a client to open a window.
            # Both run headless — WLR_BACKENDS=headless, no DRM, no seat — which
            # is the only reason this is possible inside a build sandbox.
            nativeBuildInputs = [ lisp pkgs.river pkgs.foot ];

            # The same steps `make check` runs.  The gates and the tests are
            # part of the build rather than a separate check, because a window
            # manager that fails its own gates should not become a store path.
            #
            # THE INTEGRATION TEST IS PART OF IT NOW, AND ITS ABSENCE WAS THE
            # HOLE.  This was the one automated path in the project — there is a
            # CI workflow beside it now, but this is the one that gates the
            # store path — and it ran build, gates, test and image and omitted
            # the only test that receives state rather than constructing it.
            # LATTICEWM_REQUIRE_INTEGRATION makes a missing river or a missing
            # terminal a failure rather than a paragraph and an exit 0; in a
            # sandbox with both named above, that is a statement about this
            # derivation rather than about the machine it was built on.
            #
            # *IF THIS PHASE FAILS ON THE PROTOCOL VERSION, THAT IS THE POINT,
            # AND IT IS THE THING THE HEADER OF THIS FILE SAYS IS THE LARGEST
            # THREAT TO THE PROJECT.*  It did, on the first run.  River bumped
            # river_window_manager_v1 from 4 to 5 between 0.4.5 and 0.4.6, which
            # is a *patch* release, and the locked nixpkgs has 0.4.6 — so this
            # derivation was building a window manager that refuses to start
            # against the river the very same derivation puts in its session
            # entry.  `nix build' succeeded, `install' wrote a wayland-sessions
            # file, and logging in got a version-mismatch paragraph at a login
            # screen.  It succeeded because nothing in the build phase had ever
            # connected to the pinned river.  Now something does.
            #
            # THE RESOLUTION WAS TO RE-VENDOR, not to pin river back to 0.4.5.
            # src/protocol/PINNED carries the argument; it rests on the 4 -> 5
            # diff being additive in every particular, so following river cost
            # two numbers and pinning back would have frozen the pair on a
            # release that leaves nixpkgs on a schedule nobody here controls.
            #
            # AND THE CHECK IT TRIPPED ON HAS SINCE BECOME A FLOOR, which
            # changes what this phase catches and is worth saying plainly
            # rather than leaving for somebody to discover.  A `nix flake
            # update' that moves river *forward* under a vendored XML no longer
            # fails here, because it is no longer a failure: the program binds
            # at its own version and river speaks it.  What still fails here is
            # a river below the floor — one missing a request the program
            # sends — which is the case that genuinely cannot be degraded
            # around.  The trade is deliberate.  Early warning on a forward
            # move was only ever valuable because a forward move was fatal; it
            # is not fatal now, and buying the warning back would cost every
            # user whose distribution ships river before we do.
            #
            # What is load-bearing is that this phase connects at all.  The
            # nixpkgs input is still a channel, so a future `nix flake update'
            # can still move the compositor under a vendored XML — and when it
            # moves somewhere the program cannot follow, this phase fails at
            # `nix build' rather than at a login screen, which is the whole
            # difference and is tested rather than asserted.  Re-vendor from
            # the new river to pick up its features, or hold the
            # lock; both are deliberate acts and either is fine.
            # THERE IS ONE LIST OF STEPS AND THIS IS NOT IT EITHER.  What stood
            # here was five hand-typed `sbcl --load' lines under a comment
            # reading "the same steps `make check` runs" — the identical defect
            # as the installPhase below, in the same file, left standing
            # directly above the commit that fixed the other one.  Gate 9 reads
            # installPhase and had six references to it; nothing anywhere read
            # buildPhase.  So `make check' could grow a step, or reorder one, or
            # drop one, and the packaged build would go on running the old
            # sequence while a comment three lines up said it did not.  Gate 21
            # reads this phase now, on exactly gate 9's terms.
            buildPhase = ''
              export HOME=$TMPDIR
              export XDG_RUNTIME_DIR=$TMPDIR/run
              mkdir -p "$XDG_RUNTIME_DIR"
              export WAYFLAN_SRC="${wayflanSrc}"
              make check image
            '';

            # THERE IS ONE INSTALLER AND THIS IS NOT IT.  This phase used to
            # hand-roll a second list of what ships, beside install.sh's, and
            # the two had drifted apart everywhere below.  Every difference was
            # a bug that only a *packaged* install could have, which is the
            # worst place for one: this is the path that reaches a user who
            # never ran `make', so nothing anybody did in a source tree could
            # see it.  The whole list, since this is the one place it is
            # enumerated:
            #
            #   * no lattice.asd and no lattice/ — which is gate 9's own bug,
            #     the one it was written to prevent, live on the nix path for
            #     the whole life of the gate.  The sample configuration loads
            #     the lattice, DATA-DIRECTORIES puts share/latticewm/ on ASDF's
            #     registry to find it, and there was nothing there to find;
            #   * no man pages, so `man latticewm' failed on nix alone;
            #   * no latticewm-session, so the launcher install.sh calls one of
            #     the four things it exists for was missing;
            #   * doc/ flattened into share/latticewm/ rather than
            #     share/latticewm/doc/, so the same file had two paths
            #     depending on how you installed;
            #   * tools/wmeval on PATH — a SWANK client, and SWANK is off by
            #     default, so the one binary unique to this path was the one
            #     that could not work.  It is deleted now; `latticewm --eval'
            #     is the same capability over the socket that is on by default;
            #   * and in the other direction, install.sh shipped no protocol
            #     XML, so the version-mismatch diagnostic this file argues is
            #     the largest threat to the project was readable only by nix
            #     users;
            #   * a *.org glob here also shipped PLAN, ASSESSMENT and
            #     SPIKE-WEEK0, which install.sh's curated list leaves out.  That
            #     one is settled the other way: the curated list wins, because
            #     the session logs are a record for whoever works on this and
            #     not documentation for whoever runs it.
            #
            # The licences were the case neither list had on purpose, and it is
            # the one that mattered legally rather than practically: the OFL
            # text arrived here through a doc/*.txt glob and nowhere at all
            # through install.sh, and our own LICENSE shipped on neither path,
            # because it has no extension and both matched on one.
            #
            # Running the script is what makes divergence impossible rather
            # than merely checked, and it makes `nix build' the clean-prefix
            # install test that gate 9 can only approximate from text.  The two
            # flags are the whole of what a sandbox needs said differently.
            installPhase = ''
              ./install.sh --prefix $out --no-config \
                           --river ${pkgs.river}/bin/river
            '';

            # THIS SAID `bsd3' AND THE FILE BESIDE IT IS 674 LINES OF GPLv3.
            #
            # Both .asd files say GPL-3.0-or-later, and PLAN.org records fixing
            # them — they had said BSD-3-Clause and were simply wrong.  Both
            # were corrected; this, the third copy, was not, and it is the one
            # that becomes what `nix search' shows.  As a metadata nit it is one
            # word.  As a *community* project it is what stops a distribution
            # packager cold and what makes the licence of an incoming
            # contribution genuinely ambiguous, which is worse than either.
            #
            # Twenty lines of comment in this same file argue that a licence
            # text must travel with a copy.  The declaration is the other half
            # of that argument and it disagreed with it.
            #
            # Gate 19 now reads all three declarations and the LICENSE text and
            # fails when they disagree, because "corrected in two of three
            # places" is exactly the failure this was.
            meta = with pkgs.lib; {
              description = "An extensible window manager for the river Wayland compositor";
              license = licenses.gpl3Plus;
              platforms = platforms.linux;
              mainProgram = "latticewm";
            };
          };
        in
        { inherit pkgs wayflanSrc lisp latticewm; };
    in
    {
      packages = forEach (system:
        let env = envFor system; in {
          default = env.latticewm;
          latticewm = env.latticewm;
          # The compositor this build is known good against, exposed so a user
          # can pin the pair rather than only half of it.
          river = env.pkgs.river;
        });

      apps = forEach (system:
        let env = envFor system; in {
          default = {
            type = "app";
            program = "${env.latticewm}/bin/latticewm";
          };
          # `nix run .#nested` — river inside the session you are already in,
          # which is the whole development loop and needs no second machine.
          nested = {
            type = "app";
            program = toString (env.pkgs.writeShellScript "latticewm-nested" ''
              exec ${env.pkgs.river}/bin/river -log-level info \
                -c "${env.latticewm}/bin/latticewm --log-level debug"
            '');
          };
        });

      devShells = forEach (system:
        let env = envFor system; in {
          default = env.pkgs.mkShell {
            packages = [
              env.lisp env.pkgs.river env.pkgs.foot env.pkgs.grim
              env.pkgs.wayland-utils env.pkgs.git
            ];
            WAYFLAN_SRC = env.wayflanSrc;
            # RIVER is the store directory, which is what session.lisp's
            # re-vendor recipe reads; RIVER_BIN is the executable, which is
            # what install.sh reads.  One name did both and the launcher
            # written at install time execed a directory.  See shell.nix.
            #
            # RIVER_VERSION used to be exported here and in shell.nix and was
            # read by nothing at all; the version anyone needs is in
            # src/protocol/PINNED, which both nix files already parse.
            RIVER = "${env.pkgs.river}";
            RIVER_BIN = "${env.pkgs.river}/bin/river";
            shellHook = ''
              export LATTICEWM_ROOT=$PWD
              export CL_SOURCE_REGISTRY="$WAYFLAN_SRC//:$PWD//"
              echo "LatticeWM dev shell.  river ${env.pkgs.river.version}."
              echo "  make          build + gates + tests"
              echo "  make run      river nested in this session"
            '';
          };
        });

      # A NixOS module, so that the pair really can be upgraded as a pair.
      nixosModules.default = { config, lib, pkgs, ... }:
        with lib;
        let cfg = config.programs.latticewm;
        in {
          options.programs.latticewm = {
            enable = mkEnableOption "LatticeWM, a window manager for river";
            package = mkOption {
              type = types.package;
              default = self.packages.${pkgs.system}.latticewm;
              description = "The LatticeWM package to use.";
            };
            river = mkOption {
              type = types.package;
              # THIS WAS `pkgs.river', WHICH IS THE USER'S NIXPKGS AND NOT THIS
              # ONE — so the module whose whole purpose is pinning the pair
              # pinned the window manager and let the compositor float, and the
              # description below said the opposite in as many words.  On a
              # system whose nixpkgs held a different river, `nixos-rebuild'
              # succeeded and the failure arrived at a login screen.  That is
              # the exact shape of the bug this release exists to fix, in the
              # one place where the person hitting it has no shell to read the
              # refusal in.
              default = self.packages.${pkgs.system}.river;
              description = ''
                The river to run it under.  Defaults to the one this flake
                locks, which is the one the protocol in src/protocol/ was
                vendored from.  Pinning this alongside the window manager is
                the point of the module: the protocol is young, and a river
                upgrade that changes it should be a deliberate act rather than
                something that happens on a Tuesday.
              '';
            };
          };
          config = mkIf cfg.enable {
            # AND THE PAIR IS ASSERTED, not merely defaulted.  Overriding
            # `river' is legitimate — a re-vendored tree wants a newer one —
            # but overriding it with a river the vendored protocol does not
            # match produces a window manager that refuses to start, and the
            # place it refuses is a login screen with no terminal in front of
            # it.  Failing `nixos-rebuild' instead is the whole argument of
            # this commit, applied to the one path that reaches a user who
            # never ran `make'.
            assertions =
              let
                pinned =
                  let
                    firstLine = builtins.head (builtins.split "\n"
                      (builtins.readFile (self + "/src/protocol/PINNED")));
                    matched = builtins.match "river ([0-9][0-9.]*).*" firstLine;
                  in
                  if matched == null then null else builtins.head matched;
                running = cfg.river.version or "";
              in
              [{
                assertion = pinned == null || running == "" || running == pinned;
                message = ''
                  programs.latticewm.river is river ${running}, but the protocol
                  in ${cfg.package.pname or "latticewm"} was vendored from river
                  ${toString pinned}.  LatticeWM checks river-window-management-v1's
                  version at startup and refuses to run on a mismatch, so this
                  would fail at your login screen rather than here.

                  Either leave programs.latticewm.river at its default, which is
                  the river this flake locks, or re-vendor the protocol -- see
                  INSTALL.org, "Moving to a newer river".
                '';
              }];

            environment.systemPackages = [ cfg.package cfg.river ];

            # THE SESSION ENTRY IS BUILT HERE RATHER THAN TAKEN FROM THE
            # PACKAGE, because the package's own .desktop hardcodes the river
            # this flake locked at build time — so `programs.latticewm.river'
            # was an option that changed which river you had installed and not
            # which one your session ran.  Setting it did nothing visible and
            # nothing said so.  Now it is the option it says it is, and the
            # assertion above is about the river that will actually run.
            services.displayManager.sessionPackages = [
              (pkgs.writeTextFile {
                name = "latticewm-session";
                destination = "/share/wayland-sessions/latticewm.desktop";
                text = ''
                  [Desktop Entry]
                  Name=LatticeWM
                  Comment=An extensible window manager for river
                  Exec=${cfg.river}/bin/river -c ${cfg.package}/bin/latticewm
                  Type=Application
                '';
                # sessionPackages wants to know the session name without
                # reading the file.
                passthru.providedSessions = [ "latticewm" ];
              })
            ];
            # river needs these to be useful at all.
            security.polkit.enable = true;
            hardware.graphics.enable = mkDefault true;
            xdg.portal.enable = mkDefault true;
            xdg.portal.wlr.enable = mkDefault true;
          };
        };
    };
}
