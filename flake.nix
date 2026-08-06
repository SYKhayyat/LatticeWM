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
            version = "0.1.0";
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
            # THREAT TO THE PROJECT.*  The pin above is `nixos-unstable', not a
            # river version — so the compositor moves when the lock moves, and
            # river bumped river_window_manager_v1 from 4 to 5 between 0.4.5 and
            # 0.4.6, which is a *patch* release.  The vendored XML is version 4.
            #
            # So this derivation was building a window manager that refuses to
            # start against the river the very same derivation puts in its
            # session entry: `nix build' succeeded, `install' wrote a
            # wayland-sessions file, and logging in got a version-mismatch
            # message at a login screen.  It succeeded because nothing in the
            # build phase had ever connected to the pinned river.  Now something
            # does.
            #
            # There are two honest ways out and they are not this file's to
            # choose: re-vendor the protocol from the pinned river and bump
            # +WINDOW-MANAGEMENT-VERSION+, or pin a river the vendored XML
            # matches.  `nix-shell' uses the ambient channel, which is 0.4.5 at
            # the time of writing, and works.
            buildPhase = ''
              export HOME=$TMPDIR
              export XDG_RUNTIME_DIR=$TMPDIR/run
              mkdir -p "$XDG_RUNTIME_DIR"
              export CL_SOURCE_REGISTRY="${wayflanSrc}//:$PWD//"
              sbcl --non-interactive --load tools/build.lisp
              sbcl --non-interactive --load tools/gates.lisp
              sbcl --non-interactive --load tools/test.lisp
              LATTICEWM_REQUIRE_INTEGRATION=1 \
                sbcl --non-interactive --load tools/integration.lisp
              sbcl --non-interactive --load tools/image.lisp
            '';

            installPhase = ''
              install -Dm755 latticewm $out/bin/latticewm
              install -Dm755 tools/wmeval $out/bin/wmeval
              for f in doc/*.txt doc/*.org *.org; do
                install -Dm644 "$f" "$out/share/latticewm/$(basename $f)"
              done
              mkdir -p $out/share/latticewm/examples
              cp examples/*.lisp $out/share/latticewm/examples/
              # The pinned protocol, so a version mismatch can be diagnosed
              # from an installed copy without the source tree.
              mkdir -p $out/share/latticewm/protocol
              cp src/protocol/*.xml $out/share/latticewm/protocol/

              mkdir -p $out/share/wayland-sessions
              cat > $out/share/wayland-sessions/latticewm.desktop <<EOF
              [Desktop Entry]
              Name=LatticeWM
              Comment=An extensible window manager for river
              Exec=${pkgs.river}/bin/river -c $out/bin/latticewm
              Type=Application
              EOF
            '';

            meta = with pkgs.lib; {
              description = "An extensible window manager for the river Wayland compositor";
              license = licenses.bsd3;
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
            RIVER = "${env.pkgs.river}";
            RIVER_VERSION = env.pkgs.river.version;
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
              default = pkgs.river;
              description = ''
                The river to run it under.  Pinning this alongside the window
                manager is the point of the module: the protocol is young, and
                a river upgrade that changes it should be a deliberate act
                rather than something that happens on a Tuesday.
              '';
            };
          };
          config = mkIf cfg.enable {
            environment.systemPackages = [ cfg.package cfg.river ];
            services.displayManager.sessionPackages = [ cfg.package ];
            # river needs these to be useful at all.
            security.polkit.enable = true;
            hardware.graphics.enable = mkDefault true;
            xdg.portal.enable = mkDefault true;
            xdg.portal.wlr.enable = mkDefault true;
          };
        };
    };
}
