# HydraMesh/flake.nix
# Copyright 2025 DeMoD LLC
# SPDX-License-Identifier: LGPL-3.0

{
  description = "Nix flake for HydraMesh (D-LISP) SDK v2.2.0 – Emacs + SLY focused development";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
        };

        sbclWithDeps = pkgs.sbcl.withPackages (ps: with ps; [
          cffi uuid cl-protobufs usocket bordeauxThreads
          log4cl trivial-backtrace flexiStreams fiveam
          ieee-floats cl-json jsonschema
        ]);

        emacsWithSly = pkgs.emacsNativeComp.pkgs.withPackages (epkgs: [
          epkgs.sly
        ]);

        hydramesh = pkgs.stdenv.mkDerivation {
          pname = "hydramesh";
          version = "2.2.0";

          src = self;

          nativeBuildInputs = [ sbclWithDeps ];

          dontStrip = true;

          buildPhase = ''
            ${sbclWithDeps}/bin/sbcl --no-userinit --non-interactive \
              --load hydramesh.lisp \
              --eval '(dcf-deploy "hydramesh")' \
              --quit
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp hydramesh $out/bin/hydramesh
          '';

          meta = with pkgs.lib; {
            description = "HydraMesh SDK executable";
            license = licenses.lgpl3Only;
            platforms = platforms.all;
            mainProgram = "hydramesh";
          };
        };

      in {
        packages.default = hydramesh;
        packages.hydramesh = hydramesh;

        # Required for ArchibaldOS integration
        overlays.default = final: prev: {
          inherit hydramesh;
        };

        # Export the NixOS module (place the service file at ./nixos-module.nix)
        nixosModules.default = ./nixos-module.nix;
        nixosModules.hydramesh = ./nixos-module.nix;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            sbclWithDeps
            emacsWithSly
            pkgs.grpc
            pkgs.protobuf
            pkgs.openssl
            pkgs.zlib
            pkgs.roswell
          ];

          shellHook = ''
            mkdir -p $HOME/.emacs.d
            cat > $HOME/.emacs.d/init.el <<'EOF'
            (require 'sly)
            (setq inferior-lisp-program "${sbclWithDeps}/bin/sbcl")
            (add-hook 'lisp-mode-hook #'sly-mode)
            (add-hook 'slime-repl-mode-hook #'sly-mrepl-mode)
            (add-hook 'sly-mode-hook #'company-mode)
            EOF

            echo "══════════════════════════════════════════════════════════════"
            echo "HydraMesh v2.2.0 development shell ready!"
            echo "• Build: nix build"
            echo "• Run:   result/bin/hydramesh help"
            echo "• Emacs: emacs → M-x sly"
            echo "══════════════════════════════════════════════════════════════"
          '';
        };
      }
    );
}
