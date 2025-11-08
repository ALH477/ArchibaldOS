{
  description = "Nix flake for HydraMesh SDK";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    streamdb = {
      url = "path:./streamdb";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, streamdb }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
        lib = pkgs.lib;

        sbcl = pkgs.sbcl;

        quicklisp-dist = "2025-06-22";

        ql-packages = [
          "cl-protobufs" "cl-grpc" "cffi" "uuid" "cl-json" "cl-json-schema"
          "cl-ppcre" "cl-csv" "usocket" "bordeaux-threads" "curses"
          "log4cl" "trivial-backtrace" "cl-store" "mgl" "hunchensocket"
          "fiveam" "cl-dot" "cl-lsquic" "cl-serial" "cl-can" "cl-sctp"
          "cl-zigbee" "flexi-streams" "ieee-floats"
        ];

quicklisp = pkgs.stdenv.mkDerivation {
  name = "quicklisp-${quicklisp-dist}";
  src = ./.; # Contains quicklisp.lisp, quicklisp.lisp.asc, distinfo.txt, release-key.txt
  nativeBuildInputs = with pkgs; [ sbcl gnupg coreutils cacert ];
  buildInputs = [ streamdb.packages.${system}.default ];
  buildPhase = ''
    echo "Verifying quicklisp.lisp integrity..."
    echo "4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17  quicklisp.lisp" | sha256sum -c || { echo "Error: SHA256 mismatch for quicklisp.lisp"; exit 1; }

    # Create a writable GPG home directory
    mkdir -p gnupg_home
    chmod 700 gnupg_home
    export GNUPGHOME=$PWD/gnupg_home

    # Check for and import Quicklisp public key
    if [ ! -f release-key.txt ]; then
      echo "Error: release-key.txt not found in HydraMesh directory. Download from https://beta.quicklisp.org/release-key.txt"
      exit 1
    fi
    gpg --import release-key.txt || { echo "Error: Failed to import release-key.txt"; exit 1; }

    # Verify the signature
    if [ ! -f quicklisp.lisp.asc ]; then
      echo "Error: quicklisp.lisp.asc not found in HydraMesh directory."
      exit 1
    fi
    gpg --verify quicklisp.lisp.asc quicklisp.lisp || { echo "Error: PGP signature verification failed"; exit 1; }

    if [ ! -f distinfo.txt ]; then
      echo "Error: distinfo.txt not found in HydraMesh directory. Download from http://beta.quicklisp.org/dist/quicklisp/2025-06-22/distinfo.txt"
      exit 1
    fi
    mkdir -p quicklisp/local-projects quicklisp/dists/quicklisp/installed/systems
    cp quicklisp.lisp quicklisp/quicklisp.lisp
    cp distinfo.txt quicklisp/distinfo.txt
    ${sbcl}/bin/sbcl --no-userinit --no-sysinit --load quicklisp/quicklisp.lisp \
      --eval '(quicklisp-quickstart:install :path "quicklisp/")' \
      --quit || { echo "Error: Quicklisp installation failed"; exit 1; }
    ${sbcl}/bin/sbcl --load quicklisp/setup.lisp \
      --eval '(ql-util:without-prompting (ql:update-client) (ql:update-dist "quicklisp" :dist-version "${quicklisp-dist}"))' \
      --quit || { echo "Error: Quicklisp update failed"; exit 1; }
    ${sbcl}/bin/sbcl --load quicklisp/setup.lisp \
      --eval '(ql:quickload :quicklisp-slime-helper)' \
      --quit || { echo "Error: Quicklisp SLIME helper failed"; exit 1; }
    ${sbcl}/bin/sbcl --load quicklisp/setup.lisp \
      --eval '(ql:quickload (list ${pkgs.lib.concatStringsSep " " (map (p: ":${p}") ql-packages)}))' \
      --eval '(format t "Quicklisp systems: ~A~%" (directory "quicklisp/local-projects/*.asd"))' \
      --eval '(format t "Quicklisp installed systems: ~A~%" (directory "quicklisp/dists/quicklisp/installed/systems/*.asd"))' \
      --eval '(format t "cl-protobufs available: ~A~%" (ql-dist:find-system "cl-protobufs"))' \
      --quit || { echo "Error: Quicklisp package loading failed"; exit 1; }
  '';
  installPhase = ''
    mkdir -p $out/etc/quicklisp
    cp -r quicklisp/* $out/etc/quicklisp/
  '';
};

        load-quicklisp = pkgs.writeTextFile {
          name = "load-quicklisp.lisp";
          text = ''
            (load "/etc/quicklisp/setup.lisp")
          '';
          destination = "/load-quicklisp.lisp";
        };

        hydramesh = pkgs.stdenv.mkDerivation {
          name = "hydramesh";
          src = ./src; # Point to ./src where hydramesh.lisp is located

          nativeBuildInputs = [ sbcl pkgs.makeWrapper ];
          buildInputs = [ streamdb.packages.${system}.default quicklisp ];

          buildPhase = ''
            export HOME=$PWD
            mkdir -p $HOME/quicklisp
            cp -r ${quicklisp}/etc/quicklisp/* $HOME/quicklisp/
            export LD_LIBRARY_PATH=${streamdb.packages.${system}.default}/lib:$LD_LIBRARY_PATH
            ${sbcl}/bin/sbcl --load ${load-quicklisp}/load-quicklisp.lisp \
              --eval '(ql:quickload (list ${pkgs.lib.concatStringsSep " " (map (p: ":${p}") ql-packages)}))' \
              --eval '(defpackage :hydramesh (:use :cl :bordeaux-threads))' \
              --eval '(in-package :hydramesh)' \
              --load hydramesh.lisp \
              --eval '(sb-ext:save-lisp-and-die "hydramesh" :toplevel #'(lambda () (in-package :hydramesh) (hydramesh-init "/etc/hydramesh/config.json" :restore-state t) (hydramesh-start) 0) :executable t)' \
              --quit
          '';

          installPhase = ''
            mkdir -p $out/bin $out/lib $out/include
            cp hydramesh $out/bin/hydramesh
            chmod +x $out/bin/hydramesh
            makeWrapper $out/bin/hydramesh $out/bin/hydramesh-wrapped \
              --set LD_LIBRARY_PATH ${streamdb.packages.${system}.default}/lib
            cp ${streamdb.packages.${system}.default}/lib/libstreamdb.so $out/lib/
            cp ${streamdb.packages.${system}.default}/include/libstreamdb.h $out/include/
          '';
        };

        toggleScript = pkgs.writeShellScriptBin "hydramesh-toggle" ''
          #!/usr/bin/env bash
          set -euo pipefail

          STATUS_FILE="/var/lib/hydramesh/hydramesh-status"
          SERVICE="hydramesh"

          notify() {
            ${pkgs.hyprland}/bin/hyprctl notify -1 4000 "$1" "HydraMesh" "$2" || true
          }

          if systemctl is-active --quiet "$SERVICE"; then
            systemctl stop "$SERVICE"
            notify "rgb(ff3333)" "Service stopped"
            echo "OFF" > "$STATUS_FILE"
          else
            systemctl start "$SERVICE"
            notify "rgb(33ff33)" "Service started"
            echo "ON" > "$STATUS_FILE"
          fi
        '';

        statusScript = pkgs.writeShellScriptBin "hydramesh-status" ''
          #!/usr/bin/env bash
          set -euo pipefail

          SERVICE="hydramesh"
          ICON_ON="/etc/waybar/hydramesh-on.svg"
          ICON_OFF="/etc/waybar/hydramesh-off.svg"

          if systemctl is-active --quiet "$SERVICE"; then
            cat <<EOF
{"text":"ON","class":"hydramesh-active","tooltip":"HydraMesh running","icon":"$ICON_ON"}
EOF
          else
            cat <<EOF
{"text":"OFF","class":"hydramesh-inactive","tooltip":"HydraMesh stopped","icon":"$ICON_OFF"}
EOF
          fi
        '';

      in {
        packages = {
          default = hydramesh;
          inherit hydramesh toggleScript statusScript;
          streamdb = streamdb.packages.${system}.default;
          quicklisp = quicklisp;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            sbcl
            pkgs.grpc
            pkgs.protobuf
            pkgs.openssl
            pkgs.zlib
            pkgs.cacert
            streamdb.packages.${system}.default
            pkgs.linuxPackages_latest.kernel
            (pkgs.emacs29.pkgs.withPackages (epkgs: [ epkgs.slime epkgs.slime-company epkgs.magit ]))
            pkgs.gnupg
            pkgs.coreutils
            pkgs.gnused
          ];
          shellHook = ''
            export HOME=$PWD
            if [ ! -d "$HOME/quicklisp" ]; then
              mkdir -p $HOME/quicklisp
              cp -r ${quicklisp}/etc/quicklisp/* $HOME/quicklisp/
            fi
            if [ -f src/hydramesh.lisp ]; then
              sed -i 's/:d-lisp/:hydramesh/g' src/hydramesh.lisp
            fi
            export LD_LIBRARY_PATH=${streamdb.packages.${system}.default}/lib:$LD_LIBRARY_PATH
            echo "Quicklisp available at $HOME/quicklisp. Load the project with: sbcl --load quicklisp/setup.lisp --load src/hydramesh.lisp"
            echo "Emacs with SLIME is available. Run 'emacs' to start."
            echo "StreamDB library at: ${streamdb.packages.${system}.default}/lib/libstreamdb.so"
          '';
        };
      }
    );
}
