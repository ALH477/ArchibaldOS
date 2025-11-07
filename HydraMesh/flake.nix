{
  description = "Nix flake for HydraMesh (D-LISP) SDK";

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
          nativeBuildInputs = [ pkgs.curl sbcl pkgs.cacert ];
          dontUnpack = true;

          buildPhase = ''
            mkdir -p quicklisp
            curl --cacert ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt -O https://beta.quicklisp.org/quicklisp.lisp
            sbcl --no-userinit --no-sysinit --load quicklisp.lisp \
              --eval '(quicklisp-quickstart:install :path "quicklisp/")' \
              --quit
            sbcl --load quicklisp/setup.lisp \
              --eval '(ql-util:without-prompting (ql:update-client))' \
              --quit
            sbcl --load quicklisp/setup.lisp \
              --eval '(ql-util:without-prompting (ql:update-dist "quicklisp" :dist-version "${quicklisp-dist}"))' \
              --quit
            sbcl --load quicklisp/setup.lisp \
              --eval '(ql:quickload :ql-dist)' \
              --quit
            sbcl --load quicklisp/setup.lisp \
              --eval '(ql:quickload '"'"'(${pkgs.lib.concatStringsSep " " (map (p: ":${p}") ql-packages)}))' \
              --quit
          '';

          installPhase = ''
            mkdir -p $out/quicklisp
            cp -r quicklisp/* $out/quicklisp/
          '';

          outputHashAlgo = "sha256";
          outputHashMode = "recursive";
          outputHash = "sha256-7NLtEW86jBC6sq8qrl9OUqb8K2cxgLMaXtnwnNDuF0E="; 
        };

        load-quicklisp = pkgs.writeTextFile {
          name = "load-quicklisp.lisp";
          text = ''
            (load "quicklisp/setup.lisp")
          '';
          destination = "/load-quicklisp.lisp";
        };

        hydramesh = pkgs.stdenv.mkDerivation {
          name = "hydramesh";
          src = ./src; # Point to ./src where hydramesh.lisp is located

          nativeBuildInputs = [ sbcl pkgs.makeWrapper ];
          buildInputs = [ streamdb.packages.${system}.default ];

          postPatch = ''
            sed -i '/(ql:quickload/,/))/d' hydramesh.lisp
          '';

            buildPhase = ''
            export HOME=$PWD
            mkdir -p $HOME/quicklisp
            cp -r ${quicklisp}/quicklisp/* $HOME/quicklisp/
            export LD_LIBRARY_PATH=${streamdb.packages.${system}.default}/lib:$LD_LIBRARY_PATH
            ${sbcl}/bin/sbcl --load ${load-quicklisp}/load-quicklisp.lisp \
              --eval '(ql:quickload '"'"'(:cffi :uuid :cl-protobufs :usocket :bordeaux-threads :log4cl :trivial-backtrace :flexi-streams :fiveam :ieee-floats :cl-json :cl-json-schema))' \
              --load hydramesh.lisp \
              --eval '(in-package :d-lisp)' \
              --eval '(dcf-deploy "dcf-lisp")' \
              --quit
          '';

          installPhase = ''
            mkdir -p $out/bin $out/lib $out/include
            cp dcf-lisp $out/bin/.dcf-lisp-unwrapped
            makeWrapper $out/bin/.dcf-lisp-unwrapped $out/bin/dcf-lisp \
              --set LD_LIBRARY_PATH $out/lib
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
            hyprctl notify -1 4000 "$1" "HydraMesh" "$2" || true
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
          ];

          shellHook = ''
            export HOME=$PWD
            if [ ! -d "$HOME/quicklisp" ]; then
              curl --cacert ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt -O https://beta.quicklisp.org/quicklisp.lisp
              ${sbcl}/bin/sbcl --no-userinit --no-sysinit --load quicklisp.lisp \
                --eval '(quicklisp-quickstart:install :path "$HOME/quicklisp/")' \
                --quit
              ${sbcl}/bin/sbcl --load $HOME/quicklisp/setup.lisp \
                --eval '(ql-util:without-prompting (ql:update-client) (ql:update-dist "quicklisp" :dist-version "${quicklisp-dist}"))' \
                --quit
            fi
            export LD_LIBRARY_PATH=${streamdb.packages.${system}.default}/lib:$LD_LIBRARY_PATH
            echo "Quicklisp set up with dist ${quicklisp-dist}. Load the project with: sbcl --load quicklisp/setup.lisp --load src/hydramesh.lisp"
            echo "StreamDB library available at: ${streamdb.packages.${system}.default}/lib/libstreamdb.so"
          '';
        };
      }
    );
}
