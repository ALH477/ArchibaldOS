{
  description = "Nix flake for HydraMesh development environment";

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
          name = "quicklisp-2025-06-22";
          src = ./.; # Contains quicklisp.lisp, quicklisp.lisp.asc, distinfo.txt
          nativeBuildInputs = with pkgs; [ sbcl gnupg coreutils cacert ];
          buildInputs = [ streamdb.packages.${system}.default ];
          buildPhase = ''
            echo "Verifying quicklisp.lisp integrity..."
            echo "4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17  quicklisp.lisp" | sha256sum -c || { echo "Error: SHA256 mismatch for quicklisp.lisp"; exit 1; }
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

      in {
        packages = {
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
            export LD_LIBRARY_PATH=${streamdb.packages.${system}.default}/lib:$LD_LIBRARY_PATH
            if [ ! -d "$HOME/quicklisp" ]; then
              mkdir -p $HOME/quicklisp
              cp -r ${quicklisp}/etc/quicklisp/* $HOME/quicklisp/
            fi
            if [ -f src/hydramesh.lisp ]; then
              sed -i 's/(in-package :d-lisp)/(in-package :hydramesh)/g' src/hydramesh.lisp
            fi
            echo "Quicklisp available at $HOME/quicklisp. Load the project with: sbcl --load quicklisp/setup.lisp --load /etc/hydramesh/src/hydramesh.lisp"
            echo "Emacs with SLIME is available. Run 'emacs' to start."
            echo "StreamDB library at: ${streamdb.packages.${system}.default}/lib/libstreamdb.so"
          '';
        };
      }
    );
}
