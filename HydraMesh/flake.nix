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

        quicklisp-dist = "2024-10-10";

        ql-packages = [
          "cl-protobufs" "cl-grpc" "cffi" "uuid" "cl-json" "cl-json-schema"
          "cl-ppcre" "cl-csv" "usocket" "bordeaux-threads" "curses"
          "log4cl" "trivial-backtrace" "cl-store" "mgl" "hunchensocket"
          "fiveam" "cl-dot" "cl-lsquic" "cl-serial" "cl-can" "cl-sctp"
          "cl-zigbee" "flexi-streams" "ieee-floats"
        ];

      in {
        packages = {
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
            pkgs.linuxPackages_latest.kernel
            (pkgs.emacs29.pkgs.withPackages (epkgs: [ epkgs.slime epkgs.slime-company epkgs.magit ]))
          ];

          shellHook = ''
            export HOME=$PWD
            if [ ! -d "$HOME/quicklisp" ]; then
              if [ -f quicklisp.lisp ] && [ -f distinfo.txt ]; then
                mkdir -p $HOME/quicklisp
                cp quicklisp.lisp $HOME/quicklisp/quicklisp.lisp
                cp distinfo.txt $HOME/quicklisp/distinfo.txt
              else
                echo "Error: quicklisp.lisp and distinfo.txt not found. Download them manually."
                echo "curl -O https://beta.quicklisp.org/quicklisp.lisp"
                echo "curl -O https://beta.quicklisp.org/dist/quicklisp/${quicklisp-dist}/distinfo.txt"
                exit 1
              fi
              ${sbcl}/bin/sbcl --no-userinit --no-sysinit --load $HOME/quicklisp/quicklisp.lisp \
                --eval '(quicklisp-quickstart:install :path "$HOME/quicklisp/")' \
                --quit || { echo "Error: Quicklisp installation failed"; exit 1; }
              ${sbcl}/bin/sbcl --load $HOME/quicklisp/setup.lisp \
                --eval '(ql-util:without-prompting (ql:update-client) (ql:update-dist "quicklisp" :dist-version "${quicklisp-dist}"))' \
                --quit || { echo "Error: Quicklisp update failed"; exit 1; }
              ${sbcl}/bin/sbcl --load $HOME/quicklisp/setup.lisp \
                --eval '(ql:quickload :quicklisp-slime-helper)' \
                --quit || { echo "Error: Quicklisp SLIME helper failed"; exit 1; }
            fi
            if [ -f src/hydramesh.lisp ]; then
              sed -i 's/(in-package :d-lisp)/(in-package :hydramesh)/g' src/hydramesh.lisp
            fi
            export LD_LIBRARY_PATH=${streamdb.packages.${system}.default}/lib:$LD_LIBRARY_PATH
            ${sbcl}/bin/sbcl --load $HOME/quicklisp/setup.lisp \
              --eval '(ql:quickload (list ${pkgs.lib.concatStringsSep " " (map (p: ":${p}") ql-packages)}))' \
              --eval '(format t "Quicklisp systems: ~A~%" (directory "$HOME/quicklisp/local-projects/*.asd"))' \
              --eval '(format t "Quicklisp installed systems: ~A~%" (directory "$HOME/quicklisp/dists/quicklisp/installed/systems/*.asd"))' \
              --eval '(format t "cl-protobufs available: ~A~%" (ql-dist:find-system "cl-protobufs"))' \
              --quit
            echo "Quicklisp set up with dist ${quicklisp-dist}."
            echo "Emacs with SLIME is available. Run 'emacs' to start."
            echo "To load HydraMesh in SLIME: M-x slime, then (load \"src/hydramesh.lisp\")"
            echo "StreamDB library available at: ${streamdb.packages.${system}.default}/lib/libstreamdb.so"
            echo "Kernel modules for audio DSP are available via linuxPackages_latest."
            echo "Troubleshooting: If Quicklisp fails, verify quicklisp.lisp and distinfo.txt, or check network."
          '';
        };
      }
    );
}
