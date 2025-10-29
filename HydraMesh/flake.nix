{
  description = "Nix flake for HydraMesh (D-LISP) SDK";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        sbcl = pkgs.sbcl;

        quicklisp-dist = "2023-10-21";
        quicklisp-setup = pkgs.writeShellScriptBin "setup-quicklisp.sh" ''
          mkdir -p $HOME/quicklisp
          curl -O https://beta.quicklisp.org/quicklisp.lisp
          ${sbcl}/bin/sbcl --no-userinit --no-sysinit --load quicklisp.lisp \
            --eval '(quicklisp-quickstart:install :path "~/quicklisp/")' \
            --eval '(ql-util:without-prompting (ql:update-client) (ql:update-dist "quicklisp" :dist-version "${quicklisp-dist}"))' \
            --eval '(ql:quickload :ql-dist)' \
            --quit
        '';

        ql-packages = [
          "cl-protobufs" "cl-grpc" "cffi" "uuid" "cl-json" "jsonschema"
          "cl-ppcre" "cl-csv" "usocket" "bordeaux-threads" "curses"
          "log4cl" "trivial-backtrace" "cl-store" "mgl" "hunchensocket"
          "fiveam" "cl-dot" "cl-lsquic" "cl-serial" "cl-can" "cl-sctp"
          "cl-zigbee" "cl-lorawan"
        ];

        load-deps = pkgs.writeShellScriptBin "load-deps.lisp" ''
          (load "~/quicklisp/setup.lisp")
          (ql:quickload '(${pkgs.lib.concatStringsSep " " (map (p: ":${p}") ql-packages)}))
          (quit)
        '';

        hydramesh = pkgs.stdenv.mkDerivation {
          name = "hydramesh";
          src = self;

          nativeBuildInputs = [ sbcl quicklisp-setup ];

          buildPhase = ''
            export HOME=$PWD
            setup-quicklisp.sh
            ${sbcl}/bin/sbcl --script ${load-deps}/bin/load-deps.lisp
            ${sbcl}/bin/sbcl --no-userinit --load hydramesh.lisp \
              --eval '(dcf-deploy "dcf-lisp")' \
              --quit
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp dcf-lisp $out/bin/dcf-lisp
          '';
        };

      in {
        packages.default = hydramesh;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            sbcl
            quicklisp-setup
            pkgs.grpc
            pkgs.protobuf
            pkgs.openssl
            pkgs.zlib
          ];

          shellHook = ''
            export HOME=$PWD
            if [ ! -d "$HOME/quicklisp" ]; then
              setup-quicklisp.sh
            fi
            echo "Quicklisp set up with dist ${quicklisp-dist}. Load deps with: sbcl --script ${load-deps}/bin/load-deps.lisp"
            echo "Load the project: sbcl --load hydramesh.lisp"
          '';
        };
      }
    );
}
