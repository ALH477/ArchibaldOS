{
  description = "Lean RT Audio ArchibaldOS: Minimal Oligarchy NixOS variant for real-time audio with Musnix, Hyprland, HydraMesh, and StreamDB";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    musnix.url = "github:musnix/musnix";
    hyprland.url = "github:hyprwm/Hyprland";
    hydramesh.url = "path:./HydraMesh";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, musnix, hyprland, hydramesh, disko }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        (final: prev: rec {
          hyprland = hyprland.packages.${system}.hyprland;
          streamdb-pkg = hydramesh.packages.${system}.streamdb;
          quicklisp = prev.stdenv.mkDerivation {
            name = "quicklisp-2025-06-22";
            src = ./HydraMesh; # Contains quicklisp.lisp, quicklisp.lisp.asc, distinfo.txt
            nativeBuildInputs = with prev; [ sbcl gnupg coreutils cacert ];
            buildInputs = [ hydramesh.packages.${system}.streamdb ];
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
              ${prev.sbcl}/bin/sbcl --no-userinit --no-sysinit --load quicklisp/quicklisp.lisp \
                --eval '(quicklisp-quickstart:install :path "quicklisp/")' \
                --quit || { echo "Error: Quicklisp installation failed"; exit 1; }
              ${prev.sbcl}/bin/sbcl --load quicklisp/setup.lisp \
                --eval '(ql-util:without-prompting (ql:update-client) (ql:update-dist "quicklisp" :dist-version "2025-06-22"))' \
                --quit || { echo "Error: Quicklisp update failed"; exit 1; }
              ${prev.sbcl}/bin/sbcl --load quicklisp/setup.lisp \
                --eval '(ql:quickload (list :cl-protobufs :cl-grpc :cffi :uuid :cl-json :cl-json-schema :cl-ppcre :cl-csv :usocket :bordeaux-threads :curses :log4cl :trivial-backtrace :cl-store :mgl :hunchensocket :fiveam :cl-dot :cl-lsquic :cl-serial :cl-can :cl-sctp :cl-zigbee :flexi-streams :ieee-floats))' \
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
        })
      ];
    };

    wallpaperSrc = ./modules/assets/wallpaper.jpg;

    sbclWithPkgs = pkgs.sbcl.withPackages (ps: with ps; [
      cffi cl-ppcre cl-json cl-csv usocket bordeaux-threads log4cl trivial-backtrace cl-store hunchensocket fiveam cl-dot cserial-port
    ]);

  in {
    nixosConfigurations = {
      archibaldOS = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
          musnix.nixosModules.musnix
          hyprland.nixosModules.default
          ./modules/audio.nix
          ./modules/desktop.nix
          ./modules/users.nix
          ./modules/installer.nix
          ./modules/hydramesh.nix
          ./modules/branding.nix
          ({ config, pkgs, lib, ... }: {
            environment.systemPackages = with pkgs; [
              usbutils libusb1 alsa-firmware alsa-tools sbclWithPkgs
              (emacs29.pkgs.withPackages (epkgs: [ epkgs.slime epkgs.slime-company epkgs.magit ]))
              streamdb-pkg
              quicklisp
            ];

            hardware.graphics.enable = true;
            isoImage.squashfsCompression = "gzip -Xcompression-level 1";
            nix.settings.experimental-features = [ "nix-command" "flakes" ];

            branding = {
              enable = true;
              asciiArt = true;
              splash = true;
              wallpaper = true;
              waybarIcons = true;
            };

            users.users.nixos = {
              initialHashedPassword = lib.mkForce null;
              home = "/home/nixos";
              createHome = true;
              extraGroups = [ "audio" ];
              shell = pkgs.bash;
            };

            environment.etc."emacs/init.el".text = ''
              (setq inferior-lisp-program "${pkgs.sbcl}/bin/sbcl --load /etc/quicklisp/setup.lisp")
              (require 'slime)
              (slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-company))
              (defun load-hydramesh () (interactive) (slime-load-file "/etc/hydramesh/src/hydramesh.lisp"))
              (defun start-hydramesh-daemon () (interactive) (slime-eval `(hydramesh-init "/etc/hydramesh/config.json" :restore-state t)) (slime-eval `(hydramesh-start)))
              (defun stop-hydramesh-daemon () (interactive) (slime-eval `(hydramesh-stop)))
              (defun check-hydramesh-status () (interactive) (shell-command "systemctl --user status hydramesh"))
              (defun run-hydramesh-tests () (interactive) (slime-eval '(fiveam:run! :hydramesh)))
              (global-set-key (kbd "C-c h") 'load-hydramesh)
              (global-set-key (kbd "C-c s") 'start-hydramesh-daemon)
              (global-set-key (kbd "C-c x") 'stop-hydramesh-daemon)
              (global-set-key (kbd "C-c c") 'check-hydramesh-status)
              (global-set-key (kbd "C-c t") 'run-hydramesh-tests)
            '';

            environment.etc."quicklisp".source = "${pkgs.quicklisp}/etc/quicklisp";
            environment.etc."quicklisp/setup.lisp".source = "${pkgs.quicklisp}/etc/quicklisp/setup.lisp";

            systemd.user.services.emacs = {
              description = "Emacs Daemon for HydraMesh Development";
              wantedBy = [ "default.target" ];
              serviceConfig = {
                ExecStart = "${pkgs.emacs29}/bin/emacs --daemon";
                Restart = "always";
              };
            };

            musnix.enable = true;
            musnix.kernel.realtime = true;
          })
        ];
      };
    };

    packages.${system}.installer = self.nixosConfigurations.archibaldOS.config.system.build.isoImage;

    devShells.${system}.default = pkgs.mkShell {
      packages = with pkgs; [
        ardour audacity fluidsynth musescore guitarix
        csound faust portaudio rtaudio supercollider qjackctl
        surge vcvrack pd
        pcmanfm vim brave
        sbclWithPkgs
        (emacs29.pkgs.withPackages (epkgs: [ epkgs.slime epkgs.slime-company epkgs.magit ]))
        quicklisp
      ];
      shellHook = ''
        export LD_LIBRARY_PATH=${streamdb-pkg}/lib:$LD_LIBRARY_PATH
        if [ ! -d "$HOME/quicklisp" ]; then
          mkdir -p $HOME/quicklisp
          cp -r ${pkgs.quicklisp}/etc/quicklisp/* $HOME/quicklisp/
        fi
        if [ -f src/hydramesh.lisp ]; then
          sed -i 's/(in-package :d-lisp)/(in-package :hydramesh)/g' src/hydramesh.lisp
        fi
        echo "Quicklisp available at $HOME/quicklisp. Load the project with: sbcl --load quicklisp/setup.lisp --load /etc/hydramesh/src/hydramesh.lisp"
        echo "Emacs with SLIME is available. Run 'emacs' to start."
        echo "StreamDB library at: ${streamdb-pkg}/lib/libstreamdb.so"
      '';
    };
  };
}
