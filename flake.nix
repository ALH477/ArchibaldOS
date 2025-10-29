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
          hydramesh-pkg = hydramesh.packages.${system}.hydramesh;
          streamdb-pkg = hydramesh.packages.${system}.streamdb;
        })
      ];
    };

    wallpaperSrc = ./modules/assets/wallpaper.jpg;

    sbclWithPkgs = pkgs.sbcl.withPackages (ps: with ps; [
      cffi cl-ppcre cl-json cl-csv usocket bordeaux-threads log4cl trivial-backtrace cl-store hunchensocket fiveam cl-dot cserial-port
      cl-lorawan cl-lsquic cl-can cl-sctp cl-zigbee
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
              usbutils libusb alsa-firmware alsa-tools sbclWithPkgs
            ] ++ [
              hydramesh.packages.${system}.toggleScript
              hydramesh.packages.${system}.statusScript
              hydramesh.packages.${system}.streamdb
            ];

            hardware.opengl.enable = true;
            isoImage.squashfsCompression = "gzip -Xcompression-level 1";
            nix.settings.experimental-features = [ "nix-command" "flakes" ];

            branding = {
              enable = true;
              asciiArt = true;
              splash = true;
              wallpaper = true;
              waybarIcons = true;
            };
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
      ] ++ [
        hydramesh.packages.${system}.toggleScript
        hydramesh.packages.${system}.statusScript
        hydramesh.packages.${system}.streamdb
      ];
    };
  };
}
