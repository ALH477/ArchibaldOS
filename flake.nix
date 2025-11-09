{
  description = "Lean RT Audio ArchibaldOS: Minimal Oligarchy NixOS variant for real-time audio with Musnix, DWM";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    musnix.url = "github:musnix/musnix";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, musnix, disko }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };

    wallpaperSrc = ./modules/assets/wallpaper.jpg;

  in {
    nixosConfigurations = {
      archibaldOS = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
          musnix.nixosModules.musnix
          ./modules/audio.nix
          ./modules/desktop.nix
          ./modules/users.nix
          ./modules/installer.nix
          ./modules/branding.nix
          ({ config, pkgs, lib, ... }: {
            nixpkgs.config.permittedInsecurePackages = [ "qtwebengine-5.15.19" ]; # Allow insecure qtwebengine for build

            environment.systemPackages = with pkgs; [
              usbutils libusb1 alsa-firmware alsa-tools
            ];

            hardware.graphics.enable = true;
            hardware.graphics.extraPackages = with pkgs; [
              mesa
              vaapiIntel
              vaapiVdpau
              libvdpau-va-gl
              intel-media-driver  # For Intel GPUs
              # amdvlk  # Uncomment for AMD GPUs
            ];
            isoImage.squashfsCompression = "gzip -Xcompression-level 1";
            nix.settings.experimental-features = [ "nix-command" "flakes" ];

            branding = {
              enable = true;
              asciiArt = true;
              splash = true;
              wallpaper = true;
              # waybarIcons = true;  # Removed: Waybar is Hyprland-specific
            };

            users.users.nixos = {
              initialHashedPassword = lib.mkForce null;
              home = "/home/nixos";
              createHome = true;
              extraGroups = [ "audio" "jackaudio" "video" "networkmanager" ];
              shell = lib.mkForce pkgs.bashInteractive;
            };

            # Override audio-user as a minimal system user in live ISO (hides from SDDM, satisfies assertions)
            users.users.audio-user = lib.mkForce {
              isSystemUser = true;
              group = "audio-user";
              description = "Disabled in live ISO";
            };
            users.groups.audio-user = {};

            # Autologin to nixos user with DWM (X11)
            services.displayManager.autoLogin.enable = true;
            services.displayManager.autoLogin.user = "nixos";
            services.displayManager.defaultSession = "none+dwm";  # Default to DWM

            # Ensure SDDM hides audio-user explicitly (redundant but safe)
            services.displayManager.sddm.settings = {
              Users.HideUsers = "audio-user";
            };

            # Create screenshot directory for DWM in live ISO
            system.activationScripts.mkdirScreenshots = {
              text = ''
                mkdir -p /home/nixos/Pictures/Screenshots
                chown nixos:users /home/nixos/Pictures/Screenshots
              '';
            };

            # Optional NVIDIA support (uncomment if needed)
            # hardware.nvidia.modesetting.enable = true;
            # hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
            # boot.kernelParams = [ "nvidia-drm.modeset=1" ];
          })
        ];
      };
    };

    packages.${system}.installer = self.nixosConfigurations.archibaldOS.config.system.build.isoImage;

    devShells.${system}.default = pkgs.mkShell {
      packages = with pkgs; [
        ardour audacity fluidsynth musescore guitarix
        csound faust portaudio rtaudio supercollider qjackctl
        surge
        pcmanfm vim brave
      ];
    };
  };
}
