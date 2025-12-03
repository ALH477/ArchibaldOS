# flake.nix
{
  description = "ArchibaldOS: Unified NixOS for real-time audio (x86_64 ISO + ARM SBC)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    musnix.url = "github:musnix/musnix";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    nixos-rk3588.url = "github:ryan4yin/nixos-rk3588";
    nixos-rk3588.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, musnix, disko, nixos-rk3588 }: let
    x86System = "x86_64-linux";
    armSystem = "aarch64-linux";
    
    mkPkgs = system: import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };

  in {
    nixosConfigurations = {
      # === x86_64 Live ISO ===
      archibaldOS-iso = nixpkgs.lib.nixosSystem {
        system = x86System;
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
          musnix.nixosModules.musnix
          ./modules/base.nix
          ./modules/audio.nix
          ./modules/desktop.nix
          ./modules/users.nix
          ./modules/branding.nix
          ({ config, pkgs, lib, ... }: {
            nixpkgs.config.permittedInsecurePackages = [ "qtwebengine-5.15.19" ];

            # x86-specific audio config
            musnix = {
              enable = true;
              kernel.realtime = true;
              kernel.packages = pkgs.linuxPackages_latest_rt;
              alsaSeq.enable = true;
              rtirq.enable = true;
              das_watchdog.enable = true;
            };

            # x86-specific audio packages (full suite)
            environment.systemPackages = with pkgs; [
              usbutils libusb1 alsa-firmware alsa-tools
              dialog disko mkpasswd networkmanager
              # Audio software
              audacity fluidsynth musescore guitarix
              csound csound-qt faust portaudio rtaudio supercollider qjackctl
              surge zrythm carla puredata cardinal helm zynaddsubfx vmpk qmidinet 
              faust2alsa faust2csound faust2jack dragonfly-reverb calf
            ];

            # x86 audio tuning
            boot.kernelParams = [
              "threadirqs"
              "isolcpus=1-3"
              "nohz_full=1-3"
              "intel_idle.max_cstate=1"
              "processor.max_cstate=1"
            ];

            boot.kernel.sysctl = {
              "vm.swappiness" = lib.mkForce 0;
              "fs.inotify.max_user_watches" = 600000;
            };

            powerManagement.cpuFreqGovernor = "performance";

            hardware.graphics.enable = true;
            hardware.graphics.extraPackages = with pkgs; [
              mesa vaapiIntel vaapiVdpau libvdpau-va-gl amdvlk
            ];

            isoImage.squashfsCompression = "gzip -Xcompression-level 1";
            nix.settings.experimental-features = [ "nix-command" "flakes" ];

            branding = {
              enable = true;
              asciiArt = true;
              splash = true;
              wallpapers = true;
            };

            # Live ISO user setup
            users.users.nixos = {
              isNormalUser = true;
              initialHashedPassword = lib.mkForce null;
              initialPassword = "nixos";
              home = "/home/nixos";
              createHome = true;
              extraGroups = [ "wheel" "audio" "jackaudio" "video" "networkmanager" ];
              shell = lib.mkForce pkgs.bashInteractive;
            };

            # Disable audio-user in live ISO
            users.users.audio-user = lib.mkForce {
              isSystemUser = true;
              group = "audio-user";
              description = "Disabled in live ISO";
            };
            users.groups.audio-user = {};

            # Autologin
            services.displayManager.autoLogin = {
              enable = true;
              user = "nixos";
            };

            services.displayManager.sddm.settings = {
              Users.HideUsers = "audio-user";
            };

            system.activationScripts.mkdirScreenshots = {
              text = ''
                mkdir -p /home/nixos/Pictures/Screenshots
                chown nixos:users /home/nixos/Pictures/Screenshots
              '';
            };
          })
        ];
      };

      # === Orange Pi 5 (RK3588) ===
      archibaldOS-orangepi5 = nixpkgs.lib.nixosSystem {
        system = armSystem;
        modules = [
          nixos-rk3588.nixosModules.orangepi5
          musnix.nixosModules.musnix
          ./modules/base.nix
          ./modules/audio.nix
          ./modules/desktop.nix
          ./modules/users.nix
          ./modules/branding.nix
          ./modules/orange-pi-5.nix
          ({ config, pkgs, lib, ... }: {
            # ARM-specific: use board kernel, not RT
            musnix = {
              enable = true;
              kernel.realtime = false;
              rtirq = {
                enable = true;
                highList = "snd_usb_audio";
              };
              das_watchdog.enable = true;
            };

            # Lightweight audio packages for ARM cross-compilation
            environment.systemPackages = with pkgs; [
              jack2 qjackctl jack_capture
              guitarix qtractor puredata
              pavucontrol helvum qpwgraph jalv
            ];

            # ARM-specific kernel params
            boot.kernelParams = [
              "threadirqs"
              "cpufreq.default_governor=performance"
              "nohz_full=1-7"
            ];

            powerManagement.cpuFreqGovernor = "performance";

            branding = {
              enable = true;
              asciiArt = true;
              splash = false;
              wallpapers = true;
            };

            # Production audio-user
            users.users.audio-user = {
              isNormalUser = true;
              extraGroups = [ "audio" "jackaudio" "realtime" "video" "wheel" ];
            };

            # Auto-login for SBC
            services.displayManager.autoLogin = {
              enable = true;
              user = "audio-user";
            };

            nix.settings.experimental-features = [ "nix-command" "flakes" ];
          })
        ];
      };

      # === Generic ARM SBC ===
      archibaldOS-arm-generic = nixpkgs.lib.nixosSystem {
        system = armSystem;
        modules = [
          musnix.nixosModules.musnix
          ./modules/base.nix
          ./modules/audio.nix
          ./modules/desktop.nix
          ./modules/users.nix
          ./modules/branding.nix
          ({ config, pkgs, lib, ... }: {
            musnix = {
              enable = true;
              kernel.realtime = false;
              rtirq.enable = true;
              das_watchdog.enable = true;
            };

            environment.systemPackages = with pkgs; [
              jack2 qjackctl guitarix qtractor puredata
              pavucontrol helvum qpwgraph
            ];

            boot.kernelParams = [ "threadirqs" "cpufreq.default_governor=performance" ];
            powerManagement.cpuFreqGovernor = "performance";

            branding = {
              enable = true;
              asciiArt = true;
              splash = false;
              wallpapers = true;
            };

            users.users.audio-user = {
              isNormalUser = true;
              extraGroups = [ "audio" "jackaudio" "realtime" "video" "wheel" ];
            };

            services.displayManager.autoLogin = {
              enable = true;
              user = "audio-user";
            };

            nix.settings.experimental-features = [ "nix-command" "flakes" ];
          })
        ];
      };

      # === Raspberry Pi 3 Model B (Headless Optimized) ===
      archibaldOS-rpi3b = nixpkgs.lib.nixosSystem {
        system = armSystem;
        modules = [
          "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
          musnix.nixosModules.musnix
          ./modules/base.nix
          ./modules/audio.nix
          ./modules/users.nix
          ({ config, pkgs, lib, ... }: {
            boot.loader.grub.enable = false;
            boot.loader.raspberryPi = {
              enable = true;
              version = 3;
              uboot.enable = true;
              firmwareConfig = ''
                dtparam=audio=on
                force_turbo=1
                arm_freq=1200
                gpu_mem=16
                disable_overscan=1
              '';
            };

            hardware.enableRedistributableFirmware = true;

            musnix = {
              enable = true;
              kernel.realtime = false;
              rtirq = {
                enable = true;
                highList = "snd_usb_audio";
              };
              das_watchdog.enable = true;
            };

            services.pipewire.extraConfig.pipewire."92-low-latency" = lib.mkForce {
              "context.properties" = {
                "default.clock.rate" = 48000;
                "default.clock.quantum" = 128;
                "default.clock.min-quantum" = 128;
                "default.clock.max-quantum" = 128;
              };
            };

            security.pam.loginLimits = [
              { domain = "@audio"; type = "-"; item = "rtprio"; value = "99"; }
              { domain = "@audio"; type = "-"; item = "memlock"; value = "unlimited"; }
              { domain = "@audio"; type = "-"; item = "nice"; value = "-19"; }
            ];

            users.groups.realtime = {};

            environment.systemPackages = with pkgs; [
              jack2 pipewire alsa-utils usbutils
              vim git htop tmux
              (pkgs.writeShellScriptBin "rt-check" ''
                #!/usr/bin/env bash
                echo "=== RT Check (RPi3B) ==="
                uname -r
                cat /sys/kernel/realtime || echo "RT not enabled"
                ulimit -r
                systemctl --user status pipewire
              '')
            ];

            boot.kernelParams = lib.mkForce [
              "threadirqs"
              "isolcpus=2-3"
              "nohz_full=2-3"
              "rcu_nocbs=2-3"
              "cpufreq.default_governor=performance"
              "cma=128M"
              "coherent_pool=1M"
            ];

            powerManagement.cpuFreqGovernor = "performance";

            boot.kernel.sysctl = lib.mkForce {
              "vm.swappiness" = 0;
              "fs.inotify.max_user_watches" = 600000;
            };

            services.xserver.enable = false;
            services.displayManager.enable = false;
            hardware.opengl.enable = false;
            hardware.graphics.enable = false;

            users.users.audio = {
              isNormalUser = true;
              extraGroups = [ "wheel" "audio" "jackaudio" "realtime" "networkmanager" ];
              initialPassword = "changeme";
            };

            services.displayManager.autoLogin.enable = false;

            nix.settings.experimental-features = [ "nix-command" "flakes" ];

            networking.hostName = "archibaldos-rpi3b";

            sdImage.imageName = "archibaldos-rpi3b-headless.img";
            sdImage.compressImage = true;

            boot.kernelModules = [ "snd_usb_audio" "snd_bcm2835" ];
          })
        ];
      };

      # === Server variant (headless) ===
      archibaldOS-server = nixpkgs.lib.nixosSystem {
        system = x86System;
        modules = [
          musnix.nixosModules.musnix
          ./modules/base.nix
          ./modules/server.nix
          ./modules/users.nix
          ({ config, pkgs, lib, ... }: {
            musnix.enable = false;
            
            users.users.admin = {
              isNormalUser = true;
              extraGroups = [ "wheel" "docker" ];
              openssh.authorizedKeys.keys = [
                # Add your SSH keys here
              ];
            };

            nix.settings.experimental-features = [ "nix-command" "flakes" ];
          })
        ];
      };
    };

    packages = {
      ${x86System} = {
        iso = self.nixosConfigurations.archibaldOS-iso.config.system.build.isoImage;
      };
      ${armSystem} = {
        orangepi5 = self.nixosConfigurations.archibaldOS-orangepi5.config.system.build.sdImage;
        generic = self.nixosConfigurations.archibaldOS-arm-generic.config.system.build.toplevel;
        rpi3b = self.nixosConfigurations.archibaldOS-rpi3b.config.system.build.sdImage;
      };
    };

    devShells = {
      ${x86System}.default = (mkPkgs x86System).mkShell {
        packages = with (mkPkgs x86System); [
          audacity ardour fluidsynth musescore guitarix
          csound faust portaudio rtaudio supercollider qjackctl
          surge pcmanfm vim
        ];
      };
      
      ${armSystem}.default = (mkPkgs armSystem).mkShell {
        packages = with (mkPkgs armSystem); [
          jack2 guitarix puredata vim
        ];
      };
    };
  };
}
