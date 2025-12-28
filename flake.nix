# flake.nix
# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 DeMoD LLC. All rights reserved.
# ============================================================================
# ArchibaldOS Community Edition
# Real-time audio workstation + HydraMesh P2P networking for NixOS
#
# For commercial features (Thunderbolt/USB4, ARM support, auto-updates,
# enterprise configurations), see: https://github.com/ALH477/archibaldos-pro
# ============================================================================
{
  description = "ArchibaldOS Community Edition - Real-time audio workstation + HydraMesh";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    musnix.url = "github:musnix/musnix";
  };

  outputs = { self, nixpkgs, musnix }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };

    # Shared flake URI for updates
    flakeUri = "github:ALH477/ArchibaldOS";

    # Common RT kernel configuration
    rtKernelConfig = { config, pkgs, lib, ... }: {
      musnix = {
        enable = true;
        kernel.realtime = true;
        alsaSeq.enable = true;
        rtirq.enable = true;
        das_watchdog.enable = true;
      };

      boot.kernelParams = [
        "threadirqs"
        "isolcpus=1-3"
        "nohz_full=1-3"
        "intel_idle.max_cstate=1"
        "processor.max_cstate=1"
      ];

      powerManagement.cpuFreqGovernor = "performance";
    };

    # Common base configuration
    baseConfig = { config, pkgs, lib, ... }: {
      system.stateVersion = "24.11";
      isoImage.squashfsCompression = "gzip -Xcompression-level 1";
      nix.settings.experimental-features = [ "nix-command" "flakes" ];
      networking.networkmanager.enable = true;
      networking.firewall.enable = true;
    };

  in {
    # ========================================================================
    # NIXOS CONFIGURATIONS
    # ========================================================================
    nixosConfigurations = {

      # ======================================================================
      # ARCHIBALDOS - Desktop RT Audio Workstation (x86_64 ISO)
      # Profile: Audio Production
      # ======================================================================
      archibaldOS-iso = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit musnix flakeUri; };
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
          musnix.nixosModules.musnix
          ./modules/audio.nix
          ./modules/desktop.nix
          ./modules/branding.nix
          ./modules/hydramesh.nix
          ./modules/profiles.nix
          rtKernelConfig
          baseConfig
          ({ config, pkgs, lib, ... }: {
            nixpkgs.config.permittedInsecurePackages = [ "qtwebengine-5.15.19" ];

            # Select audio profile
            profiles.audio.enable = true;

            # Audio production packages
            environment.systemPackages = with pkgs; [
              # Core
              usbutils libusb1 alsa-firmware alsa-tools
              dialog mkpasswd

              # DAWs & Audio Tools
              audacity ardour reaper
              fluidsynth guitarix

              # Synths & Effects
              surge helm vmpk calf
              zrythm carla

              # DSP & Programming
              csound csound-qt
              faust faust2alsa faust2jack
              puredata supercollider

              # Utilities
              qjackctl pavucontrol
            ];

            # Graphics
            hardware.graphics.enable = true;
            hardware.graphics.extraPackages = with pkgs; [
              mesa vaapiIntel vaapiVdpau libvdpau-va-gl
            ];

            # Branding
            branding.enable = true;
            branding.variant = "audio";

            # Live user
            users.users.nixos = {
              isNormalUser = true;
              initialHashedPassword = lib.mkForce null;
              initialPassword = "nixos";
              home = "/home/nixos";
              createHome = true;
              extraGroups = [ "wheel" "audio" "jackaudio" "video" "networkmanager" "docker" ];
              shell = lib.mkForce pkgs.bashInteractive;
            };

            services.displayManager.autoLogin = {
              enable = true;
              user = "nixos";
            };
          })
        ];
      };

      # ======================================================================
      # ARCHIBALDOS ROBOTICS - RT Robotics Workstation (x86_64 ISO)
      # Profile: Robotics & Control Systems
      # ======================================================================
      archibaldOS-robotics = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit musnix flakeUri; };
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
          musnix.nixosModules.musnix
          ./modules/desktop.nix
          ./modules/branding.nix
          ./modules/hydramesh.nix
          ./modules/profiles.nix
          rtKernelConfig
          baseConfig
          ({ config, pkgs, lib, ... }: {
            nixpkgs.config.permittedInsecurePackages = [ "qtwebengine-5.15.19" ];

            # Select robotics profile
            profiles.robotics.enable = true;

            # Robotics packages
            environment.systemPackages = with pkgs; [
              # Core
              usbutils libusb1
              dialog mkpasswd
              
              # ROS 2 (if available in nixpkgs)
              # ros2-humble
              
              # Robotics frameworks
              gazebo
              # openrave
              
              # Control & Simulation
              octave
              python3Packages.numpy
              python3Packages.scipy
              python3Packages.matplotlib
              python3Packages.sympy
              python3Packages.control
              python3Packages.roboticstoolbox-python or null
              
              # Serial & Hardware
              minicom
              screen
              picocom
              python3Packages.pyserial
              
              # CAN bus
              can-utils
              
              # Vision
              opencv
              python3Packages.opencv4
              
              # 3D & CAD
              freecad
              openscad
              blender
              
              # Electronics
              kicad
              fritzing
              
              # Development
              cmake
              gnumake
              gcc
              gdb
              clang
              python3
              python3Packages.pip
              
              # IDEs
              vscode
              arduino-ide
              
              # Networking for robot comms
              wireshark
              tcpdump
              nmap
              
              # Documentation
              doxygen
              graphviz
            ];

            # Graphics for simulation
            hardware.graphics.enable = true;
            hardware.graphics.extraPackages = with pkgs; [
              mesa vaapiIntel vaapiVdpau libvdpau-va-gl
            ];

            # Branding
            branding.enable = true;
            branding.variant = "robotics";

            # Enable HydraMesh for robot-to-robot comms
            services.hydramesh = {
              enable = lib.mkDefault false;  # User can enable
              mode = "p2p";
              hardened = true;
            };

            # Serial port access
            users.groups.dialout = {};
            users.groups.plugdev = {};

            # udev rules for robotics hardware
            services.udev.extraRules = ''
              # Arduino
              SUBSYSTEM=="tty", ATTRS{idVendor}=="2341", MODE="0666", GROUP="dialout"
              SUBSYSTEM=="tty", ATTRS{idVendor}=="1a86", MODE="0666", GROUP="dialout"
              
              # FTDI
              SUBSYSTEM=="tty", ATTRS{idVendor}=="0403", MODE="0666", GROUP="dialout"
              
              # Silicon Labs CP210x
              SUBSYSTEM=="tty", ATTRS{idVendor}=="10c4", MODE="0666", GROUP="dialout"
              
              # STM32
              SUBSYSTEM=="usb", ATTRS{idVendor}=="0483", MODE="0666", GROUP="plugdev"
              
              # Teensy
              SUBSYSTEM=="usb", ATTRS{idVendor}=="16c0", MODE="0666", GROUP="plugdev"
              
              # Generic USB serial
              KERNEL=="ttyUSB*", MODE="0666", GROUP="dialout"
              KERNEL=="ttyACM*", MODE="0666", GROUP="dialout"
            '';

            # CAN bus kernel modules
            boot.kernelModules = [ "can" "can_raw" "can_bcm" "vcan" "slcan" ];

            # Live user with robotics groups
            users.users.nixos = {
              isNormalUser = true;
              initialHashedPassword = lib.mkForce null;
              initialPassword = "nixos";
              home = "/home/nixos";
              createHome = true;
              extraGroups = [ 
                "wheel" "video" "networkmanager" "docker"
                "dialout" "plugdev" "input" "gpio" "i2c" "spi"
              ];
              shell = lib.mkForce pkgs.bashInteractive;
            };

            # GPIO/I2C/SPI groups
            users.groups.gpio = {};
            users.groups.i2c = {};
            users.groups.spi = {};

            services.displayManager.autoLogin = {
              enable = true;
              user = "nixos";
            };

            # Kernel params for robotics (deterministic timing)
            boot.kernelParams = lib.mkAfter [
              "usbhid.mousepoll=1"  # Faster USB polling
            ];
          })
        ];
      };

      # ======================================================================
      # HYDRAMESH - Headless P2P Networking Node (x86_64 ISO)
      # ======================================================================
      hydramesh = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit musnix flakeUri; };
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
          musnix.nixosModules.musnix
          ./modules/hydramesh.nix
          ./modules/profiles.nix
          baseConfig
          ({ config, pkgs, lib, ... }: {
            # RT kernel for low-latency networking
            musnix = {
              enable = true;
              kernel.realtime = true;
              alsaSeq.enable = false;
              rtirq.enable = false;
              das_watchdog.enable = true;
            };

            boot.kernelParams = [
              "threadirqs"
              "isolcpus=1-3"
              "nohz_full=1-3"
              "intel_idle.max_cstate=1"
              "processor.max_cstate=1"
            ];

            environment.systemPackages = with pkgs; [
              usbutils
              dialog mkpasswd
              htop iotop
              tcpdump iperf3
              vim git tmux
            ];

            # Enable HydraMesh
            services.hydramesh = {
              enable = true;
              image = "alh477/hydramesh:latest";
              mode = "p2p";
              hardened = true;
            };

            powerManagement.cpuFreqGovernor = "performance";

            # Headless
            services.xserver.enable = false;
            services.displayManager.enable = false;

            # Console user
            users.users.hydramesh = {
              isNormalUser = true;
              extraGroups = [ "wheel" "networkmanager" "docker" ];
              initialPassword = "hydramesh";
            };

            services.getty.autologinUser = lib.mkForce "hydramesh";
          })
        ];
      };

    };

    # ========================================================================
    # BUILD OUTPUTS
    # ========================================================================
    packages.${system} = {
      default = self.nixosConfigurations.archibaldOS-iso.config.system.build.isoImage;
      iso = self.nixosConfigurations.archibaldOS-iso.config.system.build.isoImage;
      robotics-iso = self.nixosConfigurations.archibaldOS-robotics.config.system.build.isoImage;
      hydramesh-iso = self.nixosConfigurations.hydramesh.config.system.build.isoImage;
    };

    # ========================================================================
    # DEV SHELLS
    # ========================================================================
    devShells.${system} = {
      default = pkgs.mkShell {
        packages = with pkgs; [
          audacity ardour fluidsynth guitarix
          csound faust supercollider qjackctl
          surge puredata vim docker
        ];
      };

      robotics = pkgs.mkShell {
        packages = with pkgs; [
          cmake gnumake gcc gdb
          python3 python3Packages.numpy python3Packages.scipy
          python3Packages.matplotlib python3Packages.pyserial
          opencv arduino-ide
          can-utils minicom
          vim docker
        ];
      };
    };
  };
}
