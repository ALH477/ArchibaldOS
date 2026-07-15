# flake.nix
# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 DeMoD LLC. All rights reserved.
# ============================================================================
# ArchibaldOS Community Edition
# Real-time audio workstation + HydraMesh P2P networking for NixOS
#
# For commercial features (Thunderbolt/USB4, ARM support, auto-updates,
# enterprise configurations), see: https://demod.ltd
#
# DSP Coprocessor VM: The `dsp-vm` configuration + `dsp-vm-qcow2` package
# build a headless RT guest image designed for use with the Oligarchy NixOS
# host (https://github.com/ALH477/Oligarchy). The host's `vm-manager/dsp-vm.nix`
# module boots this qcow2 with CPU isolation + NETJACK audio routing.
#
# Organization: https://github.com/ALH477
# ============================================================================
{
  description = "ArchibaldOS Community Edition - Real-time audio workstation + HydraMesh";

  inputs = {
    # nixos-unstable to match the chaotic (nyxpkgs-unstable) overlay, which
    # tracks unstable — a 24.11 base drifts out of sync with chaotic's CachyOS
    # kernel (missing zfs/kernel attrs). Same tree serves the riscv64 image.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    musnix.url = "github:musnix/musnix";
    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
    # RISC-V (StarFive JH7110) hardware baseline.
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixpkgs-riscv.follows = "nixpkgs";
    # For building qcow2 VM images (DSP coprocessor guest)
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";

    # DeMoD RT engine (proprietary — PolyForm Shield 1.0.0).
    # NOT included by default. To build the dsp-vm-demod config:
    #   1. Ensure ~/demod-work exists (git clone the private repo)
    #   2. Uncomment the input below
    #   3. Uncomment the dsp-vm-demod nixosConfiguration + package
    #   4. nix build .#dsp-vm-demod-qcow2
    # demod.url = "path:/home/asher/demod-work";
  };

  outputs = { self, nixpkgs, musnix, chaotic, nixos-hardware, nixpkgs-riscv, nixos-generators }:
  # NOTE: when demod input is enabled, add `demod` to the outputs args above.
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };

    # Shared flake URI for updates
    flakeUri = "github:ALH477/ArchibaldOS";

    # DSP VM guest modules — shared between nixosConfiguration and qcow2
    # image build so they stay in sync.
    # Uses musnix PREEMPT_RT kernel (not CachyOS) for maximum determinism.
    dspVmModules = [
      musnix.nixosModules.musnix
      ./modules/headless-dsp.nix
      ({ config, pkgs, lib, ... }: {
        system.stateVersion = "24.11";
        boot.supportedFilesystems.zfs = lib.mkForce false;
        nixpkgs.config.allowUnfree = true;

        networking.hostName = "archibaldos-dsp";
        networking.useDHCP = true;
        networking.networkmanager.enable = lib.mkForce false;

        # Root filesystem on virtio disk (for qcow2 VM image)
        fileSystems."/" = {
          device = "/dev/disk/by-label/nixos";
          fsType = "ext4";
        };

        # GRUB bootloader on virtio disk
        boot.loader.grub.enable = true;
        boot.loader.grub.device = "/dev/vda";
        boot.loader.grub.efiSupport = false;
        boot.loader.timeout = 1;  # Fast boot — no menu delay
      })
    ];

    # CachyOS BORE kernel configuration (primary)
    cachyosRtConfig = { config, pkgs, lib, ... }: {
      # CachyOS kernel: BORE scheduler + full preemption (via preempt=full
      # below). Chaotic dropped the separate -rt package; the main cachyos
      # kernel is the low-latency baseline.
      boot.kernelPackages = pkgs.linuxPackages_cachyos;

      # RT kernel params
      boot.kernelParams = [
        "threadirqs"
        "isolcpus=1-3"
        "nohz_full=1-3"
        "intel_idle.max_cstate=1"
        "processor.max_cstate=1"
        "tsc=reliable"
        "clocksource=tsc"
        "preempt=full"
      ];

      powerManagement.cpuFreqGovernor = "performance";

      # scx scheduler support (if available)
      # boot.kernelModules = [ "scx" ];
    };

    # Fallback musnix RT kernel configuration
    musnixRtConfig = { config, pkgs, lib, ... }: {
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

      # The CachyOS kernel tracks mainline very closely; OpenZFS lags the newest
      # kernels and refuses to evaluate against them. ZFS is not needed on an RT
      # audio workstation, so keep it off (the graphical installer enables it by
      # default otherwise).
      boot.supportedFilesystems.zfs = lib.mkForce false;

      # Some bundled tools (e.g. reaper) are unfree. The module-system pkgs
      # needs this even though the flake's top-level pkgs already sets it.
      nixpkgs.config.allowUnfree = true;
    };

  in {
    # ========================================================================
    # NIXOS CONFIGURATIONS
    # ========================================================================
    nixosConfigurations = {

      # ======================================================================
      # ARCHIBALDOS - Desktop RT Audio Workstation (x86_64 ISO)
      # Profile: Audio Production
      # Kernel: CachyOS RT with BORE scheduler
      # ======================================================================
      archibaldOS-iso = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit musnix chaotic flakeUri; };
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
          chaotic.nixosModules.default
          musnix.nixosModules.musnix
          ./modules/audio.nix
          ./modules/desktop.nix
          ./modules/branding.nix
          ./modules/hydramesh.nix
          ./modules/profiles.nix
          cachyosRtConfig
          baseConfig
          ({ config, pkgs, lib, ... }: {
            nixpkgs.config.permittedInsecurePackages = [ "qtwebengine-5.15.19" ];

            # Select audio profile
            profiles.audio.enable = true;

            # musnix for audio tooling (not kernel)
            musnix = {
              enable = true;
              kernel.realtime = false;  # Using CachyOS RT instead
              alsaSeq.enable = true;
              rtirq.enable = true;
              das_watchdog.enable = true;
            };

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
              intel-media-driver intel-vaapi-driver libvdpau-va-gl
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
      # Kernel: CachyOS RT with BORE scheduler
      # ======================================================================
      archibaldOS-robotics = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit musnix chaotic flakeUri; };
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
          chaotic.nixosModules.default
          musnix.nixosModules.musnix
          ./modules/desktop.nix
          ./modules/branding.nix
          ./modules/hydramesh.nix
          ./modules/profiles.nix
          cachyosRtConfig
          baseConfig
          ({ config, pkgs, lib, ... }: {
            nixpkgs.config.permittedInsecurePackages = [ "qtwebengine-5.15.19" ];

            # Select robotics profile
            profiles.robotics.enable = true;

            # musnix for RT tooling (not kernel)
            musnix = {
              enable = true;
              kernel.realtime = false;  # Using CachyOS RT instead
              rtirq.enable = true;
              das_watchdog.enable = true;
            };

            # Robotics packages
            environment.systemPackages = with pkgs; [
              # Core
              usbutils libusb1
              dialog mkpasswd
              
              # ROS 2 (if available in nixpkgs)
              # ros2-humble
              
              # Robotics frameworks
              # (Gazebo Classic was removed from nixpkgs; add gz-sim from an
              #  overlay if you need a simulator.)

              # Control & Simulation
              octave
              python3Packages.numpy
              python3Packages.scipy
              python3Packages.matplotlib
              python3Packages.sympy
              python3Packages.control
              
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
              intel-media-driver intel-vaapi-driver libvdpau-va-gl
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
      # Kernel: CachyOS RT with BORE scheduler
      # ======================================================================
      hydramesh = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit musnix chaotic flakeUri; };
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
          chaotic.nixosModules.default
          musnix.nixosModules.musnix
          ./modules/hydramesh.nix
          ./modules/profiles.nix
          cachyosRtConfig
          baseConfig
          ({ config, pkgs, lib, ... }: {
            # musnix for RT tooling (not kernel)
            musnix = {
              enable = true;
              kernel.realtime = false;  # Using CachyOS RT instead
              alsaSeq.enable = false;
              rtirq.enable = false;
              das_watchdog.enable = true;
            };

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

      # ======================================================================
      # FALLBACK CONFIGURATIONS (musnix PREEMPT_RT kernel)
      # For systems that can't use CachyOS or need mainline RT
      # ======================================================================

      # Audio with musnix RT kernel
      archibaldOS-musnix = nixpkgs.lib.nixosSystem {
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
          musnixRtConfig
          baseConfig
          ({ config, pkgs, lib, ... }: {
            nixpkgs.config.permittedInsecurePackages = [ "qtwebengine-5.15.19" ];

            profiles.audio.enable = true;

            environment.systemPackages = with pkgs; [
              usbutils libusb1 alsa-firmware alsa-tools
              dialog mkpasswd
              audacity ardour reaper fluidsynth guitarix
              surge helm vmpk calf zrythm carla
              csound csound-qt faust faust2alsa faust2jack
              puredata supercollider qjackctl pavucontrol
            ];

            hardware.graphics.enable = true;
            hardware.graphics.extraPackages = with pkgs; [
              intel-media-driver intel-vaapi-driver libvdpau-va-gl
            ];

            branding.enable = true;
            branding.variant = "audio";

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

      # Robotics with musnix RT kernel
      archibaldOS-robotics-musnix = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit musnix flakeUri; };
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
          musnix.nixosModules.musnix
          ./modules/desktop.nix
          ./modules/branding.nix
          ./modules/hydramesh.nix
          ./modules/profiles.nix
          musnixRtConfig
          baseConfig
          ({ config, pkgs, lib, ... }: {
            nixpkgs.config.permittedInsecurePackages = [ "qtwebengine-5.15.19" ];

            profiles.robotics.enable = true;

            environment.systemPackages = with pkgs; [
              usbutils libusb1 dialog mkpasswd
              octave
              python3Packages.numpy python3Packages.scipy
              python3Packages.matplotlib python3Packages.sympy
              python3Packages.pyserial
              minicom screen picocom can-utils
              opencv freecad openscad blender
              kicad cmake gnumake gcc gdb clang
              python3 vscode arduino-ide
              wireshark tcpdump nmap
              doxygen graphviz
            ];

            hardware.graphics.enable = true;
            hardware.graphics.extraPackages = with pkgs; [
              intel-media-driver intel-vaapi-driver libvdpau-va-gl
            ];

            branding.enable = true;
            branding.variant = "robotics";

            services.udev.extraRules = ''
              SUBSYSTEM=="tty", ATTRS{idVendor}=="2341", MODE="0666", GROUP="dialout"
              SUBSYSTEM=="tty", ATTRS{idVendor}=="1a86", MODE="0666", GROUP="dialout"
              SUBSYSTEM=="tty", ATTRS{idVendor}=="0403", MODE="0666", GROUP="dialout"
              SUBSYSTEM=="tty", ATTRS{idVendor}=="10c4", MODE="0666", GROUP="dialout"
              SUBSYSTEM=="usb", ATTRS{idVendor}=="0483", MODE="0666", GROUP="plugdev"
              SUBSYSTEM=="usb", ATTRS{idVendor}=="16c0", MODE="0666", GROUP="plugdev"
              KERNEL=="ttyUSB*", MODE="0666", GROUP="dialout"
              KERNEL=="ttyACM*", MODE="0666", GROUP="dialout"
            '';

            boot.kernelModules = [ "can" "can_raw" "can_bcm" "vcan" "slcan" ];

            users.groups.dialout = {};
            users.groups.plugdev = {};
            users.groups.gpio = {};
            users.groups.i2c = {};
            users.groups.spi = {};

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

            services.displayManager.autoLogin = {
              enable = true;
              user = "nixos";
            };
          })
        ];
      };

      # ======================================================================
      # ARCHIBALDOS DSP-VM — Headless DSP Coprocessor (qcow2 VM image)
      # For use as a QEMU/KVM guest on the Oligarchy host. Provides JACK2
      # with NETJACK netone driver for audio routing over the VM network.
      # Kernel: CachyOS BORE + preempt=full
      # ======================================================================
      dsp-vm = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit musnix; };
        modules = dspVmModules;
      };

      # ======================================================================
      # DSP-VM-DEMOD — ArchibaldOS DSP VM with DeMoD RT engine baked in
      # Same as dsp-vm but includes demod-rt (Faust FX processing inside VM).
      # Requires the `demod` flake input — uncomment it + this block + the
      # package output below to build.
      # ======================================================================
      # dsp-vm-demod = nixpkgs.lib.nixosSystem {
      #   inherit system;
      #   specialArgs = { inherit musnix demod; };
      #   modules = dspVmModules ++ [
      #     ./modules/demod-rt.nix
      #     ({ config, pkgs, lib, ... }: {
      #       services.demod-rt = {
      #         enable = true;
      #         package = demod.packages.${system}.demod-rt;
      #         rtCore = 0;           # CPU 0 (only vCPU in VM)
      #         rtPriority = 80;      # Below JACK (99)
      #         # faustLibs = [ /path/to/compiled/effects.so ];
      #       };
      #     })
      #   ];
      # };

      # ======================================================================
      # ARCHIBALDOS-RISCV - RT Audio SD image for StarFive JH7110 boards
      # (VisionFive 2 / DeepComputing Framework 13 RV). riscv64-linux.
      # Kernel: mainline Linux 6.12 with native PREEMPT_RT (CachyOS RT does
      # not build for riscv64). Build natively on the board, or cross from
      # x86_64 under binfmt qemu-user. See docs/riscv.md.
      # ======================================================================
      archibaldOS-riscv = nixpkgs-riscv.lib.nixosSystem {
        system = "riscv64-linux";
        specialArgs = { inherit musnix flakeUri; };
        modules = [
          "${nixpkgs-riscv}/nixos/modules/installer/sd-card/sd-image.nix"
          nixos-hardware.nixosModules.starfive-visionfive-2
          ./modules/rt-audio-riscv.nix
          ./modules/riscv-cross-overlay.nix
          ./hardware/jh7110.nix
          ({ config, pkgs, lib, ... }: {
            system.stateVersion = "24.11";
            nix.settings.experimental-features = [ "nix-command" "flakes" ];

            networking.hostName = "archibaldos-rv";
            # Headless appliance: plain systemd-networkd DHCP, not NetworkManager
            # (NM drags in Haskell-dependent VPN plugins that cannot bootstrap
            # GHC on riscv64). Configure wireless per-machine if needed.
            networking.useNetworkd = true;
            networking.useDHCP = lib.mkDefault true;
            networking.firewall.enable = true;

            # Lean RT-audio userland — only what builds cleanly for riscv64.
            environment.systemPackages = with pkgs; [
              alsa-utils alsa-lib
              jack2 jack-example-tools
              vim git htop usbutils i2c-tools
            ];

            # Console user (change the password on first boot).
            users.users.audio = {
              isNormalUser = true;
              extraGroups = [ "wheel" "audio" "dialout" "i2c" "plugdev" ];
              initialPassword = "archibald";
            };
            users.groups.i2c = {};
            users.groups.plugdev = {};

            services.getty.autologinUser = lib.mkForce "audio";
          })
        ];
      };

    };

    # ========================================================================
    # BUILD OUTPUTS
    # ========================================================================

    # RISC-V SD image — exposed under riscv64-linux (native board build) and
    # x86_64-linux (binfmt qemu-user cross build). See docs/riscv.md.
    packages.riscv64-linux = {
      default = self.nixosConfigurations.archibaldOS-riscv.config.system.build.sdImage;
      archibaldOS-riscv-sdimage = self.nixosConfigurations.archibaldOS-riscv.config.system.build.sdImage;
    };

    packages.${system} = {
      # CachyOS RT BORE (primary)
      default = self.nixosConfigurations.archibaldOS-iso.config.system.build.isoImage;
      iso = self.nixosConfigurations.archibaldOS-iso.config.system.build.isoImage;
      robotics-iso = self.nixosConfigurations.archibaldOS-robotics.config.system.build.isoImage;
      hydramesh-iso = self.nixosConfigurations.hydramesh.config.system.build.isoImage;

      # musnix PREEMPT_RT (fallback)
      iso-musnix = self.nixosConfigurations.archibaldOS-musnix.config.system.build.isoImage;
      robotics-iso-musnix = self.nixosConfigurations.archibaldOS-robotics-musnix.config.system.build.isoImage;

      # RISC-V SD image, cross-built from x86_64 under binfmt qemu-user.
      # Requires `boot.binfmt.emulatedSystems = [ "riscv64-linux" ];` on the
      # build host (or a native riscv64 builder). Slow — prefer a native
      # on-board build; see docs/riscv.md.
      archibaldOS-riscv-sdimage = self.nixosConfigurations.archibaldOS-riscv.config.system.build.sdImage;

      # DSP coprocessor VM image (qcow2) — for QEMU/KVM on Oligarchy host.
      # Build: nix build .#dsp-vm-qcow2
      # Place: cp result/*.qcow2 ~/vms/archibaldos-dsp.qcow2
      dsp-vm-qcow2 = (nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit musnix; };
        modules = dspVmModules ++ [
          ({ config, pkgs, lib, ... }: {
            # Build a qcow2 disk image using nixpkgs' make-disk-image
            system.build.qcow2 = pkgs.callPackage "${nixpkgs}/nixos/lib/make-disk-image.nix" {
              inherit config lib pkgs;
              diskSize = 4096;       # 4GB — enough for DSP guest
              format = "qcow2";
            };
          })
        ];
      }).config.system.build.qcow2;

      # DSP coprocessor VM image with DeMoD RT engine (qcow2).
      # Requires the `demod` flake input — uncomment to build.
      # dsp-vm-demod-qcow2 = (nixpkgs.lib.nixosSystem {
      #   inherit system;
      #   specialArgs = { inherit musnix demod; };
      #   modules = dspVmModules ++ [
      #     ./modules/demod-rt.nix
      #     ({ config, pkgs, lib, ... }: {
      #       services.demod-rt = {
      #         enable = true;
      #         package = demod.packages.${system}.demod-rt;
      #       };
      #     })
      #     ({ config, pkgs, lib, ... }: {
      #       system.build.qcow2 = pkgs.callPackage "${nixpkgs}/nixos/lib/make-disk-image.nix" {
      #         inherit config lib pkgs;
      #         diskSize = 4096;
      #         format = "qcow2";
      #       };
      #     })
      #   ];
      # }).config.system.build.qcow2;
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
