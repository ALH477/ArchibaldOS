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
# module boots this qcow2 with CPU isolation + NETJACK audio routing — under
# OVMF by default, which is why the image is hybrid BIOS+UEFI
# (modules/dsp-vm-image.nix).
#
# Gates: `nix flake check` runs checks.rt-exec (the wrapper's effects, read
# from the exec'd process) and the eval-only contracts checks.dsp-vm-contract
# and checks.robotics-contract. packages.dsp-vm-boot-proxy boots the image
# layout under SeaBIOS and OVMF (slow: QEMU without KVM inside the sandbox),
# and is a package, not a check, so `nix flake check` stays cheap.
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

    # DeMoD: the audio engine (demod-orchestrator + demod-rt, GPL-3.0-only or
    # commercial) and DeMoD Mixer (MPL-2.0), the kiosk app. Public repo; the
    # licences are DeMoD's LICENSING.md. git+https with a pinned branch until
    # the mixer/kiosk work reaches DeMoD's main; follows our nixpkgs so a 4 GB
    # box carries one closure, not two.
    demod.url = "git+https://github.com/ALH477/DeMoD?ref=ccr-08f057a2-l4wyo1&shallow=1";
    demod.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, musnix, chaotic, nixos-hardware, nixpkgs-riscv, nixos-generators, demod }:
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
      # Disk layout + bootloader: hybrid GPT, GRUB for BIOS and as the
      # removable UEFI loader, and system.build.qcow2 itself.
      ./modules/dsp-vm-image.nix
      ({ config, pkgs, lib, ... }: {
        system.stateVersion = "24.11";
        boot.supportedFilesystems.zfs = lib.mkForce false;
        nixpkgs.config.allowUnfree = true;

        networking.hostName = "archibaldos-dsp";
        networking.useDHCP = true;
        networking.networkmanager.enable = lib.mkForce false;

        # Serial console output for VM debugging
        boot.kernelParams = [ "console=ttyS0" ];
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


    # ========================================================================
    # PROFILES — what each system IS, apart from how it is delivered.
    #
    # An ISO is `cd module ++ profiles.<p> ++ [ isoOnly live.<p> ]`; an
    # install made by the ISO's installer is `profiles.<p> ++ [ hardware scan,
    # installer choices ]` (installer/installed.nix). So what the installer
    # puts on the disk is what the ISO ran, minus the live session — and
    # never a generic configuration.nix (docs/installer.md).
    # ========================================================================
    audioProfile =
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

      });
    roboticsProfile =
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

        # udev rules (group-scoped, 0660) and the CAN modules come from
        # profiles.robotics.hardware.{arduino,canbus} in modules/profiles.nix.

        # GPIO/I2C/SPI groups
        users.groups.gpio = {};
        users.groups.i2c = {};
        users.groups.spi = {};

        # Kernel params for robotics (deterministic timing)
        boot.kernelParams = lib.mkAfter [
          "usbhid.mousepoll=1"  # Faster USB polling
        ];
      });
    hydrameshProfile =
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

      });
    audioMusnixProfile =
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

      });
    roboticsMusnixProfile =
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

        # udev rules (0660) and CAN modules: profiles.robotics.hardware.*

        users.groups.dialout = {};
        users.groups.plugdev = {};
        users.groups.gpio = {};
        users.groups.i2c = {};
        users.groups.spi = {};

      });
    # The live session's user (ISO only; an install gets the installer's user).
    audioLive = { config, pkgs, lib, ... }: {
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
      };    };
    # The live session's user (ISO only; an install gets the installer's user).
    roboticsLive = { config, pkgs, lib, ... }: {
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

      services.displayManager.autoLogin = {
        enable = true;
        user = "nixos";
      };    };
    # The live session's user (ISO only; an install gets the installer's user).
    hydrameshLive = { config, pkgs, lib, ... }: {
      # Console user
      users.users.hydramesh = {
        isNormalUser = true;
        extraGroups = [ "wheel" "networkmanager" "docker" ];
        initialPassword = "hydramesh";
      };

      services.getty.autologinUser = lib.mkForce "hydramesh";    };
    # The live session's user (ISO only; an install gets the installer's user).
    audioMusnixLive = { config, pkgs, lib, ... }: {
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
      };    };
    # The live session's user (ISO only; an install gets the installer's user).
    roboticsMusnixLive = { config, pkgs, lib, ... }: {
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
      };    };

    # ISO-only settings that used to live in baseConfig. An installed system
    # has no `isoImage` option at all.
    isoOnly = { ... }: {
      isoImage.squashfsCompression = "gzip -Xcompression-level 1";
    };

    # The companion role (modules/companion.nix): headless, 4 GB, commanded by
    # Oligarchy. JACK runs as archibald.companion.user, which an install sets
    # to the user it creates; this default only serves evaluating the profile
    # on its own.
    companionProfile = { config, lib, ... }: {
      archibald.companion.enable = true;
      archibald.companion.user = lib.mkDefault "archibald";
      users.users = lib.mkIf (config.archibald.companion.user == "archibald") {
        archibald = { isNormalUser = true; extraGroups = [ "wheel" ]; };
      };
    };

    # DeMoD's packages for whatever system a profile builds: the engine
    # (archibald.engine) and DeMoD Mixer for the kiosk (archibald.kiosk).
    demodProfile = { pkgs, lib, ... }:
      let dp = demod.packages.${pkgs.stdenv.hostPlatform.system} or { }; in {
        archibald.kiosk.package = lib.mkIf (dp ? demod-mixer) (lib.mkDefault dp.demod-mixer);
        archibald.engine.packages = lib.mkIf (dp ? demod-rt) (lib.mkDefault dp);
      };

    # A rack unit or mixer-like box: the companion, plus the DeMoD engine here,
    # on this box's interface. Its front panel is the mixer when a touchscreen
    # is attached (driving the local engine), headless otherwise. Oligarchy
    # commands it like any companion.
    rackProfile = { ... }: {
      archibald.engine.enable = true;
    };

    # Microsoft Surface (Intel) on top of the companion: nixos-hardware's
    # linux-surface kernel (built from source: build it on Oligarchy and push
    # it), iptsd for touch, thermald. The companion's CachyOS kernel is
    # mkDefault, so the Surface kernel wins.
    companionSurfaceProfile = { ... }: {
      hardware.microsoft-surface.kernelVersion = "longterm";
    };

    profiles = {
      audio = [
        chaotic.nixosModules.default
        musnix.nixosModules.musnix
        ./modules/audio.nix
        ./modules/desktop.nix
        ./modules/branding.nix
        ./modules/hydramesh.nix
        ./modules/profiles.nix
        cachyosRtConfig
        baseConfig
        audioProfile
      ];
      robotics = [
        chaotic.nixosModules.default
        musnix.nixosModules.musnix
        ./modules/desktop.nix
        ./modules/branding.nix
        ./modules/hydramesh.nix
        ./modules/profiles.nix
        cachyosRtConfig
        baseConfig
        roboticsProfile
      ];
      hydramesh = [
        chaotic.nixosModules.default
        musnix.nixosModules.musnix
        ./modules/hydramesh.nix
        ./modules/profiles.nix
        cachyosRtConfig
        baseConfig
        hydrameshProfile
      ];
      audio-musnix = [
        musnix.nixosModules.musnix
        ./modules/audio.nix
        ./modules/desktop.nix
        ./modules/branding.nix
        ./modules/hydramesh.nix
        ./modules/profiles.nix
        musnixRtConfig
        baseConfig
        audioMusnixProfile
      ];
      companion = [
        chaotic.nixosModules.default
        ./modules/companion.nix
        baseConfig
        companionProfile
        demodProfile
      ];
      companion-surface = [
        chaotic.nixosModules.default
        ./modules/companion.nix
        baseConfig
        companionProfile
        demodProfile
        nixos-hardware.nixosModules.microsoft-surface-pro-intel
        companionSurfaceProfile
      ];
      rack = [
        chaotic.nixosModules.default
        ./modules/companion.nix
        baseConfig
        companionProfile
        demodProfile
        rackProfile
      ];
      robotics-musnix = [
        musnix.nixosModules.musnix
        ./modules/desktop.nix
        ./modules/branding.nix
        ./modules/hydramesh.nix
        ./modules/profiles.nix
        musnixRtConfig
        baseConfig
        roboticsMusnixProfile
      ];
    };

    # ── Installer ─────────────────────────────────────────────────────────
    # What the profile page offers (installer/profiles.nix), the CLI for the
    # minimal ISO, and the Calamares overlay for the graphical ones. Both run
    # installer/calamares/distroinstall/main.py; see docs/installer.md.
    installerProfiles = import ./installer/profiles.nix;
    installerCli = defaultProfile: pkgs.callPackage ./installer/cli.nix {
      distro = "ArchibaldOS";
      source = self;
      profiles = installerProfiles;
      inherit defaultProfile;
    };
    installerModules = { cd, profile }:
      [ ({ pkgs, ... }: { environment.systemPackages = [ (installerCli profile) ]; }) ]
      ++ nixpkgs.lib.optional (nixpkgs.lib.hasInfix "calamares" cd)
        (import ./installer/iso.nix {
          distro = "ArchibaldOS";
          source = self;
          profiles = installerProfiles;
          defaultProfile = profile;
        });

    # An ISO: the installer CD module, the profile, the live session, and the
    # installer that puts THIS profile (or another) on the disk.
    mkIso = { cd, profile, live, specialArgs }: nixpkgs.lib.nixosSystem {
      inherit system specialArgs;
      modules = [ cd ] ++ profiles.${profile} ++ [ isoOnly live ]
        ++ installerModules { inherit cd profile; };
    };

    # Companion SD images for embedded boards (docs/form-factors.md). No
    # installer: the card is the system. The companion user's password is
    # "archibald", published, and expired on first boot, so the first login
    # (console or SSH) must change it; then `oligarchy-companion enroll` on
    # Oligarchy ends password logins altogether.
    sdFirstBoot = { config, pkgs, ... }:
      let user = config.archibald.companion.user; in {
        users.users.${user}.initialPassword = "archibald";
        systemd.services.archibald-expire-password = {
          description = "Expire the SD image's published password once";
          wantedBy = [ "multi-user.target" ];
          unitConfig.ConditionPathExists = "!/var/lib/archibald/password-expired";
          serviceConfig.Type = "oneshot";
          script = ''
            ${pkgs.shadow}/bin/chage -d 0 ${user}
            mkdir -p /var/lib/archibald
            touch /var/lib/archibald/password-expired
          '';
        };
      };
    mkCompanionSd = { system, flake ? nixpkgs, sdModule, board, extra ? [ ] }: flake.lib.nixosSystem {
      inherit system;
      specialArgs = { inherit musnix flakeUri; };
      modules = [ sdModule board ./modules/companion.nix baseConfig companionProfile demodProfile sdFirstBoot ] ++ extra;
    };

    # A system the installer put on a disk: `dir` holds what it wrote,
    # install.json (the user's choices) and hardware-configuration.nix (the
    # scan), plus optional commander.nix (written by Oligarchy's
    # `oligarchy-companion enroll`) and local.nix (yours; nothing writes it).
    mkInstalled = dir:
      let
        lib = nixpkgs.lib;
        install = builtins.fromJSON (builtins.readFile (dir + "/install.json"));
        profile = install.profile or (throw "${toString dir}/install.json names no profile");
        modules = profiles.${profile} or (throw
          "install.json asks for profile '${profile}'; this tree has: ${lib.concatStringsSep ", " (builtins.attrNames profiles)}");
        optionalFile = name: lib.optional (builtins.pathExists (dir + "/${name}")) (dir + "/${name}");
      in
      lib.nixosSystem {
        inherit system;
        specialArgs = { inherit musnix chaotic flakeUri; };
        modules = modules ++ [
          (dir + "/hardware-configuration.nix")
          (import ./installer/installed.nix { inherit install; })
        ]
        ++ lib.optional ((lib.hasPrefix "companion" profile || profile == "rack") && (install.user or null) != null)
          { archibald.companion.user = install.user.name; }
        ++ optionalFile "commander.nix"
        ++ optionalFile "local.nix";
      };

  in {
    # ========================================================================
    # NIXOS CONFIGURATIONS
    # ========================================================================
    nixosConfigurations = nixpkgs.lib.optionalAttrs (builtins.pathExists ./hosts/installed/install.json) {
      # This tree as the installer left it in /etc/nixos on an installed
      # machine: `nixos-rebuild switch --flake /etc/nixos#installed`.
      installed = mkInstalled ./hosts/installed;
    } // {

      # ======================================================================
      # ARCHIBALDOS - Desktop RT Audio Workstation (x86_64 ISO)
      # Profile: Audio Production
      # Kernel: CachyOS RT with BORE scheduler
      # ======================================================================
      archibaldOS-iso = mkIso {
        cd = "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix";
        profile = "audio";
        live = audioLive;
        specialArgs = { inherit musnix chaotic flakeUri; };
      };

      # ======================================================================
      # ARCHIBALDOS ROBOTICS - RT Robotics Workstation (x86_64 ISO)
      # Profile: Robotics & Control Systems
      # Kernel: CachyOS RT with BORE scheduler
      # ======================================================================
      archibaldOS-robotics = mkIso {
        cd = "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix";
        profile = "robotics";
        live = roboticsLive;
        specialArgs = { inherit musnix chaotic flakeUri; };
      };

      # ======================================================================
      # HYDRAMESH - Headless P2P Networking Node (x86_64 ISO)
      # Kernel: CachyOS RT with BORE scheduler
      # ======================================================================
      hydramesh = mkIso {
        cd = "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix";
        profile = "hydramesh";
        live = hydrameshLive;
        specialArgs = { inherit musnix chaotic flakeUri; };
      };

      # ======================================================================
      # FALLBACK CONFIGURATIONS (musnix PREEMPT_RT kernel)
      # For systems that can't use CachyOS or need mainline RT
      # ======================================================================

      # Audio with musnix RT kernel
      archibaldOS-musnix = mkIso {
        cd = "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix";
        profile = "audio-musnix";
        live = audioMusnixLive;
        specialArgs = { inherit musnix flakeUri; };
      };

      # Robotics with musnix RT kernel
      archibaldOS-robotics-musnix = mkIso {
        cd = "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix";
        profile = "robotics-musnix";
        live = roboticsMusnixLive;
        specialArgs = { inherit musnix flakeUri; };
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
      # COMPANION SD IMAGES — embedded boards as companions (headless, or the
      # mixer kiosk when a touchscreen is attached). docs/form-factors.md.
      # ======================================================================
      companion-pi4 = mkCompanionSd {
        system = "aarch64-linux";
        sdModule = "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix";
        board = nixos-hardware.nixosModules.raspberry-pi-4;
        extra = [{ networking.hostName = "archibald-pi"; }];
      };
      companion-pi5 = mkCompanionSd {
        system = "aarch64-linux";
        sdModule = "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix";
        board = nixos-hardware.nixosModules.raspberry-pi-5;
        extra = [{ networking.hostName = "archibald-pi"; }];
      };
      companion-riscv = mkCompanionSd {
        system = "riscv64-linux";
        flake = nixpkgs-riscv;
        sdModule = "${nixpkgs-riscv}/nixos/modules/installer/sd-card/sd-image.nix";
        board = nixos-hardware.nixosModules.starfive-visionfive-2;
        extra = [
          ./modules/rt-audio-riscv.nix
          ./modules/riscv-cross-overlay.nix
          ./hardware/jh7110.nix
          ({ lib, ... }: {
            networking.hostName = "archibald-rv";
            # NetworkManager drags in Haskell-built VPN plugins that cannot
            # bootstrap GHC on riscv64 (the archibaldOS-riscv image's reason).
            networking.networkmanager.enable = lib.mkForce false;
            networking.useNetworkd = true;
            # No mainline GPU driver for the JH7110's IMG BXE.
            archibald.kiosk.softwareRendering = true;
          })
        ];
      };

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
      companion-riscv-sdimage = self.nixosConfigurations.companion-riscv.config.system.build.sdImage;
    };

    # Raspberry Pi companions. Build on an aarch64 machine (or Oligarchy with
    # its aarch64 binfmt emulation, slowly).
    packages.aarch64-linux = {
      companion-pi4-sdimage = self.nixosConfigurations.companion-pi4.config.system.build.sdImage;
      companion-pi5-sdimage = self.nixosConfigurations.companion-pi5.config.system.build.sdImage;
    };

    # The roles, for a host that is not an ArchibaldOS image: Oligarchy's DSP
    # VM imports netjack (manager), demod-engine and dsp-control-bridge;
    # anything with JACK can be a box (adapter) or show the kiosk. They take
    # DeMoD's packages as options.
    nixosModules = {
      jack-graph = ./modules/jack-graph.nix;
      netjack = ./modules/netjack.nix;
      demod-engine = ./modules/demod-engine.nix;
      dsp-control-bridge = ./modules/dsp-control-bridge.nix;
      kiosk = ./modules/kiosk.nix;
      companion = ./modules/companion.nix;
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
      # Hybrid GPT: boots under SeaBIOS and under OVMF (the host's default).
      # Build: nix build .#dsp-vm-qcow2
      # Place: cp result/*.qcow2 ~/vms/archibaldos-dsp.qcow2
      #   (or assign the derivation to the host's archibaldOS.diskImage)
      dsp-vm-qcow2 = self.nixosConfigurations.dsp-vm.config.system.build.qcow2;

      # The RT wrapper JACK2 and demod-rt run under (modules/rt-exec.c).
      rt-exec = pkgs.callPackage ./modules/rt-exec.nix { };

      # The installer, from a shell: partition + mount under /mnt, then
      # `archibaldos-install --profile companion --user you`. Also on every ISO.
      archibaldos-install = installerCli "audio";

      # Boot the DSP image's layout + loader (modules/dsp-vm-image.nix) under
      # SeaBIOS and under OVMF, and require userspace on the serial console in
      # both. A stock kernel stands in for the RT one: what is under test is
      # the ESP / bios_grub / GRUB install, which no kernel choice changes.
      # Slow (QEMU TCG in the sandbox); a package, not a check.
      dsp-vm-boot-proxy = import ./tests/dsp-vm-boot-proxy.nix {
        inherit pkgs nixpkgs system;
      };

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

    # The installed-system builder, for anyone composing a host from this tree
    # (Oligarchy's `oligarchy-companion` builds a companion's copy with it).
    lib = { inherit mkInstalled; };

    # ========================================================================
    # CHECKS — `nix flake check`. Cheap: no KVM, no image build, no kernel.
    # A gate must exercise what the subsystem DOES (Oligarchy's rule): rt-exec
    # is run and its effect read back from the exec'd process; the contracts
    # are evaluated against the real configurations, and every one of them
    # fails on the tree before this change (tests/README.md says how that was
    # checked).
    # ========================================================================
    checks.${system} = {
      rt-exec = import ./tests/rt-exec.nix {
        inherit pkgs;
        rt-exec = self.packages.${system}.rt-exec;
      };
      dsp-vm-contract = import ./tests/dsp-vm-contract.nix {
        inherit pkgs;
        dspVm = self.nixosConfigurations.dsp-vm;
      };
      installed-contract = import ./tests/installed-contract.nix {
        inherit pkgs mkInstalled;
      };
      # NetJack2 between real JACK servers in the sandbox, with the modules'
      # own commands: a box's audio round-trips through the DSP host's engine
      # position, for a box that joins before the router and one after.
      roles-contract = import ./tests/roles-contract.nix {
        inherit pkgs mkInstalled;
        configs = self.nixosConfigurations;
      };
      netjack2 = import ./tests/netjack2/check.nix {
        inherit pkgs;
        roles = import ./tests/roles.nix { inherit nixpkgs system; };
      };
      installer-unit = import ./tests/installer-unit.nix {
        inherit pkgs;
        cli = installerCli "audio";
        extensions = pkgs.callPackage ./installer/calamares/extensions.nix {
          distro = "ArchibaldOS";
          source = self;
          profiles = installerProfiles;
          defaultProfile = "audio";
        };
      };
      robotics-contract = import ./tests/robotics-contract.nix {
        inherit pkgs;
        configs = {
          inherit (self.nixosConfigurations) archibaldOS-robotics archibaldOS-robotics-musnix;
        };
      };
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
