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
    streamdb.url = "path:./HydraMesh/streamdb";
    streamdb.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, musnix, disko, nixos-rk3588, streamdb }: let
    x86System = "x86_64-linux";
    armSystem = "aarch64-linux";
    
    mkPkgs = system: import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };

    pkgsX86 = mkPkgs x86System;
    pkgsArm = mkPkgs armSystem;

    # RT kernel patcher
    mkRtKernel = pkgs: kernel: let
      rtVersion = builtins.replaceStrings ["."] [""] kernel.version;
      rtPatchNum = "66";
      rtPatch = pkgs.fetchurl {
        url = "https://cdn.kernel.org/pub/linux/kernel/projects/rt/6.6/older/patch-${kernel.version}-rt${rtPatchNum}.patch.xz";
        hash = "sha256-e8f32d9aa0692f8194606221241758052bf76fec565ab629b778913e88d2d226=";
      };
    in pkgs.linuxManualConfig {
      inherit (kernel) src configfile allowImportFromDerivation version modDirVersion;
      kernelPatches = kernel.kernelPatches or [] ++ [ { name = "preempt-rt"; patch = rtPatch; } ];
      extraConfig = (kernel.extraConfig or "") + ''
        PREEMPT_RT y
      '';
    };

    # CachyOS RT BORE kernel (x86 only)
    cachyRtBoreKernel = pkgs: let
      kernelVersion = "6.17.9";
      pkgUrl = "https://mirror.cachyos.org/repo/x86_64_v3/cachyos-v3/linux-cachyos-rt-bore-${kernelVersion}-1-x86_64_v3.pkg.tar.zst";
      headersUrl = "https://mirror.cachyos.org/repo/x86_64_v3/cachyos-v3/linux-cachyos-rt-bore-headers-${kernelVersion}-1-x86_64_v3.pkg.tar.zst";
      
      kernelPkg = pkgs.fetchurl {
        url = pkgUrl;
        hash = "sha256-9z6v0d9z8q2x3c4v5b6n7m8k9j0h1g2f3d4s5a6t7y8u9i0o1p2q3r4e5w6x7c8v9n0m1l2k3j4h5g6f7d8s9a0b1c2e3r4t5y6u7i8o9p0q1w2e3r4t5y6u7i8o9p0";
      };
      
      headersPkg = pkgs.fetchurl {
        url = headersUrl;
        hash = "sha256-8y5v4d3c2b1a0z9x8w7v6u5t4r3q2p1o0n9m8l7k6j5h4g3f2e1d0c9b8a7z6y5x4w3v2u1t0s9r8q7p6o5n4m3l2k1j0h9g8f7e6d5c4b3a2z1y0x9w8v7u6t5s4r3q2p1";
      };
      
      unpackedKernel = pkgs.runCommand "unpack-cachy-kernel" {} ''
        mkdir -p $out
        ${pkgs.zstd}/bin/zstd -d ${kernelPkg} -o pkg.tar
        tar xf pkg.tar -C $out
      '';
      
      unpackedHeaders = pkgs.runCommand "unpack-cachy-headers" {} ''
        mkdir -p $out
        ${pkgs.zstd}/bin/zstd -d ${headersPkg} -o pkg.tar
        tar xf pkg.tar -C $out
      '';
    in pkgs.linuxManualConfig {
      version = "${kernelVersion}-cachyos-rt-bore";
      modDirVersion = "${kernelVersion}-cachyos-rt-bore";
      src = unpackedKernel;
      configfile = "${unpackedKernel}/usr/lib/modules/${kernelVersion}-cachyos-rt-bore/config";
      allowImportFromDerivation = true;
      kernelPatches = [];
      extraConfig = ''
        LOCALVERSION "-cachyos-rt-bore"
      '';
      buildRoot = unpackedHeaders;
      installPhase = ''
        mkdir -p $out/boot $out/lib/modules $out/lib/firmware
        cp ${unpackedKernel}/usr/lib/modules/*/vmlinuz $out/boot/vmlinuz || true
        cp -r ${unpackedKernel}/usr/lib/modules/* $out/lib/modules/ || true
        cp -r ${unpackedKernel}/usr/lib/firmware/* $out/lib/firmware/ || true
        cp -r ${unpackedHeaders}/usr/src/linux-* $out/lib/modules/*/build || true
      '';
    };

    # ARM kernels
    linux_rpi3 = pkgsArm.linux_rpi3;
    linux_rk3588 = pkgsArm.callPackage (nixos-rk3588 + "/pkgs/linux-rk3588/default.nix") {};
    linux_generic = pkgsArm.linux_6_1;

  in {
    nixosConfigurations = {
      # === x86_64 Live ISO ===
      archibaldOS-iso = nixpkgs.lib.nixosSystem {
        system = x86System;
        specialArgs = { standardKernel = pkgsX86.linux_latest; cachyRtBoreKernel = cachyRtBoreKernel pkgsX86; mkRtKernel = mkRtKernel pkgsX86; };
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
          musnix.nixosModules.musnix
          ./modules/base.nix
          ./modules/audio.nix
          ./modules/desktop.nix
          ./modules/users.nix
          ./modules/branding.nix
          ./modules/rt-kernel.nix
          ({ config, pkgs, lib, ... }: {
            system.stateVersion = "24.11";
            nixpkgs.config.permittedInsecurePackages = [ "qtwebengine-5.15.19" ];

            musnix = {
              enable = true;
              kernel.realtime = true;
              alsaSeq.enable = true;
              rtirq.enable = true;
              das_watchdog.enable = true;
            };

            environment.systemPackages = with pkgs; [
              usbutils libusb1 alsa-firmware alsa-tools
              dialog disko mkpasswd networkmanager
              audacity fluidsynth guitarix
              csound csound-qt faust portaudio rtaudio supercollider qjackctl
              surge zrythm carla puredata helm vmpk qmidinet 
              faust2alsa faust2csound faust2jack calf
            ];

            boot.kernelParams = [
              "threadirqs"
              "isolcpus=1-3"
              "nohz_full=1-3"
              "intel_idle.max_cstate=1"
              "processor.max_cstate=1"
            ];

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

            users.users.nixos = {
              isNormalUser = true;
              initialHashedPassword = lib.mkForce null;
              initialPassword = "nixos";
              home = "/home/nixos";
              createHome = true;
              extraGroups = [ "wheel" "audio" "jackaudio" "video" "networkmanager" ];
              shell = lib.mkForce pkgs.bashInteractive;
            };

            users.users.audio-user = lib.mkForce {
              isSystemUser = true;
              group = "audio-user";
              description = "Disabled in live ISO";
            };
            users.groups.audio-user = {};

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

      # === Orange Pi 5 ===
      archibaldOS-orangepi5 = nixpkgs.lib.nixosSystem {
        system = armSystem;
        specialArgs = { standardKernel = linux_rk3588; mkRtKernel = mkRtKernel pkgsArm; };
        modules = [
          nixos-rk3588.nixosModules.orangepi5
          musnix.nixosModules.musnix
          ./modules/base.nix
          ./modules/audio.nix
          ./modules/desktop.nix
          ./modules/users.nix
          ./modules/branding.nix
          ./modules/orange-pi-5.nix
          ./modules/rt-kernel.nix
          ({ config, pkgs, lib, ... }: {
            system.stateVersion = "24.11";

            musnix = {
              enable = true;
              kernel.realtime = false;
              rtirq = { enable = true; highList = "snd_usb_audio"; };
              das_watchdog.enable = true;
            };

            environment.systemPackages = with pkgs; [
              jack2 qjackctl jack_capture
              guitarix qtractor puredata
              pavucontrol helvum qpwgraph jalv
            ];

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

            archibaldOS.rtKernel = {
              enable = true;
              variant = "cachyos-rt-bore";
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

      # === Generic ARM SBC ===
      archibaldOS-arm-generic = nixpkgs.lib.nixosSystem {
        system = armSystem;
        specialArgs = { standardKernel = linux_generic; mkRtKernel = mkRtKernel pkgsArm; };
        modules = [
          musnix.nixosModules.musnix
          ./modules/base.nix
          ./modules/audio.nix
          ./modules/desktop.nix
          ./modules/users.nix
          ./modules/branding.nix
          ./modules/rt-kernel.nix
          ({ config, pkgs, lib, ... }: {
            system.stateVersion = "24.11";

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

      # === Raspberry Pi 3B (headless) ===
      archibaldOS-rpi3b = nixpkgs.lib.nixosSystem {
        system = armSystem;
        specialArgs = { standardKernel = linux_rpi3; mkRtKernel = mkRtKernel pkgsArm; };
        modules = [
          "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
          musnix.nixosModules.musnix
          ./modules/base.nix
          ./modules/audio.nix
          ./modules/users.nix
          ./modules/rt-kernel.nix
          ({ config, pkgs, lib, ... }: {
            system.stateVersion = "24.11";

            boot.loader.grub.enable = false;
            boot.loader.generic-extlinux-compatible.enable = true;

            fileSystems."/boot" = {
              device = "/dev/disk/by-label/BOOT";
              fsType = "vfat";
            };

            sdImage.populateFirmwareCommands = ''
              mkdir -p $NIX_BUILD_TOP/boot
              cp -r ${pkgs.raspberrypifw}/share/raspberrypi/boot/* $NIX_BUILD_TOP/boot/
              cat > $NIX_BUILD_TOP/boot/config.txt <<'EOF'
              # ArchibaldOS RPi3B config
              dtparam=audio=on
              force_turbo=1
              arm_freq=1200
              gpu_mem=16
              disable_overscan=1
              kernel=vmlinuz
              initramfs initrd.img followkernel
              EOF
              ln -s ${pkgs.raspberrypifw}/share/raspberrypi/boot/bcm2837-rpi-3-b.dtb $NIX_BUILD_TOP/boot/
            '';

            hardware.enableRedistributableFirmware = true;
            hardware.graphics.enable = false;

            musnix = {
              enable = true;
              kernel.realtime = false;
              rtirq = { enable = true; highList = "snd_usb_audio"; };
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
              guitarix puredata
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

            services.xserver.enable = false;
            services.displayManager.enable = false;

            users.users.audio = {
              isNormalUser = true;
              extraGroups = [ "wheel" "audio" "jackaudio" "realtime" "networkmanager" ];
              initialPassword = "changeme";
            };

            services.getty.autologinUser = "audio";

            nix.settings.experimental-features = [ "nix-command" "flakes" ];
            networking.hostName = "archibaldos-rpi3b";

            sdImage.imageName = "archibaldos-rpi3b-headless.img";
            sdImage.compressImage = true;

            boot.kernelModules = [ "snd_usb_audio" "snd_bcm2835" ];
          })
        ];
      };

      # === Server (headless x86) ===
      archibaldOS-server = nixpkgs.lib.nixosSystem {
        system = x86System;
        modules = [
          musnix.nixosModules.musnix
          ./modules/base.nix
          ./modules/server.nix
          ./modules/users.nix
          ({ config, pkgs, lib, ... }: {
            system.stateVersion = "24.11";
            musnix.enable = false;
            
            users.users.admin = {
              isNormalUser = true;
              extraGroups = [ "wheel" "docker" ];
              openssh.authorizedKeys.keys = [ ];
            };

            nix.settings.experimental-features = [ "nix-command" "flakes" ];
          })
        ];
      };

      # === DSP Coprocessor (kexec, minimal, x86_64) ===
      archibaldOS-dsp = nixpkgs.lib.nixosSystem {
        system = x86System;
        specialArgs = {
          standardKernel = pkgsX86.linux_latest;
          mkRtKernel = mkRtKernel pkgsX86;
          cachyRtBoreKernel = cachyRtBoreKernel pkgsX86; 
        };
        modules = [
          disko.nixosModules.disko
          musnix.nixosModules.musnix
          ./modules/dsp.nix
          ({ config, pkgs, lib, ... }: {
            system.stateVersion = "24.11";

            archibaldOS.rtKernel = {
              enable = true;
              variant = "cachyos-rt-bore";
            };

            systemd.services.kexec-load = {
              description = "Load real-time kernel via kexec";
              wantedBy = [ "multi-user.target" ];
              after = [ "local-fs.target" ];
              serviceConfig = {
                Type = "oneshot";
                ExecStart = "${pkgs.kexec-tools}/bin/kexec -l /boot/kexec/vmlinuz --initrd=/boot/kexec/initrd --append=\"$(cat /boot/kexec/cmdline)\"";
                RemainAfterExit = true;
              };
            };

            systemd.services.kexec-exec = {
              description = "Execute kexec (boot into real kernel)";
              wantedBy = [ "multi-user.target" ];
              requires = [ "kexec-load.service" ];
              serviceConfig = {
                Type = "oneshot";
                ExecStart = "${pkgs.kexec-tools}/bin/kexec -e";
              };
            };
          })
        ];
      };
    };

    # === Build Outputs (XZ compressed) ===
    packages.${x86System} = {
      iso = let
        rawIso = self.nixosConfigurations.archibaldOS-iso.config.system.build.isoImage;
      in pkgsX86.runCommand "archibaldOS.iso.xz" {
        nativeBuildInputs = [ pkgsX86.xz ];
      } ''
        cp ${rawIso}/iso/*.iso $out.tmp
        xz -9e --threads=0 $out.tmp
        mv $out.tmp.xz $out
      '';

      dsp = pkgsX86.runCommandLocal "archibaldOS-dsp.img.xz" {
        nativeBuildInputs = [ pkgsX86.xz ];
      } ''
        mkdir -p $out.tmp
        cp ${self.nixosConfigurations.archibaldOS-dsp.config.system.build.diskoImages}/disk1.img $out.tmp/dsp.img
        xz -9e --threads=0 $out.tmp/dsp.img
        mv $out.tmp/dsp.img.xz $out
      '';
    };

    packages.${armSystem} = {
      orangepi5 = pkgsArm.runCommandLocal "archibaldOS-orangepi5.img.xz" {
        nativeBuildInputs = [ pkgsArm.xz ];
      } ''
        mkdir -p $out.tmp-dir
        cp -r ${self.nixosConfigurations.archibaldOS-orangepi5.config.system.build.sdImage}/sd-image/* $out.tmp-dir/
        img_file=$(ls $out.tmp-dir/*.img)
        xz -9e --threads=0 "$img_file"
        mv "$img_file.xz" "$out"
      '';

      generic = self.nixosConfigurations.archibaldOS-arm-generic.config.system.build.toplevel;

      genericTarXz = pkgsArm.runCommandLocal "archibaldOS-arm-generic.tar.xz" {
        nativeBuildInputs = [ pkgsArm.xz ];
      } ''
        tar -C ${self.nixosConfigurations.archibaldOS-arm-generic.config.system.build.toplevel} -cf - . \
          | xz -9e --threads=0 > "$out"
      '';

      rpi3b = pkgsArm.runCommandLocal "archibaldOS-rpi3b.img.xz" {
        nativeBuildInputs = [ pkgsArm.xz ];
      } ''
        mkdir -p $out.tmp-dir
        cp -r ${self.nixosConfigurations.archibaldOS-rpi3b.config.system.build.sdImage}/sd-image/* $out.tmp-dir/
        img_file=$(ls $out.tmp-dir/*.img)
        xz -9e --threads=0 "$img_file"
        mv "$img_file.xz" "$out"
      '';
    };

    # === Dev Shells ===
    devShells.${x86System}.default = (mkPkgs x86System).mkShell {
      packages = with (mkPkgs x86System); [
        audacity ardour fluidsynth musescore guitarix
        csound faust portaudio rtaudio supercollider qjackctl
        surge pcmanfm vim
      ];
    };
      
    devShells.${armSystem}.default = (mkPkgs armSystem).mkShell {
      packages = with (mkPkgs armSystem); [
        jack2 guitarix puredata vim
      ];
    };
  };
}
