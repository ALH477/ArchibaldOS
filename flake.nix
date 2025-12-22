# flake.nix
# Copyright 2025 DeMoD LLC
# SPDX-License-Identifier: BSD-3-Clause
#
# ArchibaldOS: Unified NixOS for real-time audio, robotics, LIDAR, secure routing, and DSP.
# Multi-target flake with x86_64 live ISO, headless servers, DSP coprocessors, and ARM SBCs.
# Fully integrated with HydraMesh v2.2.0 – low-latency Common Lisp DCF for gaming & real-time audio.
# Preserves working CachyOS RT-BORE kernel logic unchanged.

{
  description = "ArchibaldOS: Unified NixOS with integrated HydraMesh DCF SDK";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

    musnix.url = "github:musnix/musnix";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    nixos-rk3588.url = "github:ryan4yin/nixos-rk3588";
    nixos-rk3588.inputs.nixpkgs.follows = "nixpkgs";

    # HydraMesh SDK flake (local path – replace with remote if needed)
    hydramesh.url = "path:./HydraMesh";
    hydramesh.inputs.nixpkgs.follows = "nixpkgs";
    hydramesh.inputs.flake-utils.follows = "flake-utils";
    hydramesh.inputs.emacs-overlay.follows = "emacs-overlay";

    # StreamDB native library (required by HydraMesh)
    streamdb.url = "path:./HydraMesh/streamdb";
    streamdb.inputs.nixpkgs.follows = "nixpkgs";

    # Required for HydraMesh devShell
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, musnix, disko, nixos-rk3588, hydramesh, streamdb, flake-utils, emacs-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            # Apply overlays from HydraMesh and StreamDB to expose their packages
            hydramesh.overlays.default
            streamdb.overlays.default
            emacs-overlay.overlays.default
          ];
        };

        # Generic RT kernel patcher (fallback – not primary)
        mkRtKernel = pkgs: kernel: let
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

        # Preserved, fully functional CachyOS RT-BORE kernel – DO NOT MODIFY VERSION OR URLS
        cachyRtBoreKernel = let
          kernelVersion = "6.17.8";
          pkgUrl = "https://mirror.cachyos.org/repo/x86_64/cachyos/linux-cachyos-rt-bore-${kernelVersion}-1-x86_64.pkg.tar.zst";
          headersUrl = "https://mirror.cachyos.org/repo/x86_64/cachyos/linux-cachyos-rt-bore-headers-${kernelVersion}-1-x86_64.pkg.tar.zst";

          kernelPkg = pkgs.fetchurl {
            url = pkgUrl;
            hash = "sha256-Qih3/F8BOGy1L0A4qH6yHGdZH2y0j6NSplLOFasBXy8=";
          };

          headersPkg = pkgs.fetchurl {
            url = headersUrl;
            hash = "sha256-BSbGmn2VhwOSB6BzpxcdJTowFOK2+efW8yLHIXxo1Yc=";
          };

          unpacked = pkgs.runCommand "unpack-cachy-rt-bore" { } ''
            mkdir -p $out/kernel $out/headers
            ${pkgs.zstd}/bin/zstd -d ${kernelPkg} -o - | tar xf - -C $out/kernel
            ${pkgs.zstd}/bin/zstd -d ${headersPkg} -o - | tar xf - -C $out/headers
          '';
        in pkgs.buildLinux {
          version = "${kernelVersion}-cachyos-rt-bore";
          modDirVersion = "${kernelVersion}-cachyos-rt-bore";
          src = unpacked + "/kernel";
          kernelPatches = [ ];
          configfile = unpacked + "/kernel/usr/lib/modules/${kernelVersion}-cachyos-rt-bore/config";
          extraMakeFlags = [ "modules_prepare" ];
          allowImportFromDerivation = true;
          ignoreConfigErrors = true;

          postInstall = ''
            cp -r ${unpacked}/kernel/usr/lib/modules/* $out/lib/modules/
            cp -r ${unpacked}/kernel/usr/lib/firmware/* $out/lib/firmware/ 2>/dev/null || true
            cp -r ${unpacked}/headers/usr/src/linux-* $out/lib/modules/*/build/
          '';
        };

        # ARM kernels
        linux_rpi3 = pkgs.linux_rpi3;
        linux_rk3588 = pkgs.callPackage (nixos-rk3588 + "/pkgs/linux-rk3588/default.nix") { };
        linux_generic = pkgs.linux_6_1;

        # Helper for NixOS systems – injects HydraMesh module globally
        mkNixosSystem = system: extraSpecialArgs: modules:
          nixpkgs.lib.nixosSystem {
            inherit system;
            specialArgs = {
              standardKernel = if system == "x86_64-linux" then pkgs.linux_latest else linux_generic;
              cachyRtBoreKernel = if system == "x86_64-linux" then cachyRtBoreKernel else null;
              mkRtKernel = mkRtKernel pkgs;
            } // extraSpecialArgs;
            modules = modules ++ [
              musnix.nixosModules.musnix
              disko.nixosModules.disko
              # Global HydraMesh service module
              hydramesh.nixosModules.default
            ];
          };

      in {
        # === NixOS Configurations ===
        nixosConfigurations = {
          archibaldOS-iso = mkNixosSystem "x86_64-linux" { } [
            "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
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
                usbutils libusb1 alsa-firmware alsa-tools dialog disko mkpasswd networkmanager
                audacity fluidsynth guitarix csound csound-qt faust portaudio rtaudio supercollider qjackctl
                surge zrythm carla puredata helm vmpk qmidinet faust2alsa faust2csound faust2jack calf
                hydramesh  # HydraMesh CLI available in live ISO
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

              branding.enable = true;

              users.users.nixos = {
                isNormalUser = true;
                initialPassword = "nixos";
                extraGroups = [ "wheel" "audio" "jackaudio" "video" "networkmanager" ];
                shell = pkgs.bashInteractive;
              };

              services.displayManager.autoLogin = {
                enable = true;
                user = "nixos";
              };

              system.activationScripts.mkdirScreenshots.text = ''
                mkdir -p /home/nixos/Pictures/Screenshots
                chown nixos:users /home/nixos/Pictures/Screenshots
              '';

              # Optional demo: start HydraMesh node on boot
              services.hydramesh = {
                enable = false;  # Set to true for live demo
                firewallEnable = true;
              };
            })
          ];

          archibaldOS-orangepi5 = mkNixosSystem "aarch64-linux" { inherit linux_rk3588; } [
            nixos-rk3588.nixosModules.orangepi5
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
                jack2 qjackctl jack_capture guitarix qtractor puredata
                pavucontrol helvum qpwgraph jalv
                hydramesh
              ];

              boot.kernelParams = [
                "threadirqs"
                "cpufreq.default_governor=performance"
                "nohz_full=1-7"
              ];

              powerManagement.cpuFreqGovernor = "performance";

              branding.enable = true;

              services.hydramesh = {
                enable = true;
                firewallEnable = true;
              };
            })
          ];

          archibaldOS-rpi3b = mkNixosSystem "aarch64-linux" { inherit linux_rpi3; } [
            ./modules/base.nix
            ./modules/audio.nix
            ./modules/users.nix
            ({ config, pkgs, lib, ... }: {
              system.stateVersion = "24.11";

              boot.loader.grub.enable = false;
              boot.loader.generic-extlinux-compatible.enable = true;

              fileSystems."/boot" = {
                device = "/dev/disk/by-label/BOOT";
                fsType = "vfat";
              };

              sdImage.populateFirmwareCommands = ''
                mkdir -p ./boot
                cp -r ${pkgs.raspberrypifw}/share/raspberrypi/boot/* ./boot/
                cat > ./boot/config.txt <<'EOF'
                dtparam=audio=on
                force_turbo=1
                arm_freq=1200
                gpu_mem=16
                disable_overscan=1
                kernel=vmlinuz
                initramfs initrd.img followkernel
                EOF
                ln -s ${pkgs.raspberrypifw}/share/raspberrypi/boot/bcm2837-rpi-3-b.dtb ./boot/
              '';

              hardware.enableRedistributableFirmware = true;
              hardware.graphics.enable = false;

              musnix = {
                enable = true;
                kernel.realtime = false;
                rtirq = { enable = true; highList = "snd_usb_audio"; };
                das_watchdog.enable = true;
              };

              services.pipewire.extraConfig.pipewire."92-low-latency" = {
                "context.properties" = {
                  "default.clock.rate" = 48000;
                  "default.clock.quantum" = 128;
                  "default.clock.min-quantum" = 128;
                  "default.clock.max-quantum" = 128;
                };
              };

              security.pam.loginLimits = [
                { domain = "@audio"; item = "rtprio"; value = "99"; }
                { domain = "@audio"; item = "memlock"; value = "unlimited"; }
                { domain = "@audio"; item = "nice"; value = "-19"; }
              ];

              environment.systemPackages = with pkgs; [
                jack2 pipewire alsa-utils usbutils vim git htop tmux guitarix puredata hydramesh
                (writeShellScriptBin "rt-check" ''
                  #!/usr/bin/env bash
                  echo "=== RT Check (RPi3B) ==="
                  uname -r
                  cat /sys/kernel/realtime 2>/dev/null || echo "RT not enabled"
                  ulimit -r
                  systemctl --user status pipewire
                '')
              ];

              boot.kernelParams = [
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

              services.hydramesh = {
                enable = true;
                firewallEnable = true;
              };
            })
          ];

          archibaldOS-server = mkNixosSystem "x86_64-linux" { } [
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

              services.hydramesh = {
                enable = true;
                firewallEnable = true;
              };
            })
          ];

          archibaldOS-dsp = mkNixosSystem "x86_64-linux" { } [
            ./modules/dsp.nix
            ({ config, pkgs, lib, ... }: {
              system.stateVersion = "24.11";

              archibaldOS.rtKernel = {
                enable = true;
                variant = "cachyos-rt-bore";
              };

              systemd.services.kexec-load = {
                description = "Load CachyOS RT-BORE kernel via kexec";
                wantedBy = [ "multi-user.target" ];
                after = [ "local-fs.target" ];
                serviceConfig = {
                  Type = "oneshot";
                  ExecStart = "${pkgs.kexec-tools}/bin/kexec -l /boot/kexec/vmlinuz --initrd=/boot/kexec/initrd --append=\"$(cat /boot/kexec/cmdline)\"";
                  RemainAfterExit = true;
                };
              };

              systemd.services.kexec-exec = {
                description = "Execute kexec reboot into real-time kernel";
                wantedBy = [ "multi-user.target" ];
                requires = [ "kexec-load.service" ];
                serviceConfig = {
                  Type = "oneshot";
                  ExecStart = "${pkgs.kexec-tools}/bin/kexec -e";
                };
              };

              services.hydramesh.enable = false;  # Minimal DSP – optional
            })
          ];
        };

        # === Compressed Build Artifacts ===
        packages.${system} =
          let
            configs = self.nixosConfigurations;
          in {
            iso = pkgs.runCommand "archibaldOS.iso.xz" {
              nativeBuildInputs = [ pkgs.xz ];
            } ''
              cp ${configs.archibaldOS-iso.config.system.build.isoImage}/iso/*.iso $out.tmp.iso
              xz -9e --threads=0 $out.tmp.iso
              mv $out.tmp.iso.xz $out
            '';

            dsp = pkgs.runCommand "archibaldOS-dsp.img.xz" {
              nativeBuildInputs = [ pkgs.xz ];
            } ''
              cp ${configs.archibaldOS-dsp.config.system.build.diskoImages}/disk1.img $out.tmp.img
              xz -9e --threads=0 $out.tmp.img
              mv $out.tmp.img.xz $out
            '';

            orangepi5 = pkgs.runCommand "archibaldOS-orangepi5.img.xz" {
              nativeBuildInputs = [ pkgs.xz ];
            } ''
              img=$(ls ${configs.archibaldOS-orangepi5.config.system.build.sdImage}/sd-image/*.img)
              cp $img $out.tmp.img
              xz -9e --threads=0 $out.tmp.img
              mv $out.tmp.img.xz $out
            '';

            rpi3b = pkgs.runCommand "archibaldOS-rpi3b.img.xz" {
              nativeBuildInputs = [ pkgs.xz ];
            } ''
              img=$(ls ${configs.archibaldOS-rpi3b.config.system.build.sdImage}/sd-image/*.img)
              cp $img $out.tmp.img
              xz -9e --threads=0 $out.tmp.img
              mv $out.tmp.img.xz $out
            '';

            hydramesh = hydramesh.packages.${system}.default;
          };

        # === Development Shells ===
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            audacity ardour fluidsynth musescore guitarix csound faust portaudio rtaudio supercollider qjackctl surge
            pcmanfm vim nixd git htop tmux
            hydramesh sbcl grpc protobuf openssl zlib
          ];

          shellHook = ''
            echo "══════════════════════════════════════════════════════════════"
            echo "ArchibaldOS + HydraMesh v2.2.0 development shell ready!"
            echo "• HydraMesh CLI: $(type -p hydramesh || echo 'not found')"
            echo "• Run: hydramesh help"
            echo "• Enable service in config: services.hydramesh.enable = true;"
            echo "══════════════════════════════════════════════════════════════"
          '';
        };

        # === Exported Modules ===
        nixosModules = {
          hydramesh = hydramesh.nixosModules.default;
          archibaldOS = ./modules;
          default = ./modules;
        };

        overlays.default = final: prev: {
          hydramesh = hydramesh.packages.${system}.default;
        };
      }
    );
}
