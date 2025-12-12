# flake.nix
# Copyright 2025 DeMoD LLC
# SPDX-License-Identifier: BSD-3-Clause
#
# ArchibaldOS: Unified NixOS for real-time audio, robotics, LIDAR, secure routing, and DSP.
# Powered by profile-selector.nix — one-line builds for all targets.

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
        hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";  # Run build to get real hash
      };

      headersPkg = pkgs.fetchurl {
        url = headersUrl;
        hash = "sha256-BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=";  # Run build to get real hash
      };

      unpacked = pkgs.runCommand "unpack-cachy" {} ''
        mkdir -p $out/kernel $out/headers
        ${pkgs.zstd}/bin/zstd -d ${kernelPkg} -o - | tar xf - -C $out/kernel
        ${pkgs.zstd}/bin/zstd -d ${headersPkg} -o - | tar xf - -C $out/headers
      '';
    in pkgs.buildLinux {
      version = "${kernelVersion}-cachyos-rt-bore";
      modDirVersion = "${kernelVersion}-cachyos-rt-bore";
      src = unpacked + "/kernel";
      kernelPatches = [];
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
    linux_rpi3 = pkgsArm.linux_rpi3;
    linux_rk3588 = pkgsArm.callPackage (nixos-rk3588 + "/pkgs/linux-rk3588/default.nix") {};
    linux_generic = pkgsArm.linux_6_1;

    # Helper: Create system with profile
    mkSystem = system: profile: nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        standardKernel = if system == x86System then pkgsX86.linux_latest else linux_generic;
        cachyRtBoreKernel = if system == x86System then cachyRtBoreKernel pkgsX86 else null;
        mkRtKernel = if system == x86System then mkRtKernel pkgsX86 else mkRtKernel pkgsArm;
      };
      modules = [
        musnix.nixosModules.musnix
        disko.nixosModules.disko
        ./modules/profile-selector.nix
        ({ config, ... }: {
          archibaldOS.profile = profile;
        })
      ];
    };

  in {
    # === NixOS Configurations (via profiles) ===
    nixosConfigurations = {
      # x86_64
      archibaldOS-iso           = mkSystem x86System "audio-live-iso";
      archibaldOS-workstation   = mkSystem x86System "audio-workstation";
      archibaldOS-server        = mkSystem x86System "custom";  # Manual server config
      archibaldOS-dsp           = mkSystem x86System "dsp-coprocessor";
      archibaldOS-lidar-station = mkSystem x86System "lidar-station";

      # ARM64
      archibaldOS-orangepi5     = mkSystem armSystem "drone-brain";
      archibaldOS-rpi5          = mkSystem armSystem "drone-brain";
      archibaldOS-rpi3b         = mkSystem armSystem "drone-brain";
      archibaldOS-rock5         = mkSystem armSystem "drone-brain";
      archibaldOS-router        = mkSystem armSystem "secure-router";
      archibaldOS-arm-generic   = mkSystem armSystem "custom";
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

      rpi5 = pkgsArm.runCommandLocal "archibaldOS-rpi5.img.xz" {
        nativeBuildInputs = [ pkgsArm.xz ];
      } ''
        mkdir -p $out.tmp-dir
        cp -r ${self.nixosConfigurations.archibaldOS-rpi5.config.system.build.sdImage}/sd-image/* $out.tmp-dir/
        img_file=$(ls $out.tmp-dir/*.img)
        xz -9e --threads=0 "$img_file"
        mv "$img_file.xz" "$out"
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

      rock5 = pkgsArm.runCommandLocal "archibaldOS-rock5.img.xz" {
        nativeBuildInputs = [ pkgsArm.xz ];
      } ''
        mkdir -p $out.tmp-dir
        cp -r ${self.nixosConfigurations.archibaldOS-rock5.config.system.build.sdImage}/sd-image/* $out.tmp-dir/
        img_file=$(ls $out.tmp-dir/*.img)
        xz -9e --threads=0 "$img_file"
        mv "$img_file.xz" "$out"
      '';

      router = pkgsArm.runCommandLocal "archibaldOS-router.img.xz" {
        nativeBuildInputs = [ pkgsArm.xz ];
      } ''
        mkdir -p $out.tmp-dir
        cp -r ${self.nixosConfigurations.archibaldOS-router.config.system.build.sdImage}/sd-image/* $out.tmp-dir/
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
    };

    # === Dev Shells ===
    devShells.${x86System}.default = (mkPkgs x86System).mkShell {
      packages = with (mkPkgs x86System); [
        audacity ardour fluidsynth musescore guitarix
        csound faust portaudio rtaudio supercollider qjackctl
        surge pcmanfm vim nixd
        rosPackages.humble.ros-core rosPackages.humble.rviz2
        pcl-tools
      ];
    };

    devShells.${armSystem}.default = (mkPkgs armSystem).mkShell {
      packages = with (mkPkgs armSystem); [
        jack2 guitarix puredata vim
        rosPackages.humble.ros-core
        i2c-tools
      ];
    };
  };
}
