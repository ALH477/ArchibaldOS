# hardware/jh7110.nix — StarFive JH7110 hardware profile (RISC-V)
# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025-2026 DeMoD LLC.
#
# Hardware enablement for boards built on the StarFive JH7110 SoC
# (quad SiFive U74-MC @ 1.5 GHz) — e.g. the VisionFive 2 and the
# DeepComputing Framework Laptop 13 RISC-V mainboard. Targets a headless
# RT-audio appliance: class-compliant USB audio, USB-attached I2C
# peripherals (the JH7110 boards do not expose convenient board-internal
# I2C headers), and console framebuffer output.

{ config, lib, pkgs, ... }:

with lib;

{
  # ── Boot ────────────────────────────────────────────────────────
  boot.initrd.availableKernelModules = [
    # Storage (SD / eMMC / USB)
    "mmc_block" "dw_mmc" "dw_mmc-starfive" "sdhci_pci"
    "usbhid" "usb_storage" "sd_mod" "uas"

    # JH7110 platform bits
    "phy_jh7110_pcie"
    "phy_jh7110_usb"
    "pcie_starfive"

    # i2c designware (StarFive uses the DesignWare i2c IP)
    "i2c_designware_core"
    "i2c_designware_platform"
    "i2c_dev"

    # USB audio
    "snd_usb_audio"
  ];

  boot.kernelModules = [
    "i2c-dev"
    "snd-usb-audio"
  ];

  # ── Boot Loader ─────────────────────────────────────────────────
  # JH7110 boots via U-Boot → extlinux.conf on the SD root. These boards
  # ship U-Boot in SPI flash, so the SD image's firmware partition is
  # intentionally left empty.
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible = {
    enable = true;
    # Let U-Boot discover the device tree at runtime; pinning a FDTDIR is
    # risky until the DTB path is confirmed for a given board.
    useGenerationDeviceTree = false;
  };

  # ── SD Image Population ─────────────────────────────────────────
  sdImage = {
    populateFirmwareCommands = "";
    populateRootCommands = ''
      mkdir -p ./files/boot
      ${config.boot.loader.generic-extlinux-compatible.populateCmd} -c ${config.system.build.toplevel} -d ./files/boot
    '';
  };

  # ── Filesystem ──────────────────────────────────────────────────
  # `sd-image.nix` provisions `/` with label NIXOS_SD and a FAT firmware
  # partition at /boot/firmware. Leave those definitions to it.

  swapDevices = [];  # Deterministic memory behavior.

  # ── i2c ─────────────────────────────────────────────────────────
  hardware.i2c.enable = true;

  services.udev.extraRules = ''
    # i2c bus access
    SUBSYSTEM=="i2c-dev", GROUP="i2c", MODE="0660"

    # USB audio interface — force a small low-latency prealloc
    ACTION=="add", SUBSYSTEM=="sound", KERNEL=="card*", \
      ATTR{id}!="HDMI*", \
      RUN+="${pkgs.bash}/bin/bash -c 'echo 64 > /sys/class/sound/%k/pcm0p/sub0/prealloc 2>/dev/null || true'"
  '';

  # ── Display ─────────────────────────────────────────────────────
  # Headless console framebuffer only (mainline KMS/DRM; no vendor GPU
  # stack on this image). Graphical desktops are an x86-only path.
  services.xserver.enable = false;
  services.displayManager.enable = false;
  hardware.graphics.enable = true;

  boot.kernelParams = [
    "console=ttyS0,115200"
    "console=tty0"
    "usbcore.autosuspend=-1"
  ];

  # ── USB Audio ───────────────────────────────────────────────────
  # Default ALSA to the first USB audio card. 48 kHz / S32_LE is a safe
  # class-compliant baseline; override in a machine config as needed.
  environment.etc."asound.conf".text = ''
    defaults.pcm.card 1
    defaults.ctl.card 1

    pcm.usb {
      type hw
      card 1
      device 0
      rate 48000
      format S32_LE
    }
  '';

  # ── Power ───────────────────────────────────────────────────────
  services.upower.enable = true;

  # ── Memory ──────────────────────────────────────────────────────
  # Deterministic-memory posture for the RT path: no swap, aggressive
  # reclaim, a reserved free-page headroom pool.
  boot.kernel.sysctl = {
    "vm.swappiness" = 0;
    "vm.vfs_cache_pressure" = 200;
    "vm.min_free_kbytes" = 16384;
  };

  documentation.enable = false;
  documentation.nixos.enable = false;
  programs.command-not-found.enable = false;
}
