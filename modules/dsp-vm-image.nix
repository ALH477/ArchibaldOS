# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025-2026 DeMoD LLC. All rights reserved.
# ============================================================================
# DSP VM disk image — bootable under BIOS (SeaBIOS) AND UEFI (OVMF).
# ============================================================================
#
# The image used to be BIOS-only: GRUB on /dev/vda with efiSupport = false and
# a plain MBR layout, so it had no EFI system partition. The host this guest is
# built for — Oligarchy's vm-manager dsp-vm module — boots with OVMF by default
# (`ovmf = true`), and OVMF finding no ESP falls through to PXE and sits in the
# netboot loop. Oligarchy hit exactly that with the image this one replaced and
# records it at its own `dsp-vm-qcow` output ("qcow-efi, NOT qcow").
#
# `partitionTableType = "hybrid"` gives GPT with a BIOS-boot partition AND an
# ESP, and GRUB is installed for both: to /dev/vda for SeaBIOS, and as the
# removable EFI loader (EFI/BOOT/BOOTX64.EFI) for OVMF, which needs no NVRAM
# entry and so survives a fresh OVMF_VARS. One image, either firmware.
#
# `packages.dsp-vm-boot-proxy` in flake.nix boots an image built by this same
# module under both firmwares and requires the guest to reach userspace. It
# uses a stock kernel instead of the RT one (the layout and loader are what is
# under test, and they do not depend on the kernel), so the RT image itself is
# [UNTESTED] under OVMF until someone boots it.
{ config, lib, pkgs, modulesPath, ... }:

{
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";   # make-disk-image `label` below
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/ESP";     # make-disk-image: mkfs.vfat -n ESP
    fsType = "vfat";
    options = [ "fmask=0077" "dmask=0077" ];
  };

  boot.loader.grub = {
    enable = true;
    device = "/dev/vda";            # BIOS: core.img into the bios_grub partition
    efiSupport = true;              # UEFI: grubx64.efi onto the ESP ...
    efiInstallAsRemovable = true;   # ... as EFI/BOOT/BOOTX64.EFI: no NVRAM needed
  };
  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.timeout = 1;          # Fast boot — no menu delay

  boot.initrd.availableKernelModules = [ "virtio_pci" "virtio_blk" "virtio_scsi" ];

  system.build.qcow2 = import (modulesPath + "/../lib/make-disk-image.nix") {
    inherit config lib pkgs;
    diskSize = 8192;                # 8GB — NixOS + JACK2 + PipeWire + musnix RT stack
    format = "qcow2";
    partitionTableType = "hybrid";
    label = "nixos";                # Must match fileSystems."/".device above
  };
}
