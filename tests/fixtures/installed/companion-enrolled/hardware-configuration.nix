# SPDX-License-Identifier: BSD-3-Clause
# A UEFI scan, as nixos-generate-config --show-hardware-config prints it.
{ lib, modulesPath, ... }:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];
  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
  fileSystems."/" = { device = "/dev/disk/by-uuid/0f6c1a2e-0000-4000-8000-000000000001"; fsType = "ext4"; };
  fileSystems."/boot" = { device = "/dev/disk/by-uuid/ABCD-1234"; fsType = "vfat"; options = [ "fmask=0077" "dmask=0077" ]; };
  swapDevices = [ ];
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
