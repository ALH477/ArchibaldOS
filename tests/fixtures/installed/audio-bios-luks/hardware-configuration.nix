# SPDX-License-Identifier: BSD-3-Clause
# A BIOS scan with a LUKS root and no separate /boot.
{ lib, modulesPath, ... }:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];
  boot.initrd.availableKernelModules = [ "ahci" "sd_mod" ];
  boot.initrd.luks.devices."luks-root".device = "/dev/disk/by-uuid/11111111-0000-4000-8000-000000000001";
  fileSystems."/" = { device = "/dev/mapper/luks-root"; fsType = "ext4"; };
  swapDevices = [ { device = "/dev/mapper/luks-swap"; } ];
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
