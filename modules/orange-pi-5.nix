# modules/orange-pi-5.nix
{ config, lib, pkgs, ... }:

{
  boot.kernelModules = lib.mkAfter [
    "snd_aloop"
  ];

  hardware.graphics = {
    enable = lib.mkDefault true;
  };
}
