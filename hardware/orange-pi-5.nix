{ config, lib, pkgs, ... }:

{
  # Orange Pi 5 hardware overrides
  # The nixos-rk3588 module provides the base configuration

  # Audio-specific kernel modules for low-latency
  boot.kernelModules = lib.mkAfter [
    "snd_aloop"  # ALSA loopback for routing
  ];

  # Ensure graphics acceleration is enabled
  hardware.graphics = {
    enable = lib.mkDefault true;
  };
}
