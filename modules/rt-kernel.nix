# modules/rt-kernel.nix
{ config, lib, pkgs, specialArgs, ... }:

let
  cfg = config.archibaldOS.rtKernel;
in {
  options.archibaldOS.rtKernel = {
    enable = lib.mkEnableOption "Enable real-time kernel";

    customPath = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to a custom kernel.nix file at the root of the flake.";
    };

    variant = lib.mkOption {
      type = lib.types.enum [ "standard" "cachyos-rt-bore" ];
      default = "standard";
      description = "Kernel variant: standard RT or CachyOS RT BORE (x86 only).";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = if cfg.variant == "cachyos-rt-bore" then config.nixpkgs.system == "x86_64-linux" else true;
        message = "CachyOS RT BORE kernel is only available for x86_64-linux.";
      }
    ];

    boot.kernelPackages = lib.mkForce (pkgs.linuxPackagesFor (
      if cfg.customPath != null then
        (import cfg.customPath { inherit pkgs; })
      else if cfg.variant == "cachyos-rt-bore" then
        specialArgs.cachyRtBoreKernel
      else
        (specialArgs.mkRtKernel specialArgs.standardKernel)
    ));

    musnix.kernel.realtime = true;
    musnix.kernel.packages = config.boot.kernelPackages;
  };
}
