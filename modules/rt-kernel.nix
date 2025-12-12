# modules/rt-kernel.nix

# Copyright 2025 DeMoD LLC

# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



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
