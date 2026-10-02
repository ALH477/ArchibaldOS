# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# The role configurations the role gates evaluate. Small NixOS configurations
# (containers: no boot loader, no disks) that import only the modules under
# test, so checks.netjack2 can run the exact commands modules/netjack.nix and
# modules/jack-graph.nix generate.
{ nixpkgs, system }:

let
  lib = nixpkgs.lib;
  stubs = pkgs: {
    # demod-engine's packages, stubbed: these configs are read, never booted.
    demod-orchestrator = pkgs.writeShellScriptBin "demod-orchestrator" "exit 0";
    demod-rt = pkgs.writeShellScriptBin "demod-rt" "exit 0";
    demod-remote-bridge = pkgs.writeShellScriptBin "demod-remote-bridge" "exit 0";
  };
  mk = modules: lib.nixosSystem {
    inherit system;
    modules = [
      ({ pkgs, ... }: {
        boot.isContainer = true;
        system.stateVersion = "24.11";
        users.users.dsp = { isNormalUser = true; };
        archibald.jack.user = "dsp";
      })
    ] ++ modules;
  };
in
{
  # The DSP host: netmanager, the engine, every box wired into demod-rt.
  manager = mk [
    ../modules/netjack.nix
    ../modules/demod-engine.nix
    ({ pkgs, ... }: {
      archibald.netjack.role = "manager";
      archibald.engine = { enable = true; io = "none"; packages = stubs pkgs; };
    })
  ];
  adapter = name: mk [
    ../modules/netjack.nix
    {
      networking.hostName = name;
      archibald.netjack = { role = "adapter"; address = "127.0.0.1"; };
    }
  ];
}
