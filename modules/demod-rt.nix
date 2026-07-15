# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 DeMoD LLC. All rights reserved.
# ============================================================================
# DeMoD RT Engine — demod-rt audio processor for ArchibaldOS DSP VM
# ============================================================================
#
# This module wraps the DeMoD demod-rt binary as a systemd service inside
# the ArchibaldOS DSP VM. demod-rt is the hard-RT C audio callback that
# connects to JACK, loads Faust FX chains, and processes audio with
# SCHED_FIFO + mlockall at 64 samples / 96kHz = 0.67ms.
#
# The demod-rt binary is proprietary (PolyForm Shield 1.0.0). It is NOT
# included in this public repo. To use this module:
#
#   1. Build demod-rt from the DeMoD repo:
#        cd ~/demod-work && nix build .#demod-rt
#   2. Uncomment the `demod` input in flake.nix
#   3. Uncomment the `dsp-vm-demod` nixosConfiguration in flake.nix
#   4. Build: nix build .#dsp-vm-demod-qcow2
#
# Source: https://github.com/ALH477 (organization)
# License: PolyForm Shield 1.0.0 (source-available, non-commercial)
# Contact: alh477@proton.me
# ============================================================================
{ config, lib, pkgs, ... }:

let
  cfg = config.services.demod-rt;
in
{
  options.services.demod-rt = {
    enable = lib.mkEnableOption "DeMoD RT audio engine (demod-rt)";

    package = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      description = "Package providing the demod-rt binary.";
    };

    rtCore = lib.mkOption {
      type = lib.types.int;
      default = 0;
      description = "CPU core for RT audio process (single-core VM = 0).";
    };

    rtPriority = lib.mkOption {
      type = lib.types.int;
      default = 80;
      description = "SCHED_FIFO priority for RT audio (below USB IRQ at 90).";
    };

    faustLibs = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [ ];
      description = "Faust .so effect files to load into FX slots.";
      example = [ ./effects/demod_overdrive.so ./effects/demod_reverb.so ];
    };

    controlSocket = lib.mkOption {
      type = lib.types.str;
      default = "/run/demod/control.sock";
      description = "Path for the demod-rt control socket.";
    };

    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/demod";
      description = "Data directory for demod-rt state.";
    };
  };

  config = lib.mkIf cfg.enable {
    # ── Shared memory directories ─────────────────────────────────────────────
    # demod-rt uses POSIX shm for IPC with the orchestrator:
    #   /dev/shm/demod-params    — triple buffer (orchestrator → RT)
    #   /dev/shm/demod-audio-cmd — SPSC ring (orchestrator → RT)
    #   /dev/shm/demod-audio-evt — SPSC ring (RT → orchestrator)
    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir} 0755 dsp audio -"
      "d /run/demod 0755 dsp audio -"
    ];

    # ── demod-rt systemd service ──────────────────────────────────────────────
    # Runs demod-rt with maximum RT priority, wrapped by rt-exec for
    # mlockall(MCL_FUTURE) + SCHED_FIFO + CPU pin + THP disable.
    #
    # demod-rt connects to JACK (running as jack2-netjack service) and
    # processes audio through the Faust FX chain. Audio flows:
    #   host → NETJACK → JACK2 → demod-rt (Faust FX) → JACK2 → NETJACK → host
    systemd.services.demod-rt = {
      description = "DeMoD RT Audio Engine (demod-rt)";
      wantedBy = [ "multi-user.target" ];
      after = [ "jack2-netjack.service" "pipewire.service" ];
      requires = [ "jack2-netjack.service" ];

      serviceConfig = {
        Type = "simple";
        User = "dsp";
        Group = "audio";
        Restart = "always";
        RestartSec = 3;

        # RT priority — below jack2-netjack (99) so JACK gets CPU first
        Nice = -20;
        IOSchedulingClass = "realtime";
        IOSchedulingPriority = 0;
        CPUSchedulingPolicy = "fifo";
        CPUSchedulingPriority = cfg.rtPriority;
        CPUAffinity = toString cfg.rtCore;
        LimitRTPrio = 99;
        LimitMEMLOCK = "infinity";
        LimitNICE = -20;

        # Security: needs SYS_NICE for SCHED_FIFO, IPC_LOCK for mlockall
        CapabilityBoundingSet = [ "CAP_SYS_NICE" "CAP_IPC_LOCK" "CAP_SYS_RESOURCE" ];
        AmbientCapabilities = [ "CAP_SYS_NICE" "CAP_IPC_LOCK" "CAP_SYS_RESOURCE" ];
        NoNewPrivileges = false;

        ExecStart = let
          rtBin =
            if cfg.package != null
            then "${cfg.package}/bin/demod-rt"
            else "demod-rt";
          faustArgs = lib.concatMapStringsSep " " (lib: "--faust-lib ${toString lib}") cfg.faustLibs;
        in pkgs.writeShellScript "demod-rt-start" ''
          # Create control socket directory
          mkdir -p "$(dirname ${cfg.controlSocket})"
          mkdir -p ${cfg.dataDir}

          # rt-exec: mlockall(MCL_FUTURE) + SCHED_FIFO(99) + CPU pin + THP disable
          # before exec'ing demod-rt. demod-rt then connects to JACK and
          # enters the audio callback loop (64 samples @ 96kHz = 0.67ms).
          exec ${pkgs.writeShellScriptBin "rt-exec-wrapper" ''
            exec ${rtBin} \
              --core ${toString cfg.rtCore} \
              ${faustArgs}
          ''}/bin/rt-exec-wrapper
        '';

        ExecStop = "${pkgs.coreutils}/bin/kill -TERM $MAINPID";
      };

      # Environment for demod-rt
      environment = {
        DEMOD_RT_BIN =
          if cfg.package != null
          then "${cfg.package}/bin/demod-rt"
          else "demod-rt";
        DEMOD_RUNTIME_DIR = "/run/demod";
      };

      startLimitIntervalSec = 300;
      startLimitBurst = 5;
    };

    # ── DSP user needs wheel for socket creation ──────────────────────────────
    users.users.dsp.extraGroups = [ "wheel" "audio" "jackaudio" "realtime" ];
  };
}
