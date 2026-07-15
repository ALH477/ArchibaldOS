# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 DeMoD LLC. All rights reserved.
# ============================================================================
# ArchibaldOS Headless DSP Guest — Maximum RT Determinism
# ============================================================================
# DSP coprocessor VM with DUAL audio paths:
#
#   1. VFIO USB HOST CONTROLLER PASSTHROUGH (primary):
#      The host passes an entire XHCI USB controller to the VM via VFIO.
#      The audio interface plugged into that controller is owned directly
#      by the VM — direct ALSA access, zero-copy, zero-latency. No hypervisor
#      translation in the audio path.
#
#   2. NETJACK (secondary, for routing processed audio back to host):
#      JACK2 runs netone driver on port 4713. The host connects via
#      jack_netsource to receive the DSP-processed audio and route it
#      through PipeWire to speakers/other apps.
#
# Audio flow:
#   USB audio interface → VFIO controller → ALSA → JACK2 → demod-rt (Faust FX)
#                                                  → NETJACK → host PipeWire
#
# Kernel: PREEMPT_RT (mainline RT patchset via musnix) — full kernel
# preemption including IRQ handlers. Every low-level knob maxed:
#   - threadirqs, nohz_full, rcu_nocbs — tickless + threaded IRQs
#   - processor.max_cstate=0, idle=poll — zero C-state latency
#   - nmi_watchdog=0, nosoftlockup, mce=ignore_ce — no overhead
#   - transparent_hugepage=never — no multi-ms page collapse spikes
#   - skew_tick=1 — avoid synchronized timer bursts
#   - kernel.sched_rt_runtime_us=-1 — RT throttling completely disabled
#   - kernel.timer_migration=0 — keep timers local
#   - dev.rtc.max-user-freq=8192 — max timer precision
#
# JACK2 wrapped with rt-exec: mlockall(MCL_FUTURE) + SCHED_FIFO(99) + CPU
# pin + THP disable + max rlimits BEFORE jackd starts.
#
# 64 samples @ 96kHz = 0.67ms round-trip latency target.
# ============================================================================
{ config, lib, pkgs, ... }:

let
  rt-exec = pkgs.callPackage ./rt-exec.nix { };
in
{
  imports = [ ./audio.nix ./demod-rt.nix ];

  # ── musnix: PREEMPT_RT kernel + RT audio tooling ────────────────────────────
  musnix = {
    enable = true;
    kernel.realtime = true;
    alsaSeq.enable = true;
    rtirq.enable = true;
    das_watchdog.enable = true;
  };

  # ── Headless ───────────────────────────────────────────────────────────────
  services.xserver.enable = lib.mkForce false;
  services.displayManager.enable = lib.mkForce false;
  services.udisks2.enable = lib.mkForce false;
  services.blueman.enable = lib.mkForce false;
  services.avahi.enable = lib.mkForce false;
  networking.firewall.enable = false;

  # ── PipeWire: ALSA routing only. No JACK compat (standalone JACK2 used). ───
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = lib.mkForce false;
    jack.enable = lib.mkForce false;
    wireplumber.enable = true;
  };

  # ── PipeWire: 32/96k — pushed to the limit ─────────────────────────────────
  # 32 samples @ 96kHz = 0.33ms per period. Matches JACK2 ALSA backend.
  services.pipewire.extraConfig.pipewire."92-dsp-latency" = {
    "context.properties" = {
      "default.clock.rate" = 96000;
      "default.clock.quantum" = 32;
      "default.clock.min-quantum" = 16;
      "default.clock.max-quantum" = 256;
    };
  };

  # ── JACK2 with NETJACK netone driver (routes processed audio to host) ──────
  # JACK2 uses the ALSA device from the VFIO-passed USB controller as its
  # audio backend, AND runs the netone driver to expose NETJACK on port 4713
  # so the host can pull the processed audio.
  #
  # The -d netone driver creates a NETJACK master. But we also need the
  # actual ALSA device. JACK2 can only use one backend driver at a time.
  # Solution: run JACK2 with the ALSA backend (direct hardware access via
  # VFIO), then run a SEPARATE jack_netsource instance that bridges the
  # local JACK to the host over the network.
  systemd.services.jack2-alsa = {
    description = "JACK2 ALSA Backend — Direct VFIO USB Audio";
    wantedBy = [ "multi-user.target" ];
    after = [ "pipewire.service" "sound.target" ];
    requires = [ "pipewire.service" ];

    serviceConfig = {
      Type = "simple";
      User = "dsp";
      Group = "audio";
      Restart = "always";
      RestartSec = 3;

      Nice = -20;
      IOSchedulingClass = "realtime";
      IOSchedulingPriority = 0;
      CPUSchedulingPolicy = "fifo";
      CPUSchedulingPriority = 99;
      LimitRTPrio = 99;
      LimitMEMLOCK = "infinity";
      LimitNICE = -20;

      # jackd with ALSA backend — direct hardware access to the VFIO USB
      # audio interface. -d alsa -d hw:0 uses the first ALSA device (the
      # passed-through USB interface).
      # 32 frames @ 96kHz = 0.33ms per period, 0.67ms buffer (n=2).
      # Input latency: ~0.46ms (0.33ms period + 0.125ms USB micro-frame).
      # If xruns, bump to -p 48 or -p 64.
      ExecStart = pkgs.writeShellScript "jack2-alsa-start" ''
        exec ${rt-exec}/bin/rt-exec \
          ${pkgs.jack2}/bin/jackd \
          -R \
          -d alsa \
          -d hw:0 \
          -r 96000 \
          -p 32 \
          -n 2 \
          -i 2 \
          -o 2
      '';

      ExecStop = "${pkgs.coreutils}/bin/kill -TERM $MAINPID";
    };
  };

  # ── NETJACK bridge — exposes local JACK to host over network ───────────────
  # jack_netsource runs inside the VM and creates a NETJACK master on port
  # 4713. The host connects as a slave to pull processed audio.
  systemd.services.jack2-netjack-master = {
    description = "JACK2 NETJACK Master — Expose DSP Audio to Host";
    wantedBy = [ "multi-user.target" ];
    after = [ "jack2-alsa.service" ];
    requires = [ "jack2-alsa.service" ];

    serviceConfig = {
      Type = "simple";
      User = "dsp";
      Group = "audio";
      Restart = "on-failure";
      RestartSec = 5;

      ExecStart = pkgs.writeShellScript "jack2-netjack-master-start" ''
        # Wait for JACK ALSA to be ready
        sleep 2
        # Create NETJACK master on port 4713, bridging local JACK to network
        # 32 frames @ 96kHz = 0.33ms — matches JACK ALSA backend
        exec ${pkgs.jack2}/bin/jack_netsource \
          -n archibaldos-dsp \
          -p 4713 \
          -C 2 \
          -P 2 \
          -l 32 \
          -r 96000
      '';

      ExecStop = "${pkgs.coreutils}/bin/kill -TERM $MAINPID";
    };
  };

  # Open NETJACK port
  networking.firewall.allowedTCPPorts = [ 4713 ];
  networking.firewall.allowedUDPPorts = [ 4713 ];

  # ── Users ──────────────────────────────────────────────────────────────────
  users.users.dsp = {
    isNormalUser = true;
    description = "DSP Audio User";
    group = "audio";
    extraGroups = [ "jackaudio" "realtime" "wheel" "networkmanager" ];
    initialPassword = "dsp";
    shell = pkgs.bash;
  };

  services.getty.autologinUser = lib.mkForce "dsp";
  services.openssh.enable = true;

  # ── Packages ───────────────────────────────────────────────────────────────
  environment.systemPackages = with pkgs; [
    alsa-utils
    jack2
    jack-example-tools
    netcat
    htop
    vim
    git
    rtirq
    rt-exec
  ];

  # ── Aggressive RT kernel params ────────────────────────────────────────────
  boot.kernelParams = lib.mkForce [
    "threadirqs"
    "nohz_full=0"
    "rcu_nocbs=0"
    "processor.max_cstate=0"
    "intel_idle.max_cstate=0"
    "idle=poll"
    "highres=on"
    "clocksource=tsc"
    "tsc=reliable"
    "skew_tick=1"
    "nmi_watchdog=0"
    "nosoftlockup"
    "mce=ignore_ce"
    "audit=0"
    "rcupdate.rcu_cpu_stall_suppress=1"
    "transparent_hugepage=never"
    "quiet"
    "loglevel=3"
  ];

  # ── Deep sysctl tuning ─────────────────────────────────────────────────────
  boot.kernel.sysctl = {
    "vm.dirty_ratio" = lib.mkForce 5;
    "vm.dirty_background_ratio" = lib.mkForce 2;
    "vm.dirty_writeback_centisecs" = lib.mkForce 0;
    "net.core.rmem_max" = lib.mkForce 16777216;
    "net.core.wmem_max" = lib.mkForce 16777216;
    "net.core.rmem_default" = lib.mkForce 8388608;
    "net.core.wmem_default" = lib.mkForce 8388608;
    "net.ipv4.tcp_rmem" = lib.mkForce "4096 8388608 16777216";
    "net.ipv4.tcp_wmem" = lib.mkForce "4096 8388608 16777216";
    "kernel.sched_rt_runtime_us" = lib.mkForce (-1);
    "kernel.timer_migration" = lib.mkForce 0;
  };

  environment.etc."sysctl.d/99-dsp-maxrt.conf".text = ''
    dev.rtc.max-user-freq = 8192
    dev.hpet.max-user-freq = 8192
  '';

  services.timesyncd.enable = true;
  powerManagement.cpuFreqGovernor = lib.mkForce "performance";
}
