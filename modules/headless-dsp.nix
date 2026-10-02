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
#   2. NetJack2 (secondary, for audio to and from other machines): this
#      guest's JACK runs jack2's netmanager on UDP 19000 (modules/netjack.nix,
#      role "manager"). The host, or any box, joins with netadapter and
#      appears here as a client named after its host. checks.netjack2 runs
#      that exchange between two real JACK servers. The earlier units ran
#      jack_netsource, the netone MASTER, here and on the host (two masters,
#      no slave), with flags it does not have; that chain could not form.
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
# JACK2 wrapped with rt-exec: raised rlimits + SCHED_FIFO(99) + CPU pin +
# THP disable BEFORE jackd starts. jackd -R locks its own memory; a lock taken
# by the wrapper would not survive execve (see rt-exec.c).
#
# 64 samples @ 96kHz = 0.67ms round-trip latency target.
#
# ── Control plane ────────────────────────────────────────────────────────────
# The DSP control socket accepts load_fx/synth.load, which make the engine
# dlopen a path, so who can reach it is decided here rather than assumed:
#   - the TCP bridge runs as the engine's own unprivileged user, sandboxed, and
#     socat itself refuses any peer outside `archibald.dsp.control.allowFrom`
#     (in-process, so it holds even where cgroup IP filtering is unavailable;
#     IPAddressAllow repeats it as defence in depth);
#   - the firewall is ON, opening only NETJACK, the control port and ssh;
#   - sshd takes keys only (the image ships a well-known console password).
# The default allowFrom is QEMU user-mode networking's host address, which is
# where a host-side `hostfwd=tcp:127.0.0.1:P-:7777` (Oligarchy's dsp-vm module)
# arrives from. Over a tap/bridge, set it to the host's address.
# See docs/security.md.
# ============================================================================
{ config, lib, pkgs, ... }:

let
  rt-exec = pkgs.callPackage ./rt-exec.nix { };
  ctl = config.archibald.dsp.control;
in
{
  imports = [ ./audio.nix ./demod-rt.nix ./dsp-control-bridge.nix ./netjack.nix ];

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
  # On. The port lists below used to be dead config under `enable = false`.
  networking.firewall.enable = lib.mkForce true;

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

  # ── JACK2 on the passed-through interface ─────────────────────────────────
  # JACK2 uses the ALSA device from the VFIO-passed USB controller as its
  # audio backend; NetJack2 rides on it as an internal client (below), so
  # one server serves both the hardware and the network.
  systemd.services.jack2-alsa = {
    description = "JACK2 ALSA Backend — Direct VFIO USB Audio";
    wantedBy = [ "multi-user.target" ];
    # NOT Requires=pipewire.service. PipeWire is not systemWide here, so NixOS
    # masks the system pipewire.service (enable = false -> /dev/null), and a
    # Requires= on a masked unit fails the start: this unit, and everything
    # requiring it, could not come up. jackd opens hw:0 itself.
    after = [ "sound.target" ];

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
        exec ${rt-exec}/bin/rt-exec --cpu 0 --prio 99 -- \
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

  # ── NetJack2 manager: boxes and the host join this guest's JACK ──────────
  archibald.jack = { user = "dsp"; unit = "jack2-alsa.service"; };
  archibald.netjack.role = "manager";

  # NetJack2 and the control bridge. ssh opens its own port (openFirewall).
  # The guest sits behind its host, so NetJack2's manager port and the
  # ephemeral data ports it negotiates are opened on every interface here.
  networking.firewall.allowedTCPPorts = [ ctl.port ];
  networking.firewall.allowedUDPPorts = [ config.archibald.netjack.port ];
  networking.firewall.allowedUDPPortRanges = [{ from = 1024; to = 65535; }];

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

  # Keys only. `dsp` ships with a published password and is in wheel, so a
  # password login over ssh was root for anyone who could reach port 22. The
  # serial console (autologin) is the way in without a key; add one with
  # users.users.dsp.openssh.authorizedKeys.keys in your own module.
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  # ── DSP Control Bridge — TCP → the engine's control socket ────────────────
  # Defined in dsp-control-bridge.nix (shared with the companion profile).
  # Here it runs as the engine's user and accepts QEMU user-net's host only.
  archibald.dsp.control = {
    enable = true;
    user = "dsp";
    group = "audio";
  };

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
    "console=ttyS0"
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
