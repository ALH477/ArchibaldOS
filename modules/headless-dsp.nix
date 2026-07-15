# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 DeMoD LLC. All rights reserved.
# ============================================================================
# ArchibaldOS Headless DSP Guest — Maximum RT Determinism
# ============================================================================
# Runs JACK2 standalone with NETJACK netone driver, listening on port 4713.
# The host connects via `jack_netsource` to route audio through this VM
# for real-time DSP processing.
#
# Kernel: PREEMPT_RT (mainline RT patchset via musnix) — NOT CachyOS BORE.
#   - Full kernel preemption including IRQ handlers (threadirqs)
#   - Tickless kernel (nohz_full) on all cores
#   - RCU callbacks offloaded
#   - C-states disabled (processor.max_cstate=0) — burns power, zero latency
#   - NMI watchdog disabled (frees perf counter, eliminates spurious overhead)
#   - High-resolution timers, TSC clocksource
#   - Transparent hugepages disabled (THP causes multi-ms latency spikes)
#   - Timer migration disabled (keep timers on local CPU)
#   - Skewed ticks (avoid synchronized timer interrupts)
#   - Soft lockup detector disabled
#   - Machine check errors ignored (no overhead)
#
# JACK2 wrapped with rt-exec: mlockall(MCL_FUTURE) + SCHED_FIFO(99) + CPU
# pin + THP disable + max rlimits BEFORE jackd starts. Eliminates page-fault
# xruns at the lowest level short of kernel patching.
#
# 64 samples @ 96kHz = 0.67ms round-trip latency target.
# ============================================================================
{ config, lib, pkgs, ... }:

let
  # Build the rt-exec wrapper from source
  rt-exec = pkgs.callPackage ./rt-exec.nix { };
in
{
  # ── Base audio config (RT limits, groups, kernel modules, sysctl) ──────────
  imports = [ ./audio.nix ];

  # ── musnix: PREEMPT_RT kernel + RT audio tooling ────────────────────────────
  musnix = {
    enable = true;
    kernel.realtime = true;       # PREEMPT_RT patchset — full kernel preemption
    alsaSeq.enable = true;        # ALSA sequencer for MIDI
    rtirq.enable = true;          # Prioritize audio IRQ threads
    das_watchdog.enable = true;   # Catch RT thread runaway (non-RT fallback)
  };

  # ── Headless: kill all desktop stuff ───────────────────────────────────────
  services.xserver.enable = lib.mkForce false;
  services.displayManager.enable = lib.mkForce false;

  # Disable unnecessary services for RT purity
  services.udisks2.enable = lib.mkForce false;
  services.blueman.enable = lib.mkForce false;
  services.avahi.enable = lib.mkForce false;
  networking.firewall.enable = false;  # NETJACK needs open port

  # ── PipeWire: keep for ALSA routing, but disable JACK compat ───────────────
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = lib.mkForce false;  # no PulseAudio needed in VM
    jack.enable = lib.mkForce false;   # standalone JACK2 handles this
    wireplumber.enable = true;
  };

  # ── PipeWire latency: 64/96k — pushed to the limit ─────────────────────────
  # 64 samples @ 96kHz = 0.67ms — half the standard 128/96k.
  # If this causes xruns, bump to 128. But try the limit first.
  services.pipewire.extraConfig.pipewire."92-dsp-latency" = {
    "context.properties" = {
      "default.clock.rate" = 96000;
      "default.clock.quantum" = 64;
      "default.clock.min-quantum" = 32;
      "default.clock.max-quantum" = 256;
    };
  };

  # ── JACK2 standalone with NETJACK netone driver — wrapped by rt-exec ───────
  # rt-exec does mlockall(MCL_FUTURE) + SCHED_FIFO(99) + CPU pin + THP disable
  # + max rlimits BEFORE jackd starts. This eliminates page-fault xruns.
  systemd.services.jack2-netjack = {
    description = "JACK2 NETJACK Master for DSP Coprocessor (RT-wrapped)";
    wantedBy = [ "multi-user.target" ];
    after = [ "pipewire.service" "sound.target" ];
    requires = [ "pipewire.service" ];

    serviceConfig = {
      Type = "simple";
      User = "dsp";
      Group = "audio";
      Restart = "always";
      RestartSec = 3;

      # Maximum RT priority at the systemd level too
      Nice = -20;
      IOSchedulingClass = "realtime";
      IOSchedulingPriority = 0;
      CPUSchedulingPolicy = "fifo";
      CPUSchedulingPriority = 99;
      LimitRTPrio = 99;
      LimitMEMLOCK = "infinity";
      LimitNICE = -20;

      # rt-exec wraps jackd: mlockall + SCHED_FIFO + CPU pin + THP disable
      # before exec'ing jackd. 64 frames @ 96kHz = 0.67ms round-trip.
      ExecStart = pkgs.writeShellScript "jack2-netjack-start" ''
        exec ${rt-exec}/bin/rt-exec \
          ${pkgs.jack2}/bin/jackd \
          -R \
          -d netone \
          -i 2 \
          -o 2 \
          -r 96000 \
          -p 64 \
          -n 2 \
          -I 0 \
          -O 0 \
          --net-port 4713
      '';

      ExecStop = "${pkgs.coreutils}/bin/kill -TERM $MAINPID";
    };
  };

  # Open NETJACK port
  networking.firewall.allowedTCPPorts = [ 4713 ];
  networking.firewall.allowedUDPPorts = [ 4713 ];

  # ── DSP user ───────────────────────────────────────────────────────────────
  users.users.dsp = {
    isNormalUser = true;
    description = "DSP Audio User";
    group = "audio";
    extraGroups = [ "jackaudio" "realtime" "wheel" "networkmanager" ];
    initialPassword = "dsp";
    shell = pkgs.bash;
  };

  # Console autologin
  services.getty.autologinUser = lib.mkForce "dsp";

  # ── SSH for remote management ──────────────────────────────────────────────
  services.openssh.enable = true;

  # ── Minimal packages ───────────────────────────────────────────────────────
  environment.systemPackages = with pkgs; [
    alsa-utils
    jack2
    jack-example-tools
    netcat
    htop
    vim
    git
    rtirq
    rt-exec           # the RT wrapper itself — useful for manual testing
  ];

  # ── Aggressive RT kernel params — pushed to the limit ──────────────────────
  boot.kernelParams = lib.mkForce [
    # PREEMPT_RT is in the kernel via musnix; these params tune it.
    "threadirqs"              # Force all IRQ handlers into kernel threads
    "nohz_full=0"             # Tickless on CPU 0 — no timer interrupts except clock
    "rcu_nocbs=0"             # Offload RCU callbacks to kthreads

    # ── C-state zeroing: never enter any power-saving state ──────────────────
    "processor.max_cstate=0"  # Block all C-states (generic)
    "intel_idle.max_cstate=0" # Block all C-states (Intel idle driver)
    "idle=poll"               # Halt → busy-wait (maximum power, zero wakeup latency)

    # ── Timer precision ─────────────────────────────────────────────────────
    "highres=on"              # High-resolution timers (hrtimer)
    "clocksource=tsc"         # TSC — most precise on x86 (nanosecond resolution)
    "tsc=reliable"            # Trust the TSC — skip validation overhead
    "skew_tick=1"             # Offset timer ticks across CPUs — avoids
                              # synchronized timer bursts that cause latency spikes

    # ── Disable overhead sources ─────────────────────────────────────────────
    "nmi_watchdog=0"          # Disable NMI watchdog — frees a perf counter
    "nosoftlockup"            # Disable soft lockup detector — eliminates
                              # periodic watchdog overhead
    "mce=ignore_ce"           # Ignore machine check errors — no polling overhead
    "audit=0"                 # Disable audit subsystem (syscall logging)
    "rcupdate.rcu_cpu_stall_suppress=1"  # Suppress RCU stall warnings

    # ── Memory: disable THP — eliminates multi-ms page collapse/split spikes ─
    "transparent_hugepage=never"

    # ── Disable boot delays ──────────────────────────────────────────────────
    "quiet"                   # Reduce dmesg spam — less console overhead
    "loglevel=3"              # Only warnings/errors to console
  ];

  # ── Deep sysctl tuning — pushed to the limit ───────────────────────────────
  boot.kernel.sysctl = {
    # Memory: minimize writeback latency
    "vm.dirty_ratio" = lib.mkForce 5;
    "vm.dirty_background_ratio" = lib.mkForce 2;
    "vm.dirty_writeback_centisecs" = lib.mkForce 0;

    # Network buffers for NETJACK
    "net.core.rmem_max" = lib.mkForce 16777216;
    "net.core.wmem_max" = lib.mkForce 16777216;
    "net.core.rmem_default" = lib.mkForce 8388608;
    "net.core.wmem_default" = lib.mkForce 8388608;
    "net.ipv4.tcp_rmem" = lib.mkForce "4096 8388608 16777216";
    "net.ipv4.tcp_wmem" = lib.mkForce "4096 8388608 16777216";

    # ── RT scheduling: completely disable throttling ─────────────────────────
    # -1 means RT tasks can run indefinitely without being throttled.
    # The default (1000000 = 1s) throttles RT tasks after 1s of CPU time
    # in a 1s window to prevent system lockup. We disable it entirely
    # because our DSP task IS the system's primary purpose.
    "kernel.sched_rt_runtime_us" = lib.mkForce (-1);

    # ── Timer migration: keep timers on the local CPU ────────────────────────
    # Default (1) allows the kernel to migrate timers across CPUs for
    # load balancing. We disable it (0) to keep timer interrupts on the
    # CPU where the audio callback runs — eliminates cross-CPU timer
    # migration latency.
    "kernel.timer_migration" = lib.mkForce 0;
  };

  # Extra sysctl for hardware-level tuning
  environment.etc."sysctl.d/99-dsp-maxrt.conf".text = ''
    # RTC/HPET — push to 8192 Hz (default 64, audio standard 2048)
    # Higher = more precise timer resolution for audio callbacks
    dev.rtc.max-user-freq = 8192
    dev.hpet.max-user-freq = 8192
  '';

  # ── NTP for sample-accurate timing ─────────────────────────────────────────
  services.timesyncd.enable = true;

  # ── Performance CPU governor — locked to max frequency ────────────────────
  powerManagement.cpuFreqGovernor = lib.mkForce "performance";
}
