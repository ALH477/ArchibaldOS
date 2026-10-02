# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC. All rights reserved.
# ============================================================================
# ArchibaldOS companion — a music computer on older hardware, commanded by
# Oligarchy.
# ============================================================================
# Sized for a 4 GB, 2-core machine (the first one is an older Microsoft
# Surface Pro). Headless: the machine's job is to be an audio device that the
# Oligarchy host drives, not a desktop.
#
#   - Memory: zram swap and earlyoom, which protects jackd/demod-rt/sshd. No
#     PipeWire (it would hold the card before jackd), no Plasma.
#   - Kernel: CachyOS (preempt=full, threadirqs), the build Chaotic's binary
#     cache carries for this nixpkgs revision, so normally nothing compiles on
#     a 4 GB box. [UNTESTED] that the cache holds this exact build: it could
#     not be reached from where this was written. companion-surface swaps in
#     the linux-surface kernel, which is never cached: build it on Oligarchy
#     and push it.
#   - RT: no isolcpus by default. `isolcpus=1-3`, as the desktop profiles have
#     it, removes 3 of 4 threads from the scheduler while nothing pins JACK
#     onto them, so everything ends up on CPU 0. Here JACK floats unless
#     `audio.cpu` is set; with `audio.isolate` that CPU is isolated AND jackd is
#     pinned to it (rt-exec --cpu), the only combination that is coherent. No
#     C-state caps either: on a passively cooled tablet they buy heat, then
#     thermal throttling, which costs more latency than C-state exits do.
#   - Command: Oligarchy's dsp-ctl reaches this machine over SSH (it runs
#     `sudo systemctl start|stop|restart <unit>` and pipes JSON to the control
#     socket through socat) or over TCP to the control bridge. Both work as
#     they are: the unit names below are the ones dsp-ctl uses, the sudo rule
#     allows exactly those commands, and the bridge accepts only the
#     commander's address. Deploys come from Oligarchy (nixos-rebuild
#     --target-host root@…), which needs root to be key-only.
#   - Enrolment: until a commander SSH key is set, sshd takes the password the
#     user chose at install (and the build warns); once `commander.sshKeys` is
#     set, password logins end and root is key-only. That is the bootstrap
#     `oligarchy-companion enroll` relies on.
#
# The plaintext rule holds: DCF and the control protocol carry no encryption,
# so the commander link is WireGuard (`commander.wireguard`).
# ============================================================================
{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption mkEnableOption mkIf mkDefault mkForce types optionals optional;
  cfg = config.archibald.companion;
  cmd = cfg.commander;
  wg = cmd.wireguard;
  enrolled = cmd.sshKeys != [ ];
  rt-exec = pkgs.callPackage ./rt-exec.nix { };
  jackCpu = if cfg.audio.cpu == null then "any" else toString cfg.audio.cpu;
  isoCpu = toString cfg.audio.cpu;

  # What dsp-ctl's SSH transport runs (Oligarchy modules/dsp-ctl
  # src/transport.rs: `sudo systemctl {start|stop|restart} <unit>`), for the
  # units that exist here. dsp-netjack-bridge is not defined on a companion.
  dspCtlUnits = [ "archibaldos-dsp.service" "demod-rt.service" ];
  systemctl = "/run/current-system/sw/bin/systemctl";
in
{
  imports = [ ./demod-rt.nix ./dsp-control-bridge.nix ];

  options.archibald.companion = {
    enable = mkEnableOption "the ArchibaldOS companion role (headless audio device commanded by Oligarchy)";

    user = mkOption {
      type = types.str;
      example = "asher";
      description = ''
        The account JACK (and demod-rt, when enabled) runs as, and the one the
        commander logs in as: `dsp-ctl --transport ssh --user <this>`. JACK
        keeps one server per user, so these must be the same account. The
        installer sets it to the user created at install.
      '';
    };

    audio = {
      device = mkOption {
        type = types.str;
        default = "hw:0";
        example = "hw:1";
        description = ''
          ALSA device jackd opens. With a USB interface on a machine that also
          has onboard audio it is usually `hw:1`; `aplay -l` lists them.
        '';
      };
      rate = mkOption { type = types.ints.positive; default = 48000; description = "Sample rate."; };
      period = mkOption {
        type = types.ints.positive;
        default = 128;
        description = ''
          Frames per period. 128 at 48 kHz is 2.7 ms: a sane start on a
          2015-era dual core. Measure (jack_iodelay, xruns) before lowering.
        '';
      };
      periods = mkOption { type = types.ints.between 2 4; default = 2; description = "Periods per buffer (3 for some USB interfaces)."; };
      cpu = mkOption {
        type = types.nullOr types.ints.unsigned;
        default = null;
        description = ''
          Pin jackd to this CPU. null (the default) lets it float, which on two
          cores is usually better than pinning.
        '';
      };
      isolate = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Also isolate `cpu` from the scheduler (isolcpus, nohz_full,
          rcu_nocbs). Only coherent together with `cpu`: isolating a CPU that
          nothing is pinned to just takes it away from everything.
        '';
      };
    };

    commander = {
      address = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "10.77.0.1";
        description = ''
          The commanding Oligarchy host's address as this machine sees it —
          its WireGuard address when `wireguard.enable`. The control bridge
          accepts this address and nothing else; null leaves the bridge off.
        '';
      };
      sshKeys = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = ''
          SSH public keys of the commander. Authorised for root (deploys) and
          for `user` (dsp-ctl). Setting any ends password logins.
        '';
      };
      wireguard = {
        enable = mkEnableOption "the WireGuard link to the commander";
        interface = mkOption { type = types.str; default = "wg-oligarchy"; description = "WireGuard interface name."; };
        address = mkOption { type = types.str; default = "10.77.0.2/24"; description = "This machine's address on the link (CIDR)."; };
        peerPublicKey = mkOption { type = types.nullOr types.str; default = null; description = "The commander's WireGuard public key."; };
        endpoint = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "192.168.1.10:51877";
          description = "Where to reach the commander (host:port). This side dials; the commander only listens.";
        };
        privateKeyFile = mkOption {
          type = types.str;
          default = "/var/lib/wireguard/oligarchy.key";
          description = ''
            This machine's private key. Generated here on first start and never
            leaves the machine; `oligarchy-companion enroll` reads the public
            half back over SSH.
          '';
        };
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.audio.isolate -> cfg.audio.cpu != null;
        message = "archibald.companion.audio.isolate needs audio.cpu: isolating a CPU nothing is pinned to only takes it away from everything.";
      }
      {
        assertion = wg.enable -> (wg.peerPublicKey != null && wg.endpoint != null && cmd.address != null);
        message = "archibald.companion.commander.wireguard needs peerPublicKey, endpoint and commander.address.";
      }
    ];
    warnings = optional (!enrolled) ''
      archibald.companion: no commander SSH key is set, so sshd accepts the
      user's password and root cannot log in. This is the bootstrap state: run
      `oligarchy-companion enroll <name> <user>@<address>` on Oligarchy, which
      sets commander.sshKeys and ends password logins.
    '';

    # ── Kernel ────────────────────────────────────────────────────────────
    boot.kernelPackages = mkDefault pkgs.linuxPackages_cachyos;
    boot.kernelParams = [ "threadirqs" "preempt=full" ]
      ++ optionals cfg.audio.isolate [ "isolcpus=managed_irq,domain,${isoCpu}" "nohz_full=${isoCpu}" "rcu_nocbs=${isoCpu}" ];
    powerManagement.cpuFreqGovernor = mkDefault "performance";
    boot.supportedFilesystems.zfs = mkForce false;

    # ── Memory: 4 GB ──────────────────────────────────────────────────────
    zramSwap = {
      enable = true;
      algorithm = "zstd";
      memoryPercent = 50;
    };
    boot.kernel.sysctl = {
      # zram is RAM: swapping to it early is cheap, reading ahead from it is
      # pointless.
      "vm.swappiness" = 100;
      "vm.page-cluster" = 0;
    };
    services.earlyoom = {
      enable = true;
      freeMemThreshold = 5;
      freeSwapThreshold = 10;
      extraArgs = [ "--avoid" "^(jackd|demod-rt|sshd|systemd|systemd-journal)$" ];
    };

    # ── Audio: one JACK server, as `user`, on the card ─────────────────────
    services.pipewire.enable = mkForce false;
    services.pulseaudio.enable = false;
    security.rtkit.enable = true;
    security.pam.loginLimits = [
      { domain = "@audio"; type = "-"; item = "rtprio"; value = "95"; }
      { domain = "@audio"; type = "-"; item = "memlock"; value = "unlimited"; }
    ];
    users.groups.audio = { };
    users.groups.jackaudio = { };
    users.groups.realtime = { };
    users.users.${cfg.user} = {
      extraGroups = [ "audio" "jackaudio" "realtime" ];
      # The commander's keys: dsp-ctl logs in as this account.
      openssh.authorizedKeys.keys = cmd.sshKeys;
    };

    systemd.services.jack2-alsa = {
      description = "JACK2 on ${cfg.audio.device} (companion)";
      after = [ "sound.target" ];
      partOf = [ "archibaldos-dsp.service" ];
      # Keep retrying while the interface is unplugged: plugging it in brings
      # JACK up without anyone having to log in.
      startLimitIntervalSec = 0;
      environment.JACK_NO_AUDIO_RESERVATION = "1"; # no session D-Bus for device reservation
      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = "audio";
        Restart = "always";
        RestartSec = 5;
        LimitRTPRIO = 95;
        LimitMEMLOCK = "infinity";
        ExecStart = "${rt-exec}/bin/rt-exec --cpu ${jackCpu} --prio 90 -- "
          + "${pkgs.jack2}/bin/jackd -R -P 85 -d alsa -d ${cfg.audio.device} "
          + "-r ${toString cfg.audio.rate} -p ${toString cfg.audio.period} -n ${toString cfg.audio.periods}";
      };
    };

    # What dsp-ctl's `vm start|stop|restart` drives on a companion: the DSP
    # stack. Starting it starts JACK; stopping it stops JACK (partOf above).
    systemd.services.archibaldos-dsp = {
      description = "ArchibaldOS companion DSP stack";
      wantedBy = [ "multi-user.target" ];
      requires = [ "jack2-alsa.service" ];
      after = [ "jack2-alsa.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.coreutils}/bin/true";
      };
    };

    # The engine, when someone enables it, runs as the same user as JACK.
    services.demod-rt.user = mkDefault cfg.user;
    services.demod-rt.rtCore = mkDefault (if cfg.audio.cpu == null then 0 else cfg.audio.cpu);

    # ── Commander ─────────────────────────────────────────────────────────
    archibald.dsp.control = {
      enable = cmd.address != null;
      user = cfg.user;
      group = "audio";
      allowFrom = mkIf (cmd.address != null) "${cmd.address}/32";
    };
    networking.firewall.enable = true;
    networking.firewall.interfaces = mkIf (wg.enable && cmd.address != null) {
      ${wg.interface}.allowedTCPPorts = [ config.archibald.dsp.control.port ];
    };
    # Without WireGuard the commander comes over the LAN; socat's range= still
    # admits its address only.
    networking.firewall.allowedTCPPorts = mkIf (!wg.enable && cmd.address != null)
      [ config.archibald.dsp.control.port ];

    networking.wireguard.interfaces = mkIf wg.enable {
      ${wg.interface} = {
        ips = [ wg.address ];
        privateKeyFile = wg.privateKeyFile;
        generatePrivateKeyFile = true;
        peers = [{
          publicKey = wg.peerPublicKey;
          endpoint = wg.endpoint;
          allowedIPs = [ "${cmd.address}/32" ];
          persistentKeepalive = 25;
        }];
      };
    };

    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = !enrolled;
        KbdInteractiveAuthentication = false;
        PermitRootLogin = if enrolled then "prohibit-password" else "no";
      };
    };
    users.users.root.openssh.authorizedKeys.keys = cmd.sshKeys; # deploys

    # dsp-ctl's SSH transport, and nothing broader.
    security.sudo.extraRules = [{
      users = [ cfg.user ];
      commands = lib.concatMap
        (unit: map (verb: { command = "${systemctl} ${verb} ${unit}"; options = [ "NOPASSWD" ]; })
          [ "start" "stop" "restart" ])
        dspCtlUnits;
    }];

    # ── Light ─────────────────────────────────────────────────────────────
    services.xserver.enable = false;
    documentation.enable = false;
    documentation.nixos.enable = false;
    programs.command-not-found.enable = false;
    networking.networkmanager.enable = mkDefault true; # nmtui for Wi-Fi
    boot.loader.systemd-boot.configurationLimit = mkDefault 5;
    nix.settings.auto-optimise-store = true;
    nix.gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 14d";
    };

    environment.systemPackages = with pkgs; [
      alsa-utils
      jack2
      jack-example-tools # jack_iodelay: measure the round trip before trusting it
      socat              # dsp-ctl's SSH transport pipes JSON through it
      rt-exec
      usbutils
      wireguard-tools
      htop
      vim
      git
    ];
  };
}
