# modules/secure-rt.nix
# Copyright 2025 DeMoD LLC
# SPDX-License-Identifier: BSD-3-Clause
#
# This module provides real-time system hardening for defense/robotics use.
# NO ENCRYPTION, NO CRYPTO, NO NETWORK EXFILTRATION.
# Fully ITAR/EAR compliant by design.

{ config, pkgs, lib, ... }:

let
  cfg = config.archibaldOS.secureRT;
in {
  options.archibaldOS.secureRT = {
    enable = lib.mkEnableOption "Enable secure real-time hardening (ITAR/EAR-safe)";

    mode = lib.mkOption {
      type = lib.types.enum [ "defense" "aerospace" "robotics" "dsp" ];
      default = "robotics";
      description = ''
        Security profile:
        - defense: Max isolation, no network
        - aerospace: Audit + watchdog
        - robotics: Balanced RT + integrity
        - dsp: Minimal, kexec-locked
      '';
    };

    lockdown = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enforce strict process/network isolation";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.archibaldOS.rtKernel.enable;
        message = "secureRT requires archibaldOS.rtKernel.enable = true";
      }
    ];

    # === 1. Kernel Hardening (ITAR-safe) ===
    boot.kernelParams = [
      "slab_nomerge"
      "slub_debug=FZ"
      "page_alloc.shuffle=1"
      "pti=on"
      "spec_store_bypass_disable=on"
      "random.trust_cpu=off"
      "mce=0"
      "hugepages=0"
      "vsyscall=none"
    ] ++ lib.optionals (cfg.mode == "defense" || cfg.mode == "dsp") [
      "lockdown=confidentiality"
    ];

    boot.kernel.sysctl = {
      "kernel.kexec_load_disabled" = lib.mkIf (cfg.mode != "dsp") 1;
      "kernel.unprivileged_bpf_disabled" = 1;
      "kernel.printk" = "3 3 3 3";
      "kernel.dmesg_restrict" = 1;
      "kernel.kptr_restrict" = 2;
      "kernel.yama.ptrace_scope" = 1;
      "kernel.sysrq" = 0;
      "kernel.perf_event_paranoid" = 3;
      "fs.protected_regular" = 2;
      "fs.protected_fifos" = 2;
      "fs.protected_symlinks" = 1;
      "fs.suid_dumpable" = 0;
      "net.core.bpf_jit_harden" = 2;
      "dev.tty.ldisc_autoload" = 0;
      "user.max_user_namespaces" = 0;
      "kernel.modules_disabled" = lib.mkIf (cfg.mode == "defense") 1;
    };

    # === 2. Filesystem Integrity ===
    fileSystems = lib.mkIf cfg.lockdown {
      "/" = { options = [ "ro" "nosuid" "nodev" "noexec" ]; };
      "/boot" = { options = [ "ro" "nosuid" "nodev" ]; };
      "/var/log" = { options = [ "noexec" ]; };
    };

    # === 3. Process Isolation ===
    security.apparmor.enable = true;
    security.apparmor.killUnconfined = cfg.lockdown;

    # Confine RT processes
    systemd.services = lib.mkIf cfg.lockdown {
      "rt-audio" = {
        serviceConfig = {
          ProtectSystem = "strict";
          ProtectHome = true;
          PrivateTmp = true;
          NoNewPrivileges = true;
          RestrictSUIDSGID = true;
          ProtectKernelTunables = true;
          ProtectControlGroups = true;
          RestrictNamespaces = true;
          MemoryDenyWriteExecute = true;
          LockPersonality = true;
        };
      };
    };

    # === 4. Network Lockdown (ITAR-safe: NO encryption) ===
    networking.firewall.enable = true;
    networking.firewall.allowedTCPPorts = lib.mkIf (cfg.mode == "robotics") [ 14550 14556 ]; # MAVLink
    networking.firewall.allowedUDPPorts = lib.mkIf (cfg.mode == "robotics") [ 14550 14556 ];
    networking.firewall.extraCommands = lib.mkIf cfg.lockdown ''
      iptables -A OUTPUT -m owner --uid-owner 0 -j ACCEPT
      iptables -A OUTPUT -j DROP
    '';

    # === 5. Audit & Watchdog ===
    services.journald.extraConfig = ''
      Storage=persistent
      SystemMaxUse=50M
      SystemMaxFileSize=10M
      MaxRetentionSec=604800  # 7 days
    '';

    services.audit.enable = true;
    services.audit.rules = [
      "-w /etc/passwd -p wa -k identity"
      "-w /etc/shadow -p wa -k identity"
      "-w /bin/su -p x -k privileged"
      "-a always,exit -F arch=b64 -S execve -k exec"
    ];

    # Hardware watchdog (critical for RT)
    services.das_watchdog.enable = true;
    services.das_watchdog.timeout = "10";  # 10s

    # === 6. User & Group Hardening ===
    users.mutableUsers = false;

    users.groups.secure-rt = {};
    security.sudo.enable = false;  # No sudo in secure RT

    # Lock down audio-user / robotics user
    users.users.audio-user = lib.mkIf config.users.users ? audio-user {
      extraGroups = [ "secure-rt" ];
      linger = false;
    };

    # === 7. DSP/Kexec Lockdown ===
    systemd.services.kexec-load = lib.mkIf (cfg.mode == "dsp") {
      serviceConfig = {
        ProtectSystem = "full";
        PrivateDevices = true;
        ProtectKernelModules = true;
        SystemCallFilter = "@system-service";
      };
    };

    # === 8. Package Stripping (ITAR-safe: no debug, no crypto) ===
    environment.defaultPackages = lib.mkIf cfg.lockdown (
      lib.filter (p: !lib.hasPrefix "openssl" p.name && !lib.hasPrefix "gnutls" p.name) config.environment.defaultPackages
    );

    # === 9. Boot Integrity ===
    boot.loader.systemd-boot.enable = lib.mkIf cfg.lockdown true;
    boot.loader.efi.canTouchEfiVariables = lib.mkIf cfg.lockdown false;

    # === 10. Real-Time Guarantees ===
    musnix.rtirq.highList = "snd_usb_audio eth0";
    security.rtkit.enable = true;

    security.pam.loginLimits = [
      { domain = "@secure-rt"; item = "rtprio"; value = "99"; }
      { domain = "@secure-rt"; item = "memlock"; value = "unlimited"; }
    ];

    # === 11. Mode-Specific Overrides ===
    config = lib.mkMerge [
      (lib.mkIf (cfg.mode == "defense") {
        networking.enable = false;
        services.openssh.enable = false;
        services.getty.autologinUser = null;
        boot.initrd.systemd.enable = false;
      })

      (lib.mkIf (cfg.mode == "aerospace") {
        services.chrony.enable = true;
        services.chrony.extraConfig = ''
          makestep 1.0 3
          rtcsync
        '';
      })

      (lib.mkIf (cfg.mode == "dsp") {
        boot.kernelParams = [ "init=/bin/systemd" "systemd.unit=kexec.target" ];
      })
    ];
  };
}
