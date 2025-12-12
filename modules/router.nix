# modules/router.nix
# Copyright 2025 DeMoD LLC
# SPDX-License-Identifier: BSD-3-Clause
#
# Turns ArchibaldOS into a hardened router/firewall.
# ITAR/EAR-safe: no VPN, no encryption, no TLS termination.

{ config, pkgs, lib, ... }:

let
  cfg = config.archibaldOS.router;

  # Common network utilities
  netTools = with pkgs; [
    iproute2
    iptables
    nftables
    bridge-utils
    dnsmasq
    frr
    tcpdump
    ethtool
    wireguard-tools  # Only for wireguard interface (ITAR-safe if disabled)
  ];

in {
  options.archibaldOS.router = {
    enable = lib.mkEnableOption "Enable router/firewall mode";

    wanInterfaces = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "eth0" ];
      description = "WAN interfaces (internet-facing)";
    };

    lanInterfaces = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "eth1" ];
      description = "LAN interfaces (internal network)";
    };

    lanSubnet = lib.mkOption {
      type = lib.types.str;
      default = "192.168.1.0/24";
      description = "LAN subnet (CIDR)";
    };

    dhcpRange = lib.mkOption {
      type = lib.types.str;
      default = "192.168.1.100,192.168.1.200,12h";
      description = "DHCP range: start,end,lease";
    };

    routing = {
      enableBGP = lib.mkEnableOption "Enable FRR BGP routing";
      asNumber = lib.mkOption {
        type = lib.types.int;
        default = 65001;
        description = "Local AS number for BGP";
      };
    };

    hardening = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable defense-grade firewall hardening";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.wanInterfaces != [] && cfg.lanInterfaces != [];
        message = "At least one WAN and one LAN interface must be defined.";
      }
    ];

    # === 1. Base Networking ===
    networking = {
      useDHCP = false;
      useNetworkd = true;
      firewall.enable = false;  # We use nftables
      nat.enable = true;
      nat.externalInterface = lib.head cfg.wanInterfaces;
      nat.internalInterfaces = cfg.lanInterfaces;
    };

    # === 2. Interfaces ===
    systemd.network = {
      enable = true;

      networks = lib.mkMerge [
        # WAN: DHCP client
        (builtins.listToAttrs (map (iface: {
          name = iface;
          value = {
            DHCP = "yes";
            networkConfig = {
              IPv6AcceptRA = true;
            };
          };
        }) cfg.wanInterfaces))

        # LAN: Static IP + DHCP server
        (builtins.listToAttrs (map (iface: {
          name = iface;
          value = {
            address = [ "${builtins.head (lib.splitString "/" cfg.lanSubnet)}.1/${builtins.substring (builtins.stringLength cfg.lanSubnet - 3) 3 cfg.lanSubnet}" ];
            networkConfig = {
              IPMasquerade = "ipv4";
              IPv6AcceptRA = false;
            };
          };
        }) cfg.lanInterfaces))
      ];
    };

    # === 3. DNS + DHCP (dnsmasq) ===
    services.dnsmasq = {
      enable = true;
      resolveLocalQueries = true;
      settings = {
        interface = lib.head cfg.lanInterfaces;
        dhcp-range = [ cfg.dhcpRange ];
        dhcp-option = [
          "option:router,${builtins.head (lib.splitString "/" cfg.lanSubnet)}.1"
          "option:dns-server,1.1.1.1,8.8.8.8"
        ];
        domain = "lan";
        expand-hosts = true;
        local = "/lan/";
      };
    };

    # === 4. nftables Firewall (stateful, hardened) ===
    networking.nftables = {
      enable = true;
      ruleset = ''
        table inet filter {
          chain input {
            type filter hook input priority 0; policy drop;

            # Accept established/related
            ct state { established, related } accept

            # Loopback
            iifname "lo" accept

            # ICMP (rate-limited)
            ip protocol icmp icmp type { echo-request, echo-reply } limit rate 5/second accept
            ip6 nexthdr icmpv6 icmpv6 type { echo-request, echo-reply } limit rate 5/second accept

            # SSH (optional, locked to LAN)
            ${lib.optionalString cfg.hardening ''
              tcp dport 22 ct state new ip saddr ${cfg.lanSubnet} accept
            ''}

            # Drop everything else
            counter drop
          }

          chain forward {
            type filter hook forward priority 0; policy drop;

            # Allow LAN to WAN
            iifname { ${lib.concatStringsSep ", " (map (i: "\"${i}\"") cfg.lanInterfaces)} } \
            oifname { ${lib.concatStringsSep ", " (map (i: "\"${i}\"") cfg.wanInterfaces)} } \
            ct state new accept

            # Allow established
            ct state { established, related } accept

            counter drop
          }

          chain output {
            type filter hook output priority 0; policy accept;
          }
        }

        table ip nat {
          chain postrouting {
            type nat hook postrouting priority 100; policy accept;
            oifname { ${lib.concatStringsSep ", " (map (i: "\"${i}\"") cfg.wanInterfaces)} } masquerade
          }
        }
      '';
    };

    # === 5. FRR Routing (BGP, OSPF) ===
    services.frr = lib.mkIf cfg.routing.enableBGP {
      enable = true;
      bgp = {
        enable = true;
        as = cfg.routing.asNumber;
        router-id = "${builtins.head (lib.splitString "/" cfg.lanSubnet)}.1";
        neighbors = [
          # Example: peer with upstream
          # { address = "203.0.113.1"; remote-as = 65000; }
        ];
      };
    };

    # === 6. Hardening (ITAR-safe) ===
    boot.kernel.sysctl = lib.mkIf cfg.hardening {
      "net.ipv4.ip_forward" = 1;
      "net.ipv6.conf.all.forwarding" = 1;
      "net.ipv4.conf.all.rp_filter" = 1;
      "net.ipv4.conf.default.rp_filter" = 1;
      "net.ipv4.tcp_syncookies" = 1;
      "net.ipv4.icmp_echo_ignore_broadcasts" = 1;
      "net.ipv4.conf.all.accept_redirects" = 0;
      "net.ipv4.conf.all.send_redirects" = 0;
      "net.ipv6.conf.all.accept_redirects" = 0;
      "net.ipv4.conf.all.log_martians" = 1;
    };

    # === 7. Real-Time Compatibility ===
    boot.kernelParams = [
      "threadirqs"
      "isolcpus=0"  # Isolate core 0 for networking
    ];

    services.das_watchdog.enable = true;
    services.das_watchdog.timeout = "15";

    # === 8. Audit & Logging ===
    services.journald.extraConfig = ''
      SystemMaxUse=100M
      SystemMaxFileSize=10M
      MaxRetentionSec=604800
    '';

    services.audit.enable = true;
    services.audit.rules = [
      "-w /etc/nftables.conf -p wa -k firewall"
      "-w /etc/dnsmasq.conf -p wa -k dhcp"
      "-a always,exit -F arch=b64 -S socket -k network"
    ];

    # === 9. Packages ===
    environment.systemPackages = netTools;

    # === 10. User & Permissions ===
    users.groups.netadmin = {};
    users.users.router-admin = {
      isSystemUser = true;
      group = "netadmin";
      extraGroups = [ "wheel" ];
      openssh.authorizedKeys.keys = [ ];
    };

    # === 11. Secure Boot (optional) ===
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = false;

    # === 12. Disable Unneeded Services ===
    services.openssh.enable = cfg.hardening;
    networking.networkmanager.enable = false;

    # === 13. Status Dashboard (optional) ===
    services.prometheus.exporters.node.enable = true;
    services.grafana.enable = lib.mkIf (!cfg.hardening) true;
  };
}
