# modules/network-infra.nix

# Copyright 2025 DeMoD LLC

# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



{ config, pkgs, lib, ... }:

let
  cfg = config.archibaldOS.networkInfra;
in {
  options.archibaldOS.networkInfra = {
    enable = lib.mkEnableOption "Enable network infrastructure mode (DHCP server + switch)";

    dhcp = {
      interfaces = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "br0" ];
        description = "Interfaces for DHCP server to listen on";
      };

      subnet = lib.mkOption {
        type = lib.types.str;
        default = "192.168.100.0";
        description = "DHCP subnet address";
      };

      netmask = lib.mkOption {
        type = lib.types.str;
        default = "255.255.255.0";
        description = "DHCP subnet mask";
      };

      rangeStart = lib.mkOption {
        type = lib.types.str;
        default = "192.168.100.100";
        description = "DHCP pool start address";
      };

      rangeEnd = lib.mkOption {
        type = lib.types.str;
        default = "192.168.100.200";
        description = "DHCP pool end address";
      };

      gatewayIP = lib.mkOption {
        type = lib.types.str;
        default = "192.168.100.1";
        description = "Gateway/router IP for DHCP clients";
      };

      dns = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "8.8.8.8" "8.8.4.4" ];
        description = "DNS servers to advertise";
      };

      domainName = lib.mkOption {
        type = lib.types.str;
        default = "archibald.local";
        description = "Domain name for DHCP clients";
      };

      staticLeases = lib.mkOption {
        type = lib.types.listOf (lib.types.attrsOf lib.types.str);
        default = [];
        description = "Static DHCP lease assignments";
        example = [
          { hostname = "audio-station-1"; mac = "00:11:22:33:44:55"; ip = "192.168.100.50"; }
        ];
      };
    };

    bridge = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "br0";
        description = "Bridge interface name";
      };

      interfaces = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [];
        description = "Physical interfaces to add to bridge (e.g., eth1, eth2)";
      };

      ip = lib.mkOption {
        type = lib.types.str;
        default = "192.168.100.1";
        description = "Bridge IP address";
      };

      prefixLength = lib.mkOption {
        type = lib.types.int;
        default = 24;
        description = "Bridge network prefix length";
      };
    };

    nat = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable NAT for internet sharing";
      };

      externalInterface = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "External WAN interface for NAT";
      };
    };

    vfio = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable VFIO passthrough support";
      };

      deviceIDs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [];
        description = "PCI device IDs to bind to VFIO (e.g., 8086:15b8)";
        example = [ "8086:15b8" "8086:15b9" ];
      };
    };

    isolation = {
      cpuCores = lib.mkOption {
        type = lib.types.listOf lib.types.int;
        default = [ 4 5 6 7 ];
        description = "CPU cores to isolate for VM use";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    # Boot configuration with RT kernel and IOMMU
    boot = {
      kernelParams = [
        # Enable IOMMU
        "intel_iommu=on"
        "iommu=pt"
        
        # CPU isolation
        "isolcpus=${lib.concatMapStringsSep "," toString cfg.isolation.cpuCores}"
        "nohz_full=${lib.concatMapStringsSep "," toString cfg.isolation.cpuCores}"
        "rcu_nocbs=${lib.concatMapStringsSep "," toString cfg.isolation.cpuCores}"
        
        # RT kernel optimizations
        "threadirqs"
        "preempt=full"
      ];

      kernelModules = [ "vfio-pci" "vhost-net" "tun" "bridge" "br_netfilter" ];
      
      # Load VFIO early
      initrd.kernelModules = lib.mkIf cfg.vfio.enable 
        [ "vfio_pci" "vfio" "vfio_iommu_type1" ];
      
      # Bind specific PCI devices to VFIO
      extraModprobeConfig = lib.mkIf (cfg.vfio.enable && cfg.vfio.deviceIDs != []) ''
        options vfio-pci ids=${lib.concatStringsSep "," cfg.vfio.deviceIDs}
      '';
    };

    # Virtualization support
    virtualisation = {
      libvirtd = {
        enable = true;
        qemu = {
          package = pkgs.qemu_kvm;
          ovmf.enable = true;
          runAsRoot = true;
          swtpm.enable = true;
          verbatimConfig = ''
            namespaces = []
            cgroup_device_acl = [
              "/dev/null", "/dev/full", "/dev/zero",
              "/dev/random", "/dev/urandom",
              "/dev/ptmx", "/dev/kvm", "/dev/kqemu",
              "/dev/rtc","/dev/hpet", "/dev/vfio/vfio"
            ]
          '';
        };
      };
    };

    # DHCP Server Configuration
    services.dhcpd4 = {
      enable = true;
      interfaces = cfg.dhcp.interfaces;
      extraConfig = ''
        option domain-name "${cfg.dhcp.domainName}";
        option domain-name-servers ${lib.concatStringsSep ", " cfg.dhcp.dns};
        option subnet-mask ${cfg.dhcp.netmask};
        option routers ${cfg.dhcp.gatewayIP};
        
        default-lease-time 600;
        max-lease-time 7200;
        
        # DHCP pool configuration
        subnet ${cfg.dhcp.subnet} netmask ${cfg.dhcp.netmask} {
          range ${cfg.dhcp.rangeStart} ${cfg.dhcp.rangeEnd};
        }
        
        # Static assignments
        ${lib.concatMapStringsSep "\n" (lease: ''
          host ${lease.hostname} {
            hardware ethernet ${lease.mac};
            fixed-address ${lease.ip};
          }
        '') cfg.dhcp.staticLeases}
      '';
    };

    # Network bridge configuration
    networking = {
      hostName = "archibaldos-netinfra";
      useDHCP = false;
      
      bridges = {
        ${cfg.bridge.name} = {
          interfaces = cfg.bridge.interfaces;
        };
      };
      
      interfaces.${cfg.bridge.name} = {
        ipv4.addresses = [{
          address = cfg.bridge.ip;
          prefixLength = cfg.bridge.prefixLength;
        }];
      };
      
      # Firewall configuration
      firewall = {
        enable = true;
        allowedUDPPorts = [ 67 68 ]; # DHCP ports
        allowedTCPPorts = [ 22 ]; # SSH
        trustedInterfaces = [ cfg.bridge.name ];
        checkReversePath = false; # Required for NAT
      };
      
      # NAT configuration
      nat = lib.mkIf cfg.nat.enable {
        enable = true;
        internalInterfaces = [ cfg.bridge.name ];
        externalInterface = lib.mkIf (cfg.nat.externalInterface != null) cfg.nat.externalInterface;
      };
    };

    # Network utilities
    environment.systemPackages = with pkgs; [
      pciutils
      usbutils
      bridge-utils
      tcpdump
      iftop
      ethtool
      iproute2
      iperf3
      tmux
      vim
      virt-manager
      virtviewer
    ];

    # Enable SSH for remote management
    services.openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
      };
    };

    # RT thread priorities for network interrupts
    musnix.rtirq = lib.mkIf config.musnix.enable {
      enable = true;
      nameList = "vfio rtc snd usb i8042 eth";
    };

    # Monitoring script
    environment.etc."network-infra-status.sh" = {
      text = ''
        #!/usr/bin/env bash
        echo "=== ArchibaldOS Network Infrastructure Status ==="
        echo ""
        echo "Bridge Status:"
        ${pkgs.iproute2}/bin/ip -br link show ${cfg.bridge.name}
        ${pkgs.iproute2}/bin/bridge link show
        echo ""
        echo "DHCP Leases:"
        ${pkgs.gawk}/bin/awk '/lease/ {print $2, $3, $4}' /var/lib/dhcp/dhcpd.leases 2>/dev/null || echo "No leases file"
        echo ""
        echo "VFIO Devices:"
        ${pkgs.pciutils}/bin/lspci -k | ${pkgs.gnugrep}/bin/grep -A 3 vfio || echo "No VFIO devices"
        echo ""
        echo "Isolated CPUs:"
        cat /sys/devices/system/cpu/isolated 2>/dev/null || echo "None"
        echo ""
        echo "RT Kernel:"
        uname -r
        cat /sys/kernel/realtime 2>/dev/null || echo "Not RT"
      '';
      mode = "0755";
    };

    # System monitoring
    systemd.services.network-infra-monitor = {
      description = "Network Infrastructure Health Monitor";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.writeShellScript "network-monitor" ''
          #!/usr/bin/env bash
          while true; do
            # Check bridge status
            if ! ${pkgs.iproute2}/bin/ip link show ${cfg.bridge.name} | grep -q "state UP"; then
              ${pkgs.systemd}/bin/systemd-cat -t network-monitor -p warning echo "Bridge ${cfg.bridge.name} is DOWN"
            fi
            
            # Check DHCP service
            if ! ${pkgs.systemd}/bin/systemctl is-active --quiet dhcpd4.service; then
              ${pkgs.systemd}/bin/systemd-cat -t network-monitor -p err echo "DHCP service is not running"
            fi
            
            sleep 60
          done
        ''}";
        Restart = "always";
      };
    };
  };
}
