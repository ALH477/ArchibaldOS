# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 DeMoD LLC. All rights reserved.
# HydraMesh NixOS Module - P2P networking as a containerized service
#
# THE SECURITY MODEL IS THE NETWORK. DCF carries no encryption, deliberately
# (EAR/ITAR), so confidentiality and access are properties of the tunnel it
# runs in (Punctim Documentation/DCF_SECURITY_EXPOSURE.md; Oligarchy's
# demod-talk module makes the same argument and refuses a wildcard bind).
# Two things here make that easy to get wrong, so they are explicit:
#
#   - Docker-published ports BYPASS networking.firewall: Docker DNATs them in
#     PREROUTING and they never traverse the INPUT chain the NixOS firewall
#     filters. The only control is the host address a port is published on —
#     `bindAddress` / `grpcBindAddress` below. Set `bindAddress` to your
#     WireGuard (or LAN) address; "0.0.0.0" publishes the plaintext mesh on
#     every interface and is warned about.
#   - The gRPC API is a control plane, not the mesh. It is published on
#     loopback unless you say otherwise (it used to be every interface).
#
# `image` should be pinned by digest (`name@sha256:...`): a tag is mutable, so
# `:latest` makes the running code a function of the day you pulled. Unpinned
# references are warned about rather than refused, because no digest has been
# verified for this repository's default yet.
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.hydramesh;

  isLoopback = a: hasPrefix "127." a || a == "::1" || a == "localhost";
  isWildcard = a: a == "0.0.0.0" || a == "::" || a == "";
  isPinned = img: builtins.match ".*@sha256:[0-9a-f]{64}" img != null;

  configFile = pkgs.writeText "hydramesh-config.json" (builtins.toJSON (
    {
      transport = cfg.transport;
      host = cfg.host;
      port = cfg.grpcPort;
      udp-port = cfg.udpPort;
      mode = cfg.mode;
      node-id = cfg.nodeId;
      peers = cfg.peers;
      optimization-level = cfg.optimizationLevel;
    }
    // optionalAttrs (cfg.rttThreshold != null) {
      group-rtt-threshold = cfg.rttThreshold;
    }
    // optionalAttrs (cfg.retryMax != null) {
      retry-max = cfg.retryMax;
    }
    // optionalAttrs (cfg.udpMtu != null) {
      udp-mtu = cfg.udpMtu;
    }
  ));

in {
  options.services.hydramesh = {
    enable = mkEnableOption "HydraMesh P2P networking service";

    image = mkOption {
      type = types.str;
      default = "alh477/hydramesh:latest";
      description = "Docker image for HydraMesh";
      example = "alh477/hydramesh:2.2.0";
    };

    nodeId = mkOption {
      type = types.str;
      default = config.networking.hostName;
      description = "Unique node identifier";
    };

    mode = mkOption {
      type = types.enum [ "p2p" "client" "server" ];
      default = "p2p";
      description = "Network mode";
    };

    transport = mkOption {
      type = types.enum [ "UDP" "TCP" ];
      default = "UDP";
      description = "Primary transport protocol";
    };

    host = mkOption {
      type = types.str;
      default = "0.0.0.0";
      description = ''
        Bind address INSIDE the container (written to config.json). What the
        host exposes is `bindAddress` / `grpcBindAddress`, not this.
      '';
    };

    bindAddress = mkOption {
      type = types.str;
      default = "0.0.0.0";
      example = "10.100.0.5";
      description = ''
        Host address the UDP mesh port is published on. Docker-published ports
        bypass networking.firewall, so this is the access control: set it to
        your WireGuard address. "0.0.0.0" (the default, for compatibility)
        exposes the plaintext DCF wire on every interface and emits a warning.
      '';
    };

    grpcBindAddress = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = ''
        Host address the gRPC API is published on. Loopback by default: it is
        a control plane, and it used to be published on every interface.
      '';
    };

    udpPort = mkOption {
      type = types.port;
      default = 7777;
      description = "UDP transport port for game/audio data";
    };

    grpcPort = mkOption {
      type = types.port;
      default = 50051;
      description = "gRPC API port";
    };

    peers = mkOption {
      type = types.listOf types.str;
      default = [];
      example = [ "192.168.1.100:7777" "192.168.1.101:7777" ];
      description = "List of peer addresses";
    };

    optimizationLevel = mkOption {
      type = types.ints.between 0 3;
      default = 2;
      description = "Optimization level (0-3)";
    };

    rttThreshold = mkOption {
      type = types.nullOr types.int;
      default = null;
      description = "Group RTT threshold in milliseconds";
    };

    retryMax = mkOption {
      type = types.nullOr types.int;
      default = null;
      description = "Maximum retry attempts";
    };

    udpMtu = mkOption {
      type = types.nullOr types.int;
      default = null;
      description = "UDP MTU size";
    };

    logLevel = mkOption {
      type = types.enum [ "debug" "info" "warn" "error" ];
      default = "info";
      description = "Log verbosity";
    };

    memoryLimit = mkOption {
      type = types.str;
      default = "256m";
      description = "Memory limit for HydraMesh container";
    };

    cpuLimit = mkOption {
      type = types.str;
      default = "0.8";
      description = "CPU limit for HydraMesh container (0.8 = 80%)";
    };

    hardened = mkOption {
      type = types.bool;
      default = true;
      description = "Run with hardened security options";
    };
  };

  config = mkIf cfg.enable {
    virtualisation.docker.enable = true;

    environment.etc."hydramesh/config.json".source = configFile;

    systemd.tmpfiles.rules = [
      "d /var/lib/hydramesh 0755 root root -"
      "d /var/log/hydramesh 0755 root root -"
    ];

    virtualisation.oci-containers.backend = "docker";

    virtualisation.oci-containers.containers.hydramesh = {
      image = cfg.image;
      autoStart = true;

      environment = {
        HYDRAMESH_CONFIG = "/etc/hydramesh/config.json";
      } // optionalAttrs (cfg.peers != []) {
        PEERS = concatStringsSep "," cfg.peers;
      };

      volumes = [
        "/etc/hydramesh/config.json:/etc/hydramesh/config.json:ro"
        "/var/lib/hydramesh:/data"
      ];

      ports = [
        "${cfg.bindAddress}:${toString cfg.udpPort}:7777/udp"
        "${cfg.grpcBindAddress}:${toString cfg.grpcPort}:50051/tcp"
      ];

      extraOptions = [
        "--memory=${cfg.memoryLimit}"
        "--cpus=${cfg.cpuLimit}"
      ] 
      ++ optionals cfg.hardened [
        "--read-only"
        "--security-opt=no-new-privileges:true"
        "--cap-drop=ALL"
        "--cap-add=NET_BIND_SERVICE"
      ];
    };

    # Kept for a non-Docker path to these ports; Docker-published ports do
    # not consult it (see the header). gRPC is opened only when it is not
    # loopback-bound.
    networking.firewall = mkIf config.networking.firewall.enable {
      allowedTCPPorts = optional (!isLoopback cfg.grpcBindAddress) cfg.grpcPort;
      allowedUDPPorts = [ cfg.udpPort ];
    };

    warnings =
      optional (isWildcard cfg.bindAddress) ''
        services.hydramesh.bindAddress is "${cfg.bindAddress}": the plaintext DCF
        mesh port ${toString cfg.udpPort}/udp is published on EVERY interface, and
        Docker-published ports bypass networking.firewall. Set it to your
        WireGuard address (see modules/hydramesh.nix and docs/security.md).
      ''
      ++ optional (!isLoopback cfg.grpcBindAddress) ''
        services.hydramesh.grpcBindAddress is "${cfg.grpcBindAddress}": the
        HydraMesh gRPC control API is reachable off-host.
      ''
      ++ optional (!isPinned cfg.image) ''
        services.hydramesh.image "${cfg.image}" is not pinned by digest. A tag
        is mutable; pin it as name@sha256:<64 hex> so the code that runs is the
        code you reviewed.
      '';

    environment.systemPackages = [
      (pkgs.writeShellScriptBin "hydramesh-logs" ''
        docker logs -f hydramesh "$@"
      '')
      (pkgs.writeShellScriptBin "hydramesh-status" ''
        echo "=== HydraMesh Status ==="
        docker run --rm ${cfg.image} status 2>/dev/null || \
          docker ps --filter name=hydramesh --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
        echo ""
        echo "=== Container Info ==="
        docker inspect hydramesh --format '{{.State.Status}} - Up {{.State.StartedAt}}' 2>/dev/null || echo "Not running"
        echo ""
        echo "=== Recent Logs ==="
        docker logs --tail 20 hydramesh 2>/dev/null || echo "No logs available"
      '')
      (pkgs.writeShellScriptBin "hydramesh-version" ''
        docker run --rm ${cfg.image} version
      '')
      (pkgs.writeShellScriptBin "hydramesh-restart" ''
        systemctl restart docker-hydramesh
      '')
      (pkgs.writeShellScriptBin "hydramesh-pull" ''
        docker pull ${cfg.image}
        systemctl restart docker-hydramesh
      '')
    ];
  };
}
