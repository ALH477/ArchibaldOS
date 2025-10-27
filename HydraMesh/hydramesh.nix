{ config, pkgs, lib, ... }:

let
  cfg = config.services.hydramesh;
  sbclWithPkgs = pkgs.sbcl.withPackages (ps: with ps; [
    cffi cl-ppcre cl-json cl-csv usocket bordeaux-threads log4cl trivial-backtrace cl-store hunchensocket fiveam cl-dot cserial-port
    cl-lorawan cl-lsquic cl-can cl-sctp cl-zigbee
  ]);
  streamdb = pkgs.rustPlatform.buildRustPackage rec {
    pname = "streamdb";
    version = "0.1.0";
    src = ./HydraMesh/streamdb;
    cargoSha256 = "sha256-placeholder-compute-with-nix-prefetch";  # Replace with nix-prefetch-url --unpack src
    meta = with lib; {
      description = "StreamDB for HydraMesh";
      license = licenses.lgpl3;
    };
    buildPhase = "cargo build --release --lib";
    installPhase = ''
      mkdir -p $out/lib
      cp target/release/libstreamdb.so $out/lib/
    '';
  };
  toggleScript = pkgs.writeShellScriptBin "hydramesh-toggle" ''
    #!/usr/bin/env bash
    if systemctl is-active --quiet hydramesh; then
      systemctl stop hydramesh
      hyprctl notify -1 4000 "rgb(ff3333)" "HydraMesh" "Service stopped"
      echo "OFF" > /var/lib/hydramesh/hydramesh-status
    else
      systemctl start hydramesh
      hyprctl notify -1 4000 "rgb(33ff33)" "HydraMesh" "Service started"
      echo "ON" > /var/lib/hydramesh/hydramesh-status
    fi
  '';
  statusScript = pkgs.writeShellScriptBin "hydramesh-status" ''
    #!/usr/bin/env bash
    STATUS=$(systemctl is-active hydramesh)
    if [ "$STATUS" = "active" ]; then
      echo "{\"text\": \"🕸️ ON\", \"class\": \"hydramesh-active\", \"tooltip\": \"HydraMesh running\", \"icon\": \"/etc/hydramesh/hydramesh.svg\"}"
    else
      echo "{\"text\": \"🕸️ OFF\", \"class\": \"hydramesh-inactive\", \"tooltip\": \"HydraMesh stopped\", \"icon\": \"/etc/hydramesh/hydramesh.svg\"}"
    fi
  '';
in {
  # Note: HydraMesh codebase is sourced from ./HydraMesh and licensed under LGPL-3.0 (see ./HydraMesh/LICENSE).
  options.services.hydramesh = {
    enable = lib.mkEnableOption "HydraMesh Lisp service";
    configFile = lib.mkOption {
      type = lib.types.path;
      default = "/etc/hydramesh/config.json";
      description = "Path to HydraMesh config.json";
    };
    firewallEnable = lib.mkEnableOption "Enable firewall for HydraMesh ports";
    apparmorEnable = lib.mkEnableOption "Enable AppArmor profile for HydraMesh";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ sbclWithPkgs toggleScript statusScript streamdb ];

    # Copy entire HydraMesh directory to /etc/hydramesh
    environment.etc."hydramesh".source = ./HydraMesh;
    environment.etc."hydramesh/config.json".text = ''
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "transport": {
      "type": "string",
      "enum": ["gRPC", "native-lisp", "WebSocket"],
      "description": "Communication transport protocol (e.g., 'gRPC' for default interop)."
    },
    "host": {
      "type": "string",
      "description": "Host address (e.g., 'localhost' for local testing)."
    },
    "port": {
      "type": "integer",
      "minimum": 0,
      "maximum": 65535,
      "description": "Port number (e.g., 50051 for gRPC)."
    },
    "mode": {
      "type": "string",
      "enum": ["client", "server", "p2p", "auto", "master"],
      "description": "Node operating mode (e.g., 'p2p' for self-healing redundancy)."
    },
    "node-id": {
      "type": "string",
      "description": "Unique node identifier (e.g., UUID for distributed systems)."
    },
    "peers": {
      "type": "array",
      "items": { "type": "string" },
      "description": "List of peer addresses for P2P (e.g., ['peer1:50051', 'peer2:50052'])."
    },
    "group-rtt-threshold": {
      "type": "integer",
      "minimum": 0,
      "maximum": 1000,
      "description": "RTT threshold in ms for peer grouping (default 50 for <50ms clusters)."
    },
    "plugins": {
      "type": "object",
      "additionalProperties": true,
      "description": "Plugin configurations (e.g., {'udp': true} for custom transports)."
    },
    "storage": {
      "type": "string",
      "enum": ["streamdb", "in-memory"],
      "description": "Persistence backend (e.g., 'streamdb' for StreamDB integration)."
    },
    "streamdb-path": {
      "type": "string",
      "description": "Path to StreamDB file (required if storage='streamdb', e.g., 'dcf.streamdb')."
    },
    "optimization-level": {
      "type": "integer",
      "minimum": 0,
      "maximum": 3,
      "description": "Optimization level (e.g., 2+ enables StreamDB quick mode for ~100MB/s reads)."
    },
    "retry-max": {
      "type": "integer",
      "minimum": 1,
      "maximum": 10,
      "default": 3,
      "description": "Max retries for transient errors (e.g., in StreamDB ops or gRPC calls)."
    }
  },
  "required": ["transport", "host", "port", "mode"],
  "additionalProperties": true,
  "dependencies": {
    "storage": {
      "oneOf": [
        { "properties": { "storage": { "const": "streamdb" } }, "required": ["streamdb-path"] },
        { "properties": { "storage": { "const": "in-memory" } } }
      ]
    }
  }
}
    '';

    # HydraMesh systemd service
    systemd.services.hydramesh = {
      description = "HydraMesh Lisp Node";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.writeShellScriptBin "hydramesh-start" ''
          # Setup Quicklisp if not present
          if [ ! -d "/root/quicklisp" ]; then
            curl -O https://beta.quicklisp.org/quicklisp.lisp
            ${sbclWithPkgs}/bin/sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
          fi
          ${sbclWithPkgs}/bin/sbcl --load /root/quicklisp/setup.lisp \
            --load /etc/hydramesh/src/hydramesh.lisp \
            --eval '(dolist (plugin (directory "/etc/hydramesh/plugins/*.lisp")) (load plugin))' \
            --eval '(in-package :hydramesh)' \
            --eval '(hydramesh-init "${cfg.configFile}" :restore-state t)' \
            --eval '(hydramesh-start)' \
            --non-interactive
        ''}/bin/hydramesh-start";
        Restart = "always";
        User = "hydramesh";
        Group = "hydramesh";
        WorkingDirectory = "/etc/hydramesh";
        Environment = "LD_LIBRARY_PATH=${streamdb}/lib";
        DynamicUser = true;
        PrivateDevices = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        NoNewPrivileges = true;
        CapabilityBoundingSet = "";
        RestrictNamespaces = true;
        SystemCallFilter = "@system-service ~@privileged";
      };
    };

    # Firewall rules if enabled
    networking.firewall = lib.mkIf cfg.firewallEnable {
      allowedTCPPorts = let
        configJson = builtins.fromJSON config.environment.etc."hydramesh/config.json".text;
      in [ configJson.port ];  # Dynamic port from config.json
      allowedUDPPorts = lib.optionals (builtins.hasAttr "plugins" configJson && 
                                      builtins.hasAttr "lorawan" configJson.plugins && 
                                      configJson.plugins.lorawan) [ 5683 ];  # CoAP for LoRaWAN
    };

    # AppArmor profile if enabled
    security.apparmor = lib.mkIf cfg.apparmorEnable {
      enable = true;
      profiles = [ (pkgs.writeText "apparmor-hydramesh" ''
        #include <tunables/global>
        /usr/bin/sbcl flags=(complain) {
          #include <abstractions/base>
          capability dac_override,
          network tcp,
          network udp,
          file,
          /etc/hydramesh/** r,
          /var/lib/hydramesh/** rw,
        }
      '') ];
    };

    # Dedicated user for security
    users.users.hydramesh = {
      isSystemUser = true;
      group = "hydramesh";
      home = "/var/lib/hydramesh";
      createHome = true;
    };
    users.groups.hydramesh = {};
  };
}
