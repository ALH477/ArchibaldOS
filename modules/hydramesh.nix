# nixos-module.nix (or HydraMesh/nixos-module.nix)
# Copyright 2025 DeMoD LLC
# SPDX-License-Identifier: BSD-3-Clause
#
# Redistribution and use in source and binary forms, with or without modification,
# are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its contributors
#    may be used to endorse or promote products derived from this software
#    without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS”
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
# GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
# OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{ config, pkgs, lib, ... }:

let
  cfg = config.services.hydramesh;

  # Simple wrapper that runs the standalone HydraMesh executable directly
  # No Quicklisp loading needed because all dependencies are baked into the core by dcf-deploy
  hydrameshWrapped = pkgs.writeShellScriptBin "hydramesh-wrapped" ''
    exec ${pkgs.hydramesh}/bin/hydramesh "$@"
  '';

in {
  options.services.hydramesh = {
    enable = lib.mkEnableOption "HydraMesh DCF node service";

    configFile = lib.mkOption {
      type = lib.types.path;
      default = "/etc/hydramesh/config.json";
      description = "Path to the HydraMesh configuration file (config.json).";
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = "Additional command-line arguments passed to HydraMesh.";
    };

    firewallEnable = lib.mkEnableOption "Automatically open firewall ports based on config.json";

    apparmorEnable = lib.mkEnableOption "Enable custom AppArmor profile for confinement";
  };

  config = lib.mkIf cfg.enable {
    # System-wide packages
    environment.systemPackages = [
      pkgs.hydramesh
      hydrameshWrapped
      pkgs.sbcl  # Optional: for debugging or manual runs
    ];

    # Default/example configuration (users should override)
    environment.etc."hydramesh/config.json".text = builtins.toJSON {
      "$schema" = "http://json-schema.org/draft-07/schema#";
      type = "object";
      properties = {
        transport = { type = "string"; enum = [ "UDP" "gRPC" "native-lisp" "WebSocket" ]; default = "UDP"; };
        host = { type = "string"; default = "0.0.0.0"; };
        port = { type = "integer"; minimum = 0; maximum = 65535; default = 50051; };
        "udp-port" = { type = "integer"; minimum = 0; maximum = 65535; default = 7777; };
        mode = { type = "string"; enum = [ "client" "server" "p2p" "auto" "master" ]; default = "p2p"; };
        "node-id" = { type = "string"; };
        peers = { type = "array"; items = { type = "string"; }; default = []; };
        "group-rtt-threshold" = { type = "integer"; minimum = 0; maximum = 1000; default = 50; };
        storage = { type = "string"; enum = [ "streamdb" "in-memory" ]; default = "streamdb"; };
        "streamdb-path" = { type = "string"; default = "/var/lib/hydramesh/streamdb.db"; };
        "optimization-level" = { type = "integer"; minimum = 0; maximum = 3; default = 2; };
        "retry-max" = { type = "integer"; minimum = 1; maximum = 10; default = 3; };
        "udp-mtu" = { type = "integer"; default = 1400; };
        "udp-reliable-timeout" = { type = "integer"; default = 500; };
        "audio-priority" = { type = "boolean"; default = true; };
      };
      required = [ "transport" "host" "mode" ];
      additionalProperties = true;
    };

    # Systemd service
    systemd.services.hydramesh = {
      description = "HydraMesh v2.2.0 – High-performance Common Lisp DCF Node";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      restartIfChanged = true;

      serviceConfig = {
        ExecStart = "${hydrameshWrapped}/bin/hydramesh-wrapped init ${cfg.configFile} ${lib.escapeShellArgs cfg.extraArgs}";
        Restart = "always";
        DynamicUser = true;
        User = "hydramesh";
        Group = "hydramesh";
        WorkingDirectory = "/var/lib/hydramesh";
        StateDirectory = "hydramesh";
        StateDirectoryMode = "0700";

        # Security hardening
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        NoNewPrivileges = true;
        RestrictSUIDSGID = true;
        RemoveIPC = true;
        RestrictNamespaces = true;
        CapabilityBoundingSet = "~CAP_SYS_ADMIN ~CAP_SYS_BOOT ~CAP_SYS_MODULE ~CAP_SYS_RAWIO";
        SystemCallArchitectures = "native";
        SystemCallFilter = "@system-service ~@privileged @resources";

        # StreamDB shared library path
        Environment = "LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.streamdb ]}";
      };
    };

    # Dynamic firewall rules from config.json
    networking.firewall = lib.mkIf cfg.firewallEnable {
      allowedTCPPorts = let
        c = builtins.fromJSON (builtins.readFile cfg.configFile);
      in [ c.port ] ++ lib.optional (c ? "udp-port") c."udp-port";

      allowedUDPPorts = let
        c = builtins.fromJSON (builtins.readFile cfg.configFile);
      in [ c.port ] ++ lib.optional (c ? "udp-port") c."udp-port";
    };

    # Optional AppArmor profile
    security.apparmor.profiles = lib.mkIf cfg.apparmorEnable [
      (pkgs.writeText "apparmor-hydramesh" ''
        #include <tunables/global>

        ${hydrameshWrapped}/bin/hydramesh-wrapped {
          #include <abstractions/base>
          #include <abstractions/nameservice>

          capability dac_override,
          capability net_bind_service,
          capability net_raw,

          network inet stream,
          network inet dgram,
          network inet6 stream,
          network inet6 dgram,

          /etc/hydramesh/** r,
          /var/lib/hydramesh/** rwk,

          ${pkgs.streamdb}/lib/libstreamdb.so r,

          /usr/bin/sbcl ix,
          /run/current-system/sw/bin/** ix,
        }
      '')
    ];

    # System user and group
    users.users.hydramesh = {
      isSystemUser = true;
      group = "hydramesh";
      home = "/var/lib/hydramesh";
      createHome = true;
      description = "HydraMesh DCF node system user";
    };

    users.groups.hydramesh = {};
  };
}
