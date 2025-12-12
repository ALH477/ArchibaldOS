# Copyright 2025 DeMoD LLC

# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



{
  config, pkgs, lib, ... }:

let
  cfg = config.services.hydramesh;

  configJson = builtins.fromJSON (builtins.readFile cfg.configFile);

in {
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
    environment.systemPackages = [ pkgs.sbcl pkgs.hydramesh toggleScript statusScript pkgs.quicklisp pkgs.streamdb ];

    environment.etc."hydramesh".source = ./HydraMesh;

    environment.etc."hydramesh/config.json".text = builtins.toJSON {
      "$schema" = "http://json-schema.org/draft-07/schema#";
      type = "object";
      properties = {
        transport = { type = "string"; enum = [ "gRPC" "native-lisp" "WebSocket" ]; };
        host = { type = "string"; };
        port = { type = "integer"; minimum = 0; maximum = 65535; };
        mode = { type = "string"; enum = [ "client" "server" "p2p" "auto" "master" ]; };
        "node-id" = { type = "string"; };
        peers = { type = "array"; items = { type = "string"; }; };
        "group-rtt-threshold" = { type = "integer"; minimum = 0; maximum = 1000; };
        plugins = { type = "object"; additionalProperties = true; };
        storage = { type = "string"; enum = [ "streamdb" "in-memory" ]; };
        "streamdb-path" = { type = "string"; };
        "optimization-level" = { type = "integer"; minimum = 0; maximum = 3; };
        "retry-max" = { type = "integer"; minimum = 1; maximum = 10; default = 3; };
      };
      required = [ "transport" "host" "port" "mode" ];
      additionalProperties = true;
      dependencies.storage.oneOf = [
        {
          properties.storage.const = "streamdb";
          required = [ "streamdb-path" ];
        }
        {
          properties.storage.const = "in-memory";
        }
      ];
    };

    systemd.services.hydramesh = {
      description = "HydraMesh Lisp Node";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.hydramesh}/bin/hydramesh-wrapped";
        Restart = "always";
        User = "hydramesh";
        Group = "hydramesh";
        WorkingDirectory = "/etc/hydramesh";
        Environment = "LD_LIBRARY_PATH=${pkgs.streamdb}/lib";
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

    networking.firewall = lib.mkIf cfg.firewallEnable {
      allowedTCPPorts = lib.optionals (configJson ? port) [ configJson.port ];
      allowedUDPPorts = lib.optionals (configJson ? port) [ configJson.port ];
    };

    security.apparmor = lib.mkIf cfg.apparmorEnable {
      enable = true;
      profiles = [ (pkgs.writeText "apparmor-hydramesh" ''
        #include <tunables/global>
        ${pkgs.hydramesh}/bin/hydramesh-wrapped flags=(complain) {
          #include <abstractions/base>
          capability dac_override,
          network tcp,
          network udp,
          file,
          /etc/hydramesh/** r,
          /var/lib/hydramesh/** rw,
          /etc/quicklisp/** r,
        }
      '') ];
    };

    users.users.hydramesh = {
      isSystemUser = true;
      group = "hydramesh";
      home = "/var/lib/hydramesh";
      createHome = true;
    };

    users.groups.hydramesh = {};
  };
}
