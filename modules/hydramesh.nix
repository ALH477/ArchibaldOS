{ config, pkgs, lib, ... }:

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
    environment.systemPackages = [ pkgs.sbcl pkgs.hydramesh-toggle pkgs.hydramesh-status pkgs.streamdb ];

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
        ExecStart = "${pkgs.writeShellScriptBin "hydramesh-start" ''
          if [ ! -d "/root/quicklisp" ]; then
            curl -O https://beta.quicklisp.org/quicklisp.lisp
            ${pkgs.sbcl}/bin/sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
          fi
          ${pkgs.sbcl}/bin/sbcl --load /root/quicklisp/setup.lisp \
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

    users.users.hydramesh = {
      isSystemUser = true;
      group = "hydramesh";
      home = "/var/lib/hydramesh";
      createHome = true;
    };

    users.groups.hydramesh = {};
  };
}
