# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# archibald.engine — the DeMoD audio engine as DeMoD runs it: the orchestrator
# (control socket, shared memory, supervision) with demod-rt as its child.
#
# demod-rt alone cannot start: it needs the orchestrator's shared memory, which
# is why services.demod-rt (modules/demod-rt.nix) never could. This is the
# boot order DeMoD's own appliance uses (docker/entrypoint.sh): jackd, then the
# orchestrator with --control-socket, then the bridges.
#
# demod-rt never connects its own ports; archibald.jack.routes does: to the
# interface (io = "system"), or, on a DSP host, to every NetJack2 box
# (modules/netjack.nix, role = "manager").
#
# The audio stack is DeMoD's, GPL-3.0-only or commercial (DeMoD
# LICENSING.md); it reaches this tree as packages of the `demod` flake input.
{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption mkEnableOption mkIf mkMerge types optionals;
  cfg = config.archibald.engine;
  jack = config.archibald.jack;
  p = cfg.packages;
in
{
  imports = [ ./jack-graph.nix ];

  options.archibald.engine = {
    enable = mkEnableOption "the DeMoD audio engine (orchestrator + demod-rt)";
    packages = mkOption {
      type = types.nullOr (types.attrsOf types.package);
      default = null;
      description = "The `demod` flake's packages for this system (demod-orchestrator, demod-rt, demod-remote-bridge).";
    };
    rtCore = mkOption { type = types.ints.unsigned; default = 0; description = "The CPU demod-rt's audio thread is pinned to."; };
    controlSocket = mkOption { type = types.str; default = "/run/demod/control.sock"; description = "The orchestrator's control socket."; };
    io = mkOption {
      type = types.enum [ "system" "none" ];
      default = "system";
      description = ''
        system: the interface's inputs 1-2 into demod-rt, its output to the
        interface's outputs 1-2. none: leave it to other rules (a DSP host
        wires NetJack2 boxes instead).
      '';
    };
    remote = {
      enable = mkEnableOption "demod-remote-bridge, so a DeMoD app elsewhere (a kiosk, TERMINUS on Oligarchy) can drive this engine over DCF";
      bind = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = "Address the bridge binds. It admits private senders only and gates every datagram (DeMoD commit 015cde2); still, bind the tunnel address, not 0.0.0.0.";
      };
      port = mkOption { type = types.port; default = 47000; description = "The bridge's UDP port."; };
      interface = mkOption { type = types.nullOr types.str; default = null; description = "Open the port on this interface only."; };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      assertions = [
        {
          assertion = p != null && p ? demod-orchestrator && p ? demod-rt;
          message = "archibald.engine needs archibald.engine.packages: the demod flake input's packages (demod-orchestrator, demod-rt).";
        }
        {
          assertion = jack.user != null;
          message = "archibald.engine needs archibald.jack.user: the engine runs as the JACK server's account.";
        }
      ];

      systemd.services.demod-orchestrator = {
        description = "DeMoD engine (orchestrator + demod-rt)";
        bindsTo = [ jack.unit ];
        after = [ jack.unit ];
        wantedBy = [ jack.unit "multi-user.target" ];
        serviceConfig = {
          User = jack.user;
          Group = "audio";
          Restart = "always";
          RestartSec = 3;
          RuntimeDirectory = "demod";
          RuntimeDirectoryMode = "0750";
          StateDirectory = "demod";
          LimitRTPRIO = 95;
          LimitMEMLOCK = "infinity";
          ExecStart = "${p.demod-orchestrator}/bin/demod-orchestrator"
            + " --control-socket ${cfg.controlSocket}"
            + " --rt-binary ${p.demod-rt}/bin/demod-rt"
            + " --rt-core ${toString cfg.rtCore}"
            + " --data-dir /var/lib/demod";
        };
      };

      archibald.jack.routes = optionals (cfg.io == "system") [
        "system:capture_1 -> demod-rt:in_L"
        "system:capture_2 -> demod-rt:in_R"
        "demod-rt:out_L -> system:playback_1"
        "demod-rt:out_R -> system:playback_2"
      ];
      environment.systemPackages = [ p.demod-orchestrator p.demod-rt ];
    }

    (mkIf cfg.remote.enable {
      assertions = [{
        assertion = p ? demod-remote-bridge;
        message = "archibald.engine.remote needs demod-remote-bridge in archibald.engine.packages.";
      }];
      systemd.services.demod-remote-bridge = {
        description = "DeMoD remote bridge (DCF on ${cfg.remote.bind}:${toString cfg.remote.port})";
        after = [ "demod-orchestrator.service" ];
        bindsTo = [ "demod-orchestrator.service" ];
        wantedBy = [ "demod-orchestrator.service" ];
        environment = {
          DEMOD_DCF_BIND = cfg.remote.bind;
          DEMOD_DCF_PORT = toString cfg.remote.port;
          DEMOD_CONTROL_SOCK = cfg.controlSocket;
        };
        serviceConfig = {
          User = jack.user;
          Group = "audio";
          Restart = "always";
          RestartSec = 2;
          ExecStart = "${p.demod-remote-bridge}/bin/demod-remote-bridge";
          NoNewPrivileges = true;
          RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" ];
          ProtectSystem = "strict";
          ProtectHome = true;
          CapabilityBoundingSet = "";
        };
      };
      networking.firewall.interfaces = mkIf (cfg.remote.interface != null) {
        ${cfg.remote.interface}.allowedUDPPorts = [ cfg.remote.port ];
      };
    })
  ]);
}
