# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# archibald.jack — who owns the JACK server, and how its graph is wired.
#
# Several modules need ports connected: the DeMoD engine to the interface,
# a box's interface to its NetJack2 adapter, every box that joins a DSP host to
# that host's engine. They append rules to `archibald.jack.routes` and one
# jack-router (tools/jack-router) applies them all, now and whenever a port
# appears, as the server's own user. Nothing here disconnects anything.
{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption mkIf types escapeShellArgs;
  cfg = config.archibald.jack;
  router = pkgs.callPackage ../tools/jack-router { };
in
{
  options.archibald.jack = {
    user = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        The account the JACK server runs as. JACK keeps one server per user, so
        every client that must see it (the engine, the router, the kiosk) runs
        as this account too.
      '';
    };
    unit = mkOption {
      type = types.str;
      default = "jack2-alsa.service";
      description = "The unit that runs the JACK server; the router lives and dies with it.";
    };
    routes = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = [ "system:capture_1 -> demod-rt:in_L" "*:from_slave_1 -> demod-rt:in_L" ];
      description = ''
        `client:port -> client:port` rules for jack-router. `*:port` on one
        side matches that port on any client (a NetJack2 box joins as a client
        named after its host).
      '';
    };
  };

  config = mkIf (cfg.routes != [ ]) {
    assertions = [{
      assertion = cfg.user != null;
      message = "archibald.jack.routes is set but archibald.jack.user is not: the router must run as the JACK server's account.";
    }];

    environment.systemPackages = [ router ];

    systemd.services.jack-router = {
      description = "Keep the JACK graph wired (jack-router)";
      bindsTo = [ cfg.unit ];
      after = [ cfg.unit ];
      wantedBy = [ cfg.unit ];
      serviceConfig = {
        User = cfg.user;
        Group = "audio";
        Restart = "always";
        RestartSec = 2;
        ExecStart = "${router}/bin/jack-router ${escapeShellArgs cfg.routes}";
        # It only talks to JACK, over Unix sockets under /dev/shm.
        NoNewPrivileges = true;
        PrivateNetwork = true;
        RestrictAddressFamilies = [ "AF_UNIX" ];
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = false; # JACK's sockets live in /dev/shm, which stays shared
        CapabilityBoundingSet = "";
      };
    };
  };
}
