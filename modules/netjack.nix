# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# archibald.netjack — NetJack2 between a box with an audio interface and a DSP
# host (the Oligarchy DSP VM) that runs the DeMoD engine.
#
#   manager  the DSP host: jack2's `netmanager` internal client in its JACK
#            server. Every box that joins appears there as a client named
#            after the box's host, with from_slave_N / to_slave_N ports, and
#            jack-router wires those into demod-rt and back.
#   adapter  the box: jack2's `netadapter` in the box's own JACK server (which
#            runs on the box's interface). It joins the manager by address and
#            resamples between the two clocks, so any number of boxes can join
#            one host. Its playback_N ports carry the box's input to the host,
#            its capture_N ports bring the processed signal back.
#
# Unicast, not NetJack2's multicast discovery: the path is WireGuard and
# routed, where multicast does not travel. Measured in the build sandbox
# (checks.netjack2): the manager listens on UDP `port`, and each joined box
# then gets its own connected UDP pair on ephemeral ports, so the tunnel
# interface admits UDP 1024-65535. WireGuard's per-peer allowed IPs are what
# bound who can send it.
#
# The earlier units ran `jack_netsource`, the netone master, on both sides
# (two masters, no slave), with flags it does not have (-C, -l, -r as a sample
# rate), from a package that does not ship it. That chain could not form.
{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption mkIf mkMerge types optionals range;
  cfg = config.archibald.netjack;
  jack = config.archibald.jack;
  tools = pkgs.jack-example-tools;
  chans = n: range 1 n;
  firewall = mkIf (cfg.interface != null) {
    networking.firewall.interfaces.${cfg.interface} = {
      allowedUDPPorts = optionals (cfg.role == "manager") [ cfg.port ];
      allowedUDPPortRanges = [{ from = 1024; to = 65535; }];
    };
  };
in
{
  imports = [ ./jack-graph.nix ];

  options.archibald.netjack = {
    role = mkOption {
      type = types.nullOr (types.enum [ "adapter" "manager" ]);
      default = null;
      description = "adapter: this box joins a DSP host. manager: this is the DSP host.";
    };
    port = mkOption { type = types.port; default = 19000; description = "The manager's UDP port."; };
    address = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "10.78.0.2";
      description = ''
        adapter: the DSP host to join. manager: this host's own address on the
        link boxes come from (the manager listens on every address regardless).
      '';
    };
    name = mkOption {
      type = types.str;
      default = config.networking.hostName;
      defaultText = lib.literalExpression "config.networking.hostName";
      description = "adapter: the client name this box appears under on the DSP host.";
    };
    channels = {
      capture = mkOption { type = types.ints.between 1 32; default = 2; description = "Channels from the box to the host."; };
      playback = mkOption { type = types.ints.between 1 32; default = 2; description = "Channels from the host back to the box."; };
    };
    routeSystem = mkOption {
      type = types.bool;
      default = true;
      description = "adapter: wire the interface's inputs to the host and the host's return to the interface's outputs.";
    };
    toEngine = mkOption {
      type = types.bool;
      default = config.archibald.engine.enable or false;
      defaultText = lib.literalExpression "config.archibald.engine.enable";
      description = "manager: wire every box's channels 1-2 into demod-rt and its output back to every box.";
    };
    interface = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "wg-oligarchy";
      description = "Open NetJack2's UDP on this interface only. null opens nothing.";
    };
  };

  config = mkMerge [
    (mkIf (cfg.role == "adapter") (mkMerge [
      {
        assertions = [{
          assertion = cfg.address != null && jack.user != null;
          message = "archibald.netjack adapter needs address (the DSP host) and archibald.jack.user.";
        }];
        systemd.services.jack-netadapter = {
          description = "NetJack2 adapter: this box's audio to the DSP host at ${toString cfg.address}";
          bindsTo = [ jack.unit ];
          after = [ jack.unit "network-online.target" ];
          wants = [ "network-online.target" ];
          wantedBy = [ jack.unit ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            User = jack.user;
            Group = "audio";
            ExecStartPre = "${tools}/bin/jack_wait -w -t 30";
            # The adapter keeps looking for its manager in its own thread, so
            # this succeeds while the DSP host is still booting.
            ExecStart = "${tools}/bin/jack_load netadapter -i '-a ${toString cfg.address} -p ${toString cfg.port} -n ${cfg.name} -C ${toString cfg.channels.capture} -P ${toString cfg.channels.playback}'";
            ExecStop = "${tools}/bin/jack_unload netadapter";
          };
        };
        archibald.jack.routes = optionals cfg.routeSystem (
          map (i: "system:capture_${toString i} -> netadapter:playback_${toString i}") (chans cfg.channels.capture)
          ++ map (i: "netadapter:capture_${toString i} -> system:playback_${toString i}") (chans cfg.channels.playback)
        );
      }
      firewall
    ]))

    (mkIf (cfg.role == "manager") (mkMerge [
      {
        assertions = [{
          assertion = jack.user != null;
          message = "archibald.netjack manager needs archibald.jack.user.";
        }];
        systemd.services.jack-netmanager = {
          description = "NetJack2 manager: boxes join this DSP host on UDP ${toString cfg.port}";
          bindsTo = [ jack.unit ];
          after = [ jack.unit ];
          wantedBy = [ jack.unit ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            User = jack.user;
            Group = "audio";
            ExecStartPre = "${tools}/bin/jack_wait -w -t 30";
            ExecStart = "${tools}/bin/jack_load netmanager -i '-a ${if cfg.address != null then cfg.address else "0.0.0.0"} -p ${toString cfg.port}'";
            ExecStop = "${tools}/bin/jack_unload netmanager";
          };
        };
        archibald.jack.routes = optionals cfg.toEngine [
          "*:from_slave_1 -> demod-rt:in_L"
          "*:from_slave_2 -> demod-rt:in_R"
          "demod-rt:out_L -> *:to_slave_1"
          "demod-rt:out_R -> *:to_slave_2"
        ];
      }
      firewall
    ]))
  ];
}
