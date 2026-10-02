# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025-2026 DeMoD LLC. All rights reserved.
# ============================================================================
# DSP control bridge — TCP → the engine's JSON-lines control socket.
# ============================================================================
# Shared by the DSP VM guest (headless-dsp.nix) and the companion profile
# (companion.nix). The control protocol has load_fx / synth.load, which make
# the engine dlopen a path, so who may reach this port is decided here:
# socat's own `range=` refuses any peer outside `allowFrom` (in-process, so it
# holds where cgroup IP filtering is unavailable), systemd's IPAddressAllow
# repeats it, and the service runs as the engine's user with no capabilities.
# Opening the port in the firewall is the importing module's decision.
# The default socket is archibald.engine's when the engine is on, else
# services.demod-rt's (demod-rt.nix); a host with neither sets `socket`.
{ config, lib, pkgs, ... }:

let
  cfg = config.archibald.dsp.control;
in
{
  options.archibald.dsp.control = {
    enable = lib.mkEnableOption "the TCP bridge to the DSP engine's control socket";
    user = lib.mkOption {
      type = lib.types.str;
      description = "Account the bridge runs as: the engine's own user, never root.";
    };
    group = lib.mkOption {
      type = lib.types.str;
      default = "audio";
      description = "Group the bridge runs as.";
    };
    socket = lib.mkOption {
      type = lib.types.str;
      # The engine as DeMoD runs it (archibald.engine) when that is on, else
      # the older services.demod-rt. `or` so this module can be imported
      # without either (a host that only sets `socket`).
      default =
        if config.archibald.engine.enable or false then config.archibald.engine.controlSocket
        else config.services.demod-rt.controlSocket;
      defaultText = lib.literalExpression "config.archibald.engine.controlSocket (or config.services.demod-rt.controlSocket)";
      description = "The engine's control socket the bridge relays to.";
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 7777;
      description = "TCP port of the DSP control bridge (JSON lines to the engine's control socket).";
    };
    allowFrom = lib.mkOption {
      type = lib.types.strMatching "[0-9]{1,3}(\\.[0-9]{1,3}){3}/[0-9]{1,2}";
      default = "10.0.2.2/32";
      example = "192.168.122.1/32";
      description = ''
        The one IPv4 range (CIDR) the control bridge accepts connections from.
        socat enforces it per connection (`range=`), and systemd's
        IPAddressAllow repeats it. The default is the address QEMU user-mode
        networking gives the host, so a host-side 127.0.0.1 hostfwd works and
        nothing else does. Anyone who can reach this port can make the engine
        load a shared object, so keep it to the host.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # ── DSP Control Bridge — expose orchestrator control socket over TCP ──────
    # The DeMoD orchestrator already has a JSON-lines Unix domain socket at
    # /run/demod/control.sock. This bridge forwards TCP:7777 → that socket
    # so remote clients (Oligarchy dsp-ctl, USB networking, HydraMesh) can
    # control the DSP coprocessor.
    #
    # No Python — just socat, which is ~zero overhead.
    #
    # Protocol: JSON-lines (one JSON object per line, response per line)
    # Commands: ping, get_health, get_state, load_fx, unload_fx,
    #           set_param, fx_bypass, set_bpm, set_gain, note_on, note_off
    #
    # Usage from host (through a hostfwd to this guest's control port):
    #   dsp-ctl --transport tcp --host 127.0.0.1 --port <forwarded port> status
    #
    # It used to run as root, listen on every address with the firewall off, and
    # accept anyone. Now: the engine's own user, no capabilities, a syscall and
    # address-family allowlist, and socat's `range=` refusing any peer outside
    # allowFrom before a byte is relayed.
    systemd.services.dsp-control-bridge = {
      description = "DSP Control Bridge — TCP → orchestrator Unix socket";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" "demod-rt.service" "demod-orchestrator.service" ];

      serviceConfig = {
        Type = "simple";
        Restart = "on-failure";
        RestartSec = 3;

        # fork = one process per connection, reuseaddr = quick reconnect,
        # range = the only peers socat will talk to (checked per accept).
        ExecStart = "${pkgs.socat}/bin/socat "
          + "TCP4-LISTEN:${toString cfg.port},reuseaddr,fork,range=${cfg.allowFrom} "
          + "UNIX-CONNECT:${cfg.socket}";

        User = cfg.user;
        Group = cfg.group;
        NoNewPrivileges = true;
        CapabilityBoundingSet = [ "" ];
        AmbientCapabilities = [ "" ];
        RestrictAddressFamilies = [ "AF_INET" "AF_UNIX" ];
        IPAddressDeny = "any";
        IPAddressAllow = [ cfg.allowFrom ];
        SystemCallFilter = [ "@system-service" "~@privileged" "~@resources" ];
        SystemCallArchitectures = "native";
        ProtectSystem = "strict";
        ReadWritePaths = [ "-${dirOf cfg.socket}" ];
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        ProtectClock = true;
        ProtectHostname = true;
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        UMask = "0077";
      };
    };
  };
}
