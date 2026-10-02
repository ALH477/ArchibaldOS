# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC. All rights reserved.
# Options for the DSP guest's control bridge (headless-dsp.nix). Declared in
# their own module so headless-dsp.nix can stay a flat list of settings.
{ lib, ... }:

{
  options.archibald.dsp.control = {
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
}
