# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# checks.roles-contract — eval-only. The companion roles on every form factor:
# the kiosk sleeps until a touchscreen appears and then runs DeMoD Mixer as the
# audio user against the right engine; a box with a DSP host routes it through
# WireGuard and joins it with NetJack2; a rack unit runs the engine itself; the
# Raspberry Pi and RISC-V images keep their board kernels.
#
# What eval cannot show (checks.netjack2 shows the NetJack2 half by running
# it): a touchscreen actually starting cage, the mixer drawing on a panel,
# and the images booting.
{ pkgs, mkInstalled, configs }:

let
  lib = pkgs.lib;
  fx = name: mkInstalled (./fixtures/installed + "/${name}");
  comp = (fx "companion-efi").config;
  linked = ((fx "companion-enrolled").extendModules {
    modules = [{ archibald.companion.dsp = { host = "10.78.0.2"; netjack = true; }; }];
  }).config;
  rack = (fx "rack-efi").config;
  pi4 = configs.companion-pi4.config;
  pi5 = configs.companion-pi5.config;
  rv = configs.companion-riscv.config;
  failed = c: map (a: a.message) (lib.filter (a: !a.assertion) c.assertions);
  cage = c: c.systemd.services."cage-tty1";
  env = c: c.services.cage.environment;
  routes = c: c.archibald.jack.routes;
  orch = c: c.systemd.services.demod-orchestrator.serviceConfig.ExecStart;
  isCachy = c: c.boot.kernelPackages.kernel.passthru ? cachyConfig;
  sys = name: configs.${name}.pkgs.stdenv.hostPlatform.system;

  checks = [
    # ── kiosk ──────────────────────────────────────────────────────────────
    { name = "kiosk: nothing starts cage at boot; udev starts it when a touchscreen appears";
      ok = lib.all (c: (cage c).wantedBy == [ ] && !(lib.elem "cage-tty1.service" (c.systemd.targets.graphical.wants or [ ]))
          && lib.hasInfix ''ENV{ID_INPUT_TOUCHSCREEN}=="1"'' c.services.udev.extraRules
          && lib.hasInfix ''ENV{SYSTEMD_WANTS}+="cage-tty1.service"'' c.services.udev.extraRules)
        [ comp rack pi4 rv ]; }
    { name = "kiosk: DeMoD Mixer, fullscreen with no cursor, as the audio user";
      ok = lib.all (c: c.services.cage.user == c.archibald.companion.user
          && lib.hasSuffix "/bin/demod-mixer" c.services.cage.program
          && (env c).DEMOD_KIOSK == "1")
        [ comp rack pi4 pi5 rv ]; }
    { name = "kiosk: drives the local engine on a rack unit, the DSP host's when one is set, else says SIMULATOR";
      ok = (env rack).DEMOD_MIXER_ENGINE == "local"
        && (env linked).DEMOD_MIXER_ENGINE == "remote:10.78.0.2"
        && (env comp).DEMOD_MIXER_ENGINE == "sim"; }

    # ── the link to the DSP host ─────────────────────────────────────────
    { name = "DSP host: routed into the WireGuard link, and nothing else is";
      ok = (lib.head linked.networking.wireguard.interfaces.wg-oligarchy.peers).allowedIPs == [ "10.77.0.1/32" "10.78.0.2/32" ]; }
    { name = "DSP host: the box joins it with NetJack2 under its host name, bound to its JACK";
      ok = let s = linked.systemd.services.jack-netadapter; in
        lib.hasInfix "netadapter -i '-a 10.78.0.2 -p 19000 -n surface" s.serviceConfig.ExecStart
        && s.bindsTo == [ "jack2-alsa.service" ] && s.serviceConfig.User == "asher"; }
    { name = "DSP host: NetJack2's UDP opened on the tunnel only";
      ok = linked.networking.firewall.interfaces.wg-oligarchy.allowedUDPPortRanges == [{ from = 1024; to = 65535; }]
        && !lib.elem 19000 linked.networking.firewall.allowedUDPPorts; }
    { name = "DSP host: the box's interface is wired to the adapter and back";
      ok = lib.elem "system:capture_1 -> netadapter:playback_1" (routes linked)
        && lib.elem "netadapter:capture_1 -> system:playback_1" (routes linked)
        && linked.systemd.services.jack-router.serviceConfig.User == "asher"; }

    # ── rack unit ──────────────────────────────────────────────────────────
    { name = "rack: the engine runs here, the orchestrator supervising demod-rt, as the audio user";
      ok = lib.hasInfix "--control-socket /run/demod/control.sock" (orch rack)
        && lib.hasInfix "/bin/demod-rt --rt-core 0" (orch rack)
        && rack.systemd.services.demod-orchestrator.serviceConfig.User == rack.archibald.companion.user
        && rack.systemd.services.demod-orchestrator.bindsTo == [ "jack2-alsa.service" ]; }
    { name = "rack: the interface is wired through demod-rt";
      ok = lib.elem "system:capture_1 -> demod-rt:in_L" (routes rack)
        && lib.elem "demod-rt:out_R -> system:playback_2" (routes rack); }
    { name = "a plain companion runs no engine and no router";
      ok = !(comp.systemd.services ? demod-orchestrator) && !(comp.systemd.services ? jack-router); }

    # ── boards ─────────────────────────────────────────────────────────────
    { name = "Raspberry Pi 4/5: aarch64, the board's kernel (not CachyOS), an SD image";
      ok = sys "companion-pi4" == "aarch64-linux" && sys "companion-pi5" == "aarch64-linux"
        && lib.all (c: !(isCachy c) && c.system.build ? sdImage) [ pi4 pi5 ]; }
    { name = "x86 companions keep CachyOS";
      ok = isCachy comp && isCachy rack; }
    { name = "RISC-V: networkd not NetworkManager, software rendering for the panel";
      ok = !rv.networking.networkmanager.enable && rv.networking.useNetworkd
        && (env rv).WLR_RENDERER == "pixman" && sys "companion-riscv" == "riscv64-linux"; }
    { name = "SD images: the published password is expired on first boot";
      ok = lib.all (c: lib.hasInfix "chage -d 0 archibald" c.systemd.services.archibald-expire-password.script)
        [ pi4 pi5 rv ]; }
    { name = "every configuration here evaluates with no failed assertion";
      ok = lib.all (c: failed c == [ ]) [ comp linked rack pi4 pi5 rv ]; }
  ];

  bad = lib.filter (x: !x.ok) checks;
  report = lib.concatMapStringsSep "\n" (x: (if x.ok then "PASS: " else "FAIL: ") + x.name) checks;
in
pkgs.runCommand "roles-contract" { inherit report; passAsFile = [ "report" ]; } ''
  cat "$reportPath"; echo
  echo "${toString (lib.length checks - lib.length bad)}/${toString (lib.length checks)} checks passed"
  ${if bad == [ ] then ''cp "$reportPath" $out'' else "exit 1"}
''
