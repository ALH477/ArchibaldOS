# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# checks.installed-contract — eval-only. What the installer writes
# (hosts/installed/install.json + the hardware scan) becomes the system the
# user chose, through flake.nix's mkInstalled and installer/installed.nix.
#
# The fixtures under tests/fixtures/installed/ are in exactly the layout the
# installer leaves in /etc/nixos/hosts/installed. Each check below fails on the
# tree before the installer existed for the simplest reason: there was no
# mkInstalled, and the ISO installed a generic configuration.nix instead.
{ pkgs, mkInstalled }:

let
  lib = pkgs.lib;
  fx = name: mkInstalled (./fixtures/installed + "/${name}");
  cfg = name: (fx name).config;

  comp = cfg "companion-efi";
  enr = cfg "companion-enrolled";
  surf = cfg "companion-surface";
  audio = cfg "audio-bios-luks";

  jack = c: c.systemd.services.jack2-alsa.serviceConfig.ExecStart;
  sudoCmds = c: lib.concatMap (r: map (x: x.command) r.commands)
    (lib.filter (r: r.users == [ c.archibald.companion.user ]) c.security.sudo.extraRules);
  failedAssertions = c: map (a: a.message) (lib.filter (a: !a.assertion) c.assertions);

  checks = [
    # ── the installer's answers ───────────────────────────────────────────
    { name = "hostname, time zone and locale come from install.json";
      ok = comp.networking.hostName == "surface" && comp.time.timeZone == "Europe/Berlin"
        && comp.i18n.defaultLocale == "de_DE.UTF-8"
        && comp.i18n.extraLocaleSettings.LC_TIME == "en_GB.UTF-8"; }
    { name = "keyboard: xkb layout/variant set, console derived from it when no keymap was named";
      ok = comp.services.xserver.xkb.layout == "de" && comp.services.xserver.xkb.variant == "nodeadkeys"
        && comp.console.useXkbConfig; }
    { name = "keyboard: a named console keymap is used as given";
      ok = audio.console.keyMap == "us" && !audio.console.useXkbConfig; }
    { name = "the user is created, in wheel and the profile's groups";
      ok = comp.users.users.asher.isNormalUser
        && lib.all (g: lib.elem g comp.users.users.asher.extraGroups) [ "wheel" "networkmanager" "audio" "jackaudio" "realtime" ]
        && lib.all (g: lib.elem g audio.users.users.asher.extraGroups) [ "wheel" "audio" "video" ]; }
    { name = "UEFI: systemd-boot, not GRUB";
      ok = comp.boot.loader.systemd-boot.enable && !comp.boot.loader.grub.enable; }
    { name = "BIOS: GRUB on the named disk, cryptodisk for an encrypted root";
      ok = audio.boot.loader.grub.enable && audio.boot.loader.grub.device == "/dev/sda"
        && audio.boot.loader.grub.enableCryptodisk && !audio.boot.loader.systemd-boot.enable; }
    { name = "LUKS: encrypted swap declared and every LUKS device gets the keyfile";
      ok = audio.boot.initrd.luks.devices.luks-swap.device == "/dev/disk/by-uuid/22222222-0000-4000-8000-000000000002"
        && audio.boot.initrd.luks.devices.luks-root.keyFile == "/boot/crypto_keyfile.bin"
        && audio.boot.initrd.luks.devices.luks-root.device == "/dev/disk/by-uuid/11111111-0000-4000-8000-000000000001"
        && audio.boot.initrd.secrets ? "/boot/crypto_keyfile.bin"; }
    { name = "autologin: desktop profiles via the display manager, headless ones not at all unless asked";
      ok = audio.services.displayManager.autoLogin.enable && audio.services.displayManager.autoLogin.user == "asher"
        && comp.services.getty.autologinUser == null; }

    # ── the profile is what was chosen ────────────────────────────────────
    { name = "audio profile: Plasma 6 and the audio stack";
      ok = audio.services.desktopManager.plasma6.enable && audio.services.pipewire.enable; }
    { name = "companion profile: no desktop, no PipeWire, zram, earlyoom";
      ok = !comp.services.desktopManager.plasma6.enable && !comp.services.xserver.enable
        && !comp.services.pipewire.enable && comp.zramSwap.enable && comp.services.earlyoom.enable; }
    { name = "companion: JACK runs as the installed user, under rt-exec, floating (no isolcpus)";
      ok = comp.archibald.companion.user == "asher"
        && comp.systemd.services.jack2-alsa.serviceConfig.User == "asher"
        && lib.hasInfix "/bin/rt-exec --cpu any " (jack comp)
        && !lib.any (lib.hasPrefix "isolcpus") comp.boot.kernelParams
        && !lib.any (lib.hasPrefix "intel_idle.max_cstate") comp.boot.kernelParams; }
    { name = "companion: dsp-ctl's `vm start|stop` unit exists and pulls JACK in";
      ok = lib.elem "jack2-alsa.service" comp.systemd.services.archibaldos-dsp.requires
        && lib.elem "archibaldos-dsp.service" comp.systemd.services.jack2-alsa.partOf; }
    { name = "companion: sudo allows exactly dsp-ctl's six systemctl commands, without a password";
      ok = lib.sort lib.lessThan (sudoCmds comp) == lib.sort lib.lessThan (lib.concatMap
        (u: map (v: "/run/current-system/sw/bin/systemctl ${v} ${u}") [ "start" "stop" "restart" ])
        [ "archibaldos-dsp.service" "demod-rt.service" ]); }

    # ── enrolment ─────────────────────────────────────────────────────────
    { name = "not enrolled: password ssh for the user, no root login, bridge off, and a warning";
      ok = comp.services.openssh.settings.PasswordAuthentication
        && comp.services.openssh.settings.PermitRootLogin == "no"
        && !comp.archibald.dsp.control.enable
        && lib.any (lib.hasInfix "oligarchy-companion enroll") comp.warnings; }
    { name = "enrolled: keys only, root key-only with the commander's key";
      ok = !enr.services.openssh.settings.PasswordAuthentication
        && enr.services.openssh.settings.PermitRootLogin == "prohibit-password"
        && enr.users.users.root.openssh.authorizedKeys.keys == enr.archibald.companion.commander.sshKeys
        && enr.users.users.asher.openssh.authorizedKeys.keys == enr.archibald.companion.commander.sshKeys; }
    { name = "enrolled: the control bridge admits the commander's tunnel address only, on the tunnel";
      ok = enr.archibald.dsp.control.enable && enr.archibald.dsp.control.allowFrom == "10.77.0.1/32"
        && lib.hasInfix "range=10.77.0.1/32" enr.systemd.services.dsp-control-bridge.serviceConfig.ExecStart
        && enr.networking.firewall.interfaces.wg-oligarchy.allowedTCPPorts == [ 7777 ]
        && !lib.elem 7777 enr.networking.firewall.allowedTCPPorts; }
    { name = "enrolled: WireGuard dials the commander and routes only its address";
      ok = let w = enr.networking.wireguard.interfaces.wg-oligarchy; p = lib.head w.peers; in
        w.ips == [ "10.77.0.2/24" ] && w.generatePrivateKeyFile
        && p.endpoint == "192.168.1.10:51877" && p.allowedIPs == [ "10.77.0.1/32" ]; }

    # ── Surface ───────────────────────────────────────────────────────────
    { name = "companion-surface: the linux-surface kernel replaces CachyOS";
      ok = surf.boot.kernelPackages.kernel.version != comp.boot.kernelPackages.kernel.version
        && !(comp.boot.kernelPackages.kernel.passthru ? surface)   # sanity: comparing two kernels
        && surf.services.iptsd.enable && surf.services.thermald.enable; }

    # ── refusals ──────────────────────────────────────────────────────────
    { name = "an install.json schema this tree does not read fails an assertion";
      ok = lib.any (lib.hasInfix "schema 2") (failedAssertions (cfg "bad-schema")); }
    { name = "every fixture evaluates without failed assertions (bad-schema aside)";
      ok = lib.all (c: failedAssertions c == [ ]) [ comp enr surf audio ]; }
  ];

  failed = lib.filter (x: !x.ok) checks;
  report = lib.concatMapStringsSep "\n" (x: (if x.ok then "PASS: " else "FAIL: ") + x.name) checks;
in
pkgs.runCommand "installed-contract" { inherit report; passAsFile = [ "report" ]; } ''
  cat "$reportPath"; echo
  echo "${toString (lib.length checks - lib.length failed)}/${toString (lib.length checks)} checks passed"
  ${if failed == [ ] then ''cp "$reportPath" $out'' else "exit 1"}
''
