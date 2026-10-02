# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# checks.dsp-vm-contract — eval-only. The DSP guest as shipped, asserted
# against its evaluated configuration, not against the source text.
#
# Every check here was false on the tree before it was written:
#   - jack2-alsa Required pipewire.service, which NixOS MASKS when PipeWire is
#     not systemWide; demod-rt Required jack2-netjack.service, which nothing
#     defines. Statically, the JACK -> NETJACK -> demod-rt chain could not start.
#   - the control bridge ran as root, on every address, firewall off, and
#     relayed anyone to a socket whose load_fx makes the engine dlopen a path;
#   - sshd took passwords for a wheel user whose password is published;
#   - the image had no ESP, under a host that boots OVMF by default.
# A check that inspects nothing is a FAIL (Oligarchy's rule), hence the first
# one: the units the dependency checks walk must exist.
{ pkgs, dspVm }:

let
  lib = pkgs.lib;

  # The private engine is not an input here, so demod-rt is evaluated with a
  # stub package: what is checked is the unit this module generates.
  withEngine = dspVm.extendModules {
    modules = [
      ({ pkgs, ... }: {
        services.demod-rt = {
          enable = true;
          package = pkgs.writeShellScriptBin "demod-rt" "exit 0";
        };
      })
    ];
  };
  c = dspVm.config;
  e = withEngine.config;

  owned = [ "jack2-alsa" "jack2-netjack-master" "dsp-control-bridge" ];

  # A hard dependency on a .service must name a unit that is defined AND
  # enabled: NixOS turns `enable = false` into a /dev/null mask, and systemd
  # fails a start whose Requires= is masked or missing.
  hardDeps = s: (s.requires or [ ]) ++ (s.bindsTo or [ ]) ++ (s.requisite or [ ]);
  depBroken = cfg: d:
    let n = lib.removeSuffix ".service" d;
    in lib.hasSuffix ".service" d
      && !(cfg.systemd.services ? ${n} && cfg.systemd.services.${n}.enable);
  depsCheck = cfg: name:
    let bad = lib.filter (depBroken cfg) (hardDeps cfg.systemd.services.${name});
    in {
      name = "${name}: every Requires/BindsTo/Requisite service is defined and enabled";
      ok = bad == [ ];
      detail = "missing or masked: ${lib.concatStringsSep " " bad}";
    };

  bridge = c.systemd.services.dsp-control-bridge.serviceConfig;
  engine = e.systemd.services.demod-rt.serviceConfig;
  ctl = c.archibald.dsp.control;
  ssh = c.services.openssh.settings;
  grub = c.boot.loader.grub;

  checks = [
    {
      name = "the units the dependency checks walk exist (else they inspect nothing)";
      ok = lib.all (n: c.systemd.services ? ${n}) owned && e.systemd.services ? demod-rt;
      detail = "expected ${lib.concatStringsSep " " owned} and demod-rt";
    }
  ]
  ++ map (depsCheck c) owned
  ++ [
    (depsCheck e "demod-rt")
    {
      name = "demod-rt runs under rt-exec";
      # The store path of the rt-exec derivation, followed by a space: the old
      # unit exec'd ".../rt-exec-wrapper/bin/rt-exec-wrapper", a script that
      # ran demod-rt directly, and a bare "/bin/rt-exec" infix matched it.
      ok = builtins.match ".*/nix/store/[a-z0-9]{32}-rt-exec/bin/rt-exec .*"
        (lib.replaceStrings [ "\n" ] [ " " ] (engine.ExecStart.text or "")) != null;
      detail = "ExecStart script does not exec the rt-exec package";
    }
    {
      name = "demod-rt keeps NoNewPrivileges (its caps are ambient)";
      ok = engine.NoNewPrivileges == true;
      detail = "NoNewPrivileges = ${toString engine.NoNewPrivileges}";
    }
    {
      name = "control bridge: not root";
      ok = (bridge.User or "root") != "root" && (bridge.User or "") != "";
      detail = "User = ${bridge.User or "<unset: root>"}";
    }
    {
      name = "control bridge: no capabilities, no new privileges";
      ok = bridge.CapabilityBoundingSet == [ "" ] && bridge.NoNewPrivileges == true;
      detail = "CapabilityBoundingSet/NoNewPrivileges";
    }
    {
      name = "control bridge: socat refuses peers outside allowFrom (${ctl.allowFrom})";
      ok = lib.hasInfix "TCP4-LISTEN:${toString ctl.port}," bridge.ExecStart
        && lib.hasInfix ",range=${ctl.allowFrom} " bridge.ExecStart;
      detail = bridge.ExecStart;
    }
    {
      name = "control bridge: systemd IP allowlist repeats it";
      ok = bridge.IPAddressDeny == "any" && bridge.IPAddressAllow == [ ctl.allowFrom ];
      detail = "IPAddressDeny/IPAddressAllow";
    }
    {
      name = "firewall on, control port and NETJACK opened";
      ok = c.networking.firewall.enable
        && lib.elem ctl.port c.networking.firewall.allowedTCPPorts
        && lib.elem 4713 c.networking.firewall.allowedUDPPorts;
      detail = "firewall.enable = ${lib.boolToString c.networking.firewall.enable}";
    }
    {
      name = "sshd: keys only, no root login";
      ok = ssh.PasswordAuthentication == false
        && ssh.KbdInteractiveAuthentication == false
        && ssh.PermitRootLogin == "no";
      detail = "PasswordAuthentication=${lib.boolToString ssh.PasswordAuthentication} "
        + "KbdInteractiveAuthentication=${lib.boolToString ssh.KbdInteractiveAuthentication} "
        + "PermitRootLogin=${ssh.PermitRootLogin}";
    }
    {
      name = "image boots under UEFI as well as BIOS (removable EFI GRUB + ESP at /boot)";
      ok = grub.enable && grub.efiSupport && grub.efiInstallAsRemovable
        && grub.device == "/dev/vda"
        && (c.fileSystems ? "/boot")
        && c.fileSystems."/boot".device == "/dev/disk/by-label/ESP";
      detail = "grub.efiSupport=${lib.boolToString grub.efiSupport} "
        + "efiInstallAsRemovable=${lib.boolToString grub.efiInstallAsRemovable}";
    }
  ];

  failed = lib.filter (x: !x.ok) checks;
  report = lib.concatMapStringsSep "\n"
    (x: (if x.ok then "PASS: " else "FAIL: ") + x.name + lib.optionalString (!x.ok) "  [${x.detail}]")
    checks;
in
pkgs.runCommand "dsp-vm-contract" { inherit report; passAsFile = [ "report" ]; } ''
  cat "$reportPath"; echo
  echo "${toString (lib.length checks - lib.length failed)}/${toString (lib.length checks)} checks passed"
  ${if failed == [ ] then ''cp "$reportPath" $out'' else "exit 1"}
''
