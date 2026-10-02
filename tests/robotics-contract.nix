# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# checks.robotics-contract — eval-only. Both robotics images:
#   - grant board access to a group, never to everyone (no MODE="0666": the
#     rules used to pair GROUP=dialout with 0666, which made the group moot);
#   - still grant it (the dialout rules are present — otherwise "no 0666"
#     would pass on an image with no rules at all);
#   - and the profile options that used to do nothing now decide it: with
#     hardware.arduino = false the rules are gone, with hardware.canbus = false
#     the SocketCAN modules are.
{ pkgs, configs }:

let
  lib = pkgs.lib;

  forConfig = cname: sys:
    let
      c = sys.config;
      off = (sys.extendModules {
        modules = [{
          profiles.robotics.hardware.arduino = false;
          profiles.robotics.hardware.canbus = false;
        }];
      }).config;
      rules = c.services.udev.extraRules;
    in [
      {
        name = "${cname}: no world-writable device rule (MODE=\"0666\")";
        ok = !lib.hasInfix ''MODE="0666"'' rules;
      }
      {
        name = "${cname}: board access is granted to dialout and plugdev at 0660";
        ok = lib.hasInfix ''KERNEL=="ttyUSB*", MODE="0660", GROUP="dialout"'' rules
          && lib.hasInfix ''ATTRS{idVendor}=="0483", MODE="0660", GROUP="plugdev"'' rules;
      }
      {
        name = "${cname}: SocketCAN modules are loaded";
        ok = lib.elem "can_raw" c.boot.kernelModules;
      }
      {
        name = "${cname}: hardware.arduino = false removes the board rules";
        ok = !lib.hasInfix ''GROUP="dialout"'' off.services.udev.extraRules;
      }
      {
        name = "${cname}: hardware.canbus = false removes the CAN modules";
        ok = !lib.elem "can_raw" off.boot.kernelModules;
      }
    ];

  checks = lib.concatLists (lib.mapAttrsToList forConfig configs);
  failed = lib.filter (x: !x.ok) checks;
  report = lib.concatMapStringsSep "\n" (x: (if x.ok then "PASS: " else "FAIL: ") + x.name) checks;
in
pkgs.runCommand "robotics-contract" { inherit report; passAsFile = [ "report" ]; } ''
  cat "$reportPath"; echo
  echo "${toString (lib.length checks - lib.length failed)}/${toString (lib.length checks)} checks passed"
  ${if checks == [ ] then "echo 'FAIL: no configuration inspected'; exit 1" else ""}
  ${if failed == [ ] then ''cp "$reportPath" $out'' else "exit 1"}
''
