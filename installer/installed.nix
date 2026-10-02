# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# install.json -> NixOS options, for a system the installer put on a disk.
#
# The installer (installer/calamares/distroinstall/main.py, or the
# `archibaldos-install` CLI) writes hosts/installed/install.json from what the
# user chose; flake.nix's `mkInstalled` reads it, takes the chosen profile's
# modules and adds this module plus the hardware scan. Keeping the mapping in
# Nix, not in the installer's Python, is what lets `checks.installed-contract`
# test it against fixtures.
#
# Only what Calamares (or the CLI) asked about is set here; everything else is
# the profile's. Edit hosts/installed/local.nix for anything personal: it is
# imported when present and is never written by the installer.
{ install }:
{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkMerge elem optionalAttrs listToAttrs nameValuePair;
  u = install.user or null;
  kb = install.keyboard or null;
  loc = install.locale or null;
  boot = install.boot;
  luks = install.luks or { swap = [ ]; keyFile = [ ]; };
  efi = boot.firmware == "efi";

  # Groups the profile's own tools need, on top of wheel + networkmanager.
  groupsFor = {
    audio = [ "audio" "jackaudio" "realtime" "video" ];
    audio-musnix = [ "audio" "jackaudio" "realtime" "video" ];
    robotics = [ "video" "realtime" "dialout" "plugdev" "input" "gpio" "i2c" "spi" ];
    robotics-musnix = [ "video" "realtime" "dialout" "plugdev" "input" "gpio" "i2c" "spi" ];
    companion = [ "audio" "jackaudio" "realtime" ];
    companion-surface = [ "audio" "jackaudio" "realtime" ];
    hydramesh = [ ];
  };
  desktop = elem install.profile [ "audio" "robotics" "audio-musnix" "robotics-musnix" ];
in
{
  assertions = [{
    assertion = (install.schema or null) == 1;
    message = "hosts/installed/install.json has schema ${builtins.toJSON (install.schema or null)}; this tree reads schema 1.";
  }];

  networking.hostName = install.hostname;
  time.timeZone = mkIf ((install.timeZone or null) != null) install.timeZone;

  i18n = mkIf (loc != null) {
    defaultLocale = mkIf (loc ? LANG) loc.LANG;
    extraLocaleSettings = removeAttrs loc [ "LANG" ];
  };

  services.xserver.xkb = mkIf (kb != null) { layout = kb.layout; variant = kb.variant; };
  # Calamares names a console keymap only sometimes; otherwise derive it from
  # the xkb layout so the TTY (and a LUKS prompt) match the desktop.
  console.keyMap = mkIf (kb != null && (kb.consoleKeyMap or null) != null) kb.consoleKeyMap;
  console.useXkbConfig = mkIf (kb != null && (kb.consoleKeyMap or null) == null) true;

  users.users = mkIf (u != null) {
    ${u.name} = {
      isNormalUser = true;
      description = u.fullName;
      extraGroups = [ "wheel" "networkmanager" ] ++ groupsFor.${install.profile};
    };
  };
  # The password is set by the installer's users step (Calamares) or by
  # `archibaldos-install` (passwd in the new system), never written here.
  services.displayManager.autoLogin = mkIf (u != null && u.autologin && desktop) {
    enable = true;
    user = u.name;
  };
  services.getty.autologinUser = mkIf (u != null && u.autologin && !desktop) u.name;

  boot.loader = if efi then {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  } else {
    grub = {
      enable = true;
      device = boot.device;
      useOSProber = true;
      # Upstream's rule: btrfs subvolumes confuse blkid probing.
      fsIdentifier = mkIf boot.btrfsRoot "provided";
      enableCryptodisk = mkIf boot.grubCryptodisk true;
    };
  };

  # Upstream's LUKS handling, from the same facts: encrypted swap that
  # nixos-generate-config cannot see, and the GRUB-cryptodisk keyfile that
  # spares a second passphrase prompt.
  boot.initrd.secrets = mkIf (luks.keyFile != [ ]) { "/boot/crypto_keyfile.bin" = null; };
  boot.initrd.luks.devices = mkMerge [
    (listToAttrs (map (s: nameValuePair s.name { device = "/dev/disk/by-uuid/${s.uuid}"; }) luks.swap))
    (listToAttrs (map (n: nameValuePair n { keyFile = "/boot/crypto_keyfile.bin"; }) luks.keyFile))
  ];
}
