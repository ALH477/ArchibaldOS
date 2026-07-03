# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 DeMoD LLC. All rights reserved.
# Desktop environment configuration (Plasma 6)
{ config, lib, pkgs, ... }:

{
  # X11 and Plasma
  services.xserver.enable = true;
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.desktopManager.plasma6.enable = true;

  # XDG portal for Wayland
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.kdePackages.xdg-desktop-portal-kde ];
  };

  # Fonts
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-color-emoji
    liberation_ttf
    fira-code
  ];

  # Desktop packages + the RT check script
  environment.systemPackages = with pkgs; [
    firefox
    # KDE apps live under kdePackages.* since the Plasma 6 reorganization.
    kdePackages.konsole
    kdePackages.dolphin
    kdePackages.kate
    kdePackages.spectacle
    kdePackages.ark
    kdePackages.okular
    kdePackages.gwenview

    # RT check script
    (writeShellScriptBin "rt-check" ''
      #!/usr/bin/env bash
      echo "=== ArchibaldOS RT Check ==="
      echo "Kernel: $(uname -r)"
      echo "RT: $(cat /sys/kernel/realtime 2>/dev/null || echo 'not available')"
      echo "RT Priority Limit: $(ulimit -r)"
      echo "Memlock Limit: $(ulimit -l)"
      echo ""
      echo "=== PipeWire Status ==="
      systemctl --user status pipewire --no-pager 2>/dev/null || echo "Run as user"
      echo ""
      echo "=== Audio Devices ==="
      aplay -l 2>/dev/null || echo "No playback devices"
    '')
  ];
}
