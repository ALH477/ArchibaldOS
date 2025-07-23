{ config, pkgs, ... }:

{
  services.desktopManager.plasma6.enable = true;
  services.displayManager.sddm.enable = true;
  services.xserver.enable = true;  # For X11 fallback if needed

  # Optimize for audio: Low-latency Wayland
  programs.xwayland.enable = true;
}
