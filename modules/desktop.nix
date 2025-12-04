{ config, pkgs, ... }: let
  basicPackages = with pkgs; [
    vim kitty wireplumber cava playerctl
    jetbrains-mono
    noto-fonts-emoji
  ];
in {
  services.xserver.enable = true;
  services.desktopManager.plasma6.enable = true;
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;  # Enable Wayland for optimal Plasma experience
  services.displayManager.defaultSession = "plasma";

  services.displayManager.sddm.settings = {
    General.Background = "/usr/share/wallpapers/ArchibaldOS/demod-wallpaper.jpg";
  };

  environment.etc."polybar/cava.sh" = {
    source = ./cava.sh;
    mode = "0755";
  };

  environment.etc."live-audio-test.sh" = {
    source = ./live-audio-test.sh;
    mode = "0755";
  };

  environment.systemPackages = basicPackages;
}
