{ config, pkgs, lib, ... }: let
  wallpaperSrc = ../wallpaper.jpg;
in {
  users.users.audio-user = {
    isNormalUser = true;
    extraGroups = [ "audio" "jackaudio" "video" "wheel" ];
    home = "/home/audio-user";
    createHome = true;
    shell = pkgs.bash;
    packages = [
      (pkgs.runCommand "wallpaper" {} ''
        mkdir -p $out/Pictures
        cp ${wallpaperSrc} $out/Pictures/wall.jpg
      '')
    ];
  };

  users.users.nixos = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "jackaudio" "video" ];
    initialPassword = "nixos";
    shell = pkgs.bash;
  };

  security.sudo.wheelNeedsPassword = false;
}
