# modules/ArchibaldVM/modules/users.nix
{ config, pkgs, lib, ... }:

{
  users.users.audio-user = {
    isNormalUser = true;
    initialPassword = "archibaldos";
    extraGroups = [ "wheel" "audio" "jackaudio" "realtime" ];
    shell = pkgs.bash;
  };

  security.sudo.wheelNeedsPassword = false;
}
