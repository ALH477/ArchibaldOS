# modules/installer.nix
{ config, pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [ dialog disko mkpasswd networkmanager ];

  # Import the real Bash scripts from the modules directory
  environment.etc."installer.sh" = {
    source = ./installer.sh;
    mode = "0755";
  };

  environment.etc."audio-setup.sh" = {
    source = ./audio-setup.sh;
    mode = "0755";
  };
}
