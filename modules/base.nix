# modules/base.nix
{ config, pkgs, lib, ... }:

{
  time.timeZone = lib.mkDefault "UTC";
  i18n.defaultLocale = "en_US.UTF-8";
  
  console = {
    font = "Lat2-Terminus16";
    keyMap = lib.mkDefault "us";
  };

  powerManagement.enable = true;

  services.openssh = {
    enable = lib.mkDefault true;
    settings = {
      PasswordAuthentication = lib.mkDefault false;
      PermitRootLogin = "no";
    };
  };

  environment.systemPackages = with pkgs; [
    vim wget curl git htop
    usbutils pciutils
  ];

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    auto-optimise-store = true;
  };

  nix.gc = {
    automatic = lib.mkDefault true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
}
