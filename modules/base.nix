{ config, pkgs, lib, ... }:

{
  # Base system configuration for all variants
  
  # Locale & Timezone
  time.timeZone = lib.mkDefault "UTC";
  i18n.defaultLocale = "en_US.UTF-8";
  
  console = {
    font = "Lat2-Terminus16";
    keyMap = lib.mkDefault "us";
  };

  # Power management
  powerManagement.enable = true;

  # SSH for remote management
  services.openssh = {
    enable = lib.mkDefault true;
    settings = {
      PasswordAuthentication = lib.mkDefault false;
      PermitRootLogin = "no";
    };
  };

  # Basic system packages
  environment.systemPackages = with pkgs; [
    vim wget curl git htop
    usbutils pciutils
  ];

  # Nix settings
  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    auto-optimise-store = true;
  };

  # Automatic garbage collection
  nix.gc = {
    automatic = lib.mkDefault true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
}
