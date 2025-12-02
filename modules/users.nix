{ config, pkgs, lib, ... }: 

{
  # Production audio user (for installed systems)
  users.users.audio-user = {
    isNormalUser = true;
    extraGroups = [ "audio" "jackaudio" "realtime" "video" "wheel" ];
    home = "/home/audio-user";
    createHome = true;
    shell = pkgs.bash;
    # Note: Initial password should be set during installation
  };

  # Live ISO / installer user
  users.users.nixos = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "jackaudio" "video" "networkmanager" ];
    initialPassword = "nixos";
    home = "/home/nixos";
    createHome = true;
  };

  # Passwordless sudo for wheel group
  security.sudo.wheelNeedsPassword = false;
}
