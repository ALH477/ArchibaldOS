{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./modules/packages.nix
    ./modules/audio.nix
    ./modules/kde.nix
    ./modules/hyprland.nix
  ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Musnix for RT audio (default kernel)
  musnix = {
    enable = true;
    kernel.realtime = true;  # Uses linuxPackages_rt
    rtirq.enable = true;  # IRQ priority for audio
    das_watchdog.enable = true;  # Watchdog for stability
  };

  # Low-latency optimizations (from ArchWiki best practices, aligned with NixOS manual)
  boot.kernel.sysctl = {
    "vm.swappiness" = 10;
    "fs.inotify.max_user_watches" = 600000;
    "dev.rtc.max-user-freq" = 2048;
    "dev.hpet.max-user-freq" = 2048;
  };

  # Specialisations for kernel switching (per NixOS manual: https://nixos.org/manual/nixos/stable/index.html#sec-configuration-specialisation)
  # Default: RT kernel for audio production
  # lts-backup: LTS kernel as failure backup (disables Musnix RT for stability)
  specialisation = {
    lts-backup.configuration = {
      musnix.enable = false;  # Disable RT features for LTS compatibility
      boot.kernelPackages = pkgs.linuxPackages_lts;  # LTS kernel (e.g., 6.6 as of July 2025)
      # Caveat: Ensure hardware modules (e.g., from hardware-configuration.nix) are compatible
    };
  };

  # Networking
  networking.networkmanager.enable = true;
  networking.firewall.enable = true;

  # Timezone/Locale
  time.timeZone = "UTC";
  i18n.defaultLocale = "en_US.UTF-8";

  # Users
  users.users.youruser = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "realtime" "networkmanager" ];
    shell = pkgs.zsh;
  };

  # Security
  security.rtkit.enable = true;
  security.sudo.wheelNeedsPassword = false;  # Optional

  # Nix settings
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.optimise.automatic = true;

  # System version
  system.stateVersion = "25.05";  # Aligned with latest stable as of July 2025
}
