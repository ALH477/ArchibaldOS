{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix  # Generate with nixos-generate-config
    ./modules/packages.nix        # Custom packages module
    ./modules/audio.nix           # Audio optimizations
    ./modules/kde.nix             # KDE DE
    ./modules/hyprland.nix        # Hyprland (optional compositor)
  ];

  # Bootloader (systemd-boot for EFI, lean alternative to GRUB)
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Kernel: CachyOS RT-BORE for real-time audio
  boot.kernelPackages = pkgs.linuxPackages_cachyos-rt-bore;
  boot.kernelParams = [ "preempt=full" "threadirqs" ];  # RT optimizations

  # Networking (lean: NetworkManager)
  networking.networkmanager.enable = true;
  networking.firewall.enable = true;  # UFW alternative, but enable ufw if preferred

  # Timezone/Locale (adjust as needed)
  time.timeZone = "UTC";
  i18n.defaultLocale = "en_US.UTF-8";

  # Users: Add your user with audio/realtime groups
  users.users.youruser = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "realtime" "networkmanager" ];
    shell = pkgs.zsh;  # From packages
  };

  # Security: RTKit for real-time audio privileges
  security.rtkit.enable = true;
  security.sudo.wheelNeedsPassword = false;  # Optional for lean workflow

  # Nix settings: Flakes, auto-optimize for maintainability
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.optimise.automatic = true;

  # System version (update to latest stable/unstable)
  system.stateVersion = "24.05";
}
