#!/usr/bin/env bash

# ArchibaldOS Setup Script
# Automates NixOS config generation with CachyOS kernel options, KDE, and audio tools.

set -e

# Display ASCII art logo
cat << EOF
                      *
                      * *
                     *   *
                    * * * *
                   *       *
                  * *     * *
                 *   *   *   *
                * * * * * * * *
               *               *
              * *             * *
             *   *           *   *
            * * * *         * * * *
           *       *       *       *
          * *     * *     * *     * *
         *   *   *   *   *   *   *   *
        * * * * * * * * * * * * * * * *
EOF

# Default paths
NIX_DIR="/etc/nixos"
HARDWARE_CONFIG="$NIX_DIR/hardware-configuration.nix"

# Prompt for user inputs
read -p "Enter hostname (default: archibald): " HOSTNAME
HOSTNAME=${HOSTNAME:-archibald}

read -p "Enter username: " USERNAME
if [ -z "$USERNAME" ]; then
  echo "Username is required."
  exit 1
fi

read -p "Enter timezone (e.g., UTC, America/New_York): " TIMEZONE
TIMEZONE=${TIMEZONE:-UTC}

echo "Choose kernel:"
echo "1) CachyOS RT-BORE (default, for low-latency audio)"
echo "2) CachyOS LTS (stable performance)"
echo "3) NixOS Native RT (fallback)"
read -p "Selection (1-3): " KERNEL_CHOICE
KERNEL_CHOICE=${KERNEL_CHOICE:-1}

case $KERNEL_CHOICE in
  1) KERNEL_PKG="pkgs.linuxPackages_cachyos-rt-bore"; KERNEL_COMMENT="";;
  2) KERNEL_PKG="pkgs.linuxPackages_cachyos-lts"; KERNEL_COMMENT="# boot.kernelPackages = pkgs.linuxPackages_cachyos-rt-bore; # Uncomment for RT-BORE";;
  3) KERNEL_PKG="pkgs.linuxPackages_rt"; KERNEL_COMMENT="# boot.kernelPackages = pkgs.linuxPackages_cachyos-rt-bore; # Uncomment for RT-BORE";;
  *) echo "Invalid choice. Exiting."; exit 1;;
esac

# Check if hardware config exists; generate if not (assumes installer mode)
if [ ! -f "$HARDWARE_CONFIG" ]; then
  echo "Generating hardware-configuration.nix..."
  nixos-generate-config --root /mnt  # Adjust /mnt if not in installer
fi

# Create directory structure
mkdir -p "$NIX_DIR/modules"

# Write flake.nix
cat << EOF > "$NIX_DIR/flake.nix"
{
  description = "ArchibaldOS: NixOS with CachyOS RT-BORE Kernel, KDE, and Audio Tools";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    chaotic = {
      url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-cachyos-kernel = {
      url = "github:drakon64/nixos-cachyos-kernel";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland.url = "github:hyprwm/Hyprland";
  };

  outputs = { self, nixpkgs, chaotic, nixos-cachyos-kernel, hyprland, ... }: {
    nixosConfigurations.$HOSTNAME = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        { imports = [ chaotic.nixosModules.default ]; }
        nixos-cachyos-kernel.nixosModules.default
      ];
    };
  };
}
EOF

# Write configuration.nix
cat << EOF > "$NIX_DIR/configuration.nix"
{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./modules/packages.nix
    ./modules/audio.nix
    ./modules/kde.nix
    ./modules/hyprland.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = $KERNEL_PKG;
  boot.kernelParams = [ "preempt=full" "threadirqs" ];

  $KERNEL_COMMENT

  networking.networkmanager.enable = true;
  networking.firewall.enable = true;

  time.timeZone = "$TIMEZONE";
  i18n.defaultLocale = "en_US.UTF-8";

  users.users.$USERNAME = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "realtime" "networkmanager" ];
    shell = pkgs.zsh;
  };

  security.rtkit.enable = true;
  security.sudo.wheelNeedsPassword = false;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.optimise.automatic = true;

  system.stateVersion = "24.05";
}
EOF

# Generate modules (stubbed; expand with actual content from previous responses)
echo "{ config, pkgs, ... }: { services.pipewire.enable = true; /* Add full audio config */ }" > "$NIX_DIR/modules/audio.nix"
echo "{ config, pkgs, ... }: { services.desktopManager.plasma6.enable = true; /* Add full KDE config */ }" > "$NIX_DIR/modules/kde.nix"
echo "{ config, pkgs, ... }: { programs.hyprland.enable = true; /* Add full Hyprland config */ }" > "$NIX_DIR/modules/hyprland.nix"

# Packages module (using the provided list, mapped to Nix)
cat << EOF > "$NIX_DIR/modules/packages.nix"
{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    vim git networkmanager alsa-utils htop btop neofetch ncdu inxi hardinfo unclutter
    fastfetch duf fd findutils coreutils curl sqlite bash zsh p7zip flatpak
    ardour audacity musescore lmms zynaddsubfx helm surge-xt lsp-plugins dragonfly-reverb
    pipewire jack2 qjackctl a2jmidid cadence carla fluidsynth timidity wildmidi osc2midi
    jack_capture guitarix faust amsynth calf projectm csound audacious ffmpeg yt-dlp
    vlc flac speexdsp libffado realtimeconfigquickscan
    floorp thunderbird libreoffice thunar xarchiver filezilla kate appimage-run
    chromium bitwarden
    amd-ucode intel-ucode linux-firmware sof-firmware nvme-cli smartmontools hdparm
    ethtool usbutils lsscsi sdparm tpm2-tools i2c-tools
    rust python gcc clang cmake cpupower
    ufw gufw openssh nmap tcpdump wpa_supplicant iwd bind dnsmasq rsync cryptsetup
    timeshift sudo
    thunar ffmpeg ufw yt-dlp realtime-privileges libffado xarchiver
  ];

  services.flatpak.enable = true;
}
EOF

# Generate neofetch config with ASCII art (system-wide or per-user; here as example for user)
mkdir -p /home/$USERNAME/.config/neofetch
cat << EOF > /home/$USERNAME/.config/neofetch/config.conf
print_info() {
  prin "OS" "\${os}"
  prin "Kernel" "\${kernel}"
  # Add other info...
}

ascii_distro="custom"
ascii_colors=(3 2 1)
ascii_bold="off"
ascii="
                       *
                      * *
                     *   *
                    * * * *
                   *       *
                  * *     * *
                 *   *   *   *
                * * * * * * * *
               *               *
              * *             * *
             *   *           *   *
            * * * *         * * * *
           *       *       *       *
          * *     * *     * *     * *
         *   *   *   *   *   *   *   *
        * * * * * * * * * * * * * * * *
"
EOF
chown -R $USERNAME:$USERNAME /home/$USERNAME/.config/neofetch

echo "Setup complete! Files generated in $NIX_DIR."
echo "Neofetch config with ASCII art added to /home/$USERNAME/.config/neofetch/config.conf."
echo "Next steps:"
echo "1. Review and edit files if needed (e.g., add passwords, hardware specifics)."
echo "2. Run: sudo nixos-rebuild switch --flake $NIX_DIR#$HOSTNAME"
echo "3. Reboot."
echo "For installation from ISO: sudo nixos-install --flake $NIX_DIR#$HOSTNAME"
