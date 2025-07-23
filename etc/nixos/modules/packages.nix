{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Core/Utils
    vim git networkmanager alsa-utils htop btop neofetch ncdu inxi hardinfo unclutter
    fastfetch duf fd findutils coreutils curl sqlite bash zsh p7zip flatpak

    # Audio Production (FOSS tools/drivers)
    ardour audacity musescore lmms zynaddsubfx helm surge-xt lsp-plugins dragonfly-reverb
    pipewire jack2 qjackctl a2jmidid cadence carla fluidsynth timidity wildmidi osc2midi
    jack_capture guitarix faust amsynth calf projectm csound audacious ffmpeg yt-dlp
    vlc flac speexdsp libffado realtimeconfigquickscan  # RT scan tool

    # Desktop/Media
    floorp thunderbird libreoffice thunar xarchiver filezilla kate appimage-run  # For AppImages
    chromium bitwarden

    # Hardware/Firmware
    amd-ucode intel-ucode linux-firmware sof-firmware nvme-cli smartmontools hdparm
    ethtool usbutils lsscsi sdparm tpm2-tools i2c-tools

    # Dev/Tools
    rust python gcc clang cmake cpupower

    # Security/Network
    ufw gufw openssh nmap tcpdump wpa_supplicant iwd bind dnsmasq rsync cryptsetup
    timeshift sudo

    # Misc (from list)
    thunar ffmpeg ufw yt-dlp realtime-privileges libffado xarchiver
  ];

  # Flatpak support (lean way for extra apps)
  services.flatpak.enable = true;
}
