{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Core/Utils (enhanced with general additions)
    vim neovim git networkmanager alsa-utils htop btop neofetch ncdu inxi hardinfo unclutter
    fastfetch duf fd findutils coreutils curl sqlite bash zsh p7zip flatpak tmux glances
    zip unzip gnupg  # Added: Session mgmt, archives, security

    # Audio Production (Expanded FOSS tools for 2025)
    ardour audacity musescore lmms zynaddsubfx helm surge-xt lsp-plugins dragonfly-reverb
    pipewire jack2 qjackctl a2jmidid cadence carla fluidsynth timidity wildmidi osc2midi
    jack_capture guitarix faust amsynth calf projectm csound audacious ffmpeg yt-dlp
    vlc flac speexdsp libffado realtimeconfigquickscan
    # Added: Overlooked essentials from 2025 FOSS lists
    sox  # Audio toolkit (conversion, effects)
    hydrogen  # Drum machine/sequencer
    rosegarden  # MIDI/audio sequencer
    qtractor  # Multi-track DAW
    obs-studio  # Screen/audio recording
    kdenlive  # Video/audio editing
    opusTools lame  # Encoders (often overlooked for export)
    lv2 ladspa-plugins dssi-plugins vst-plugins  # Plugin suites
    swh-plugins x42-plugins zam-plugins
    # New FOSS synths/DAWs
    zrythm  # Modern DAW
    vital  # Wavetable synth
    vcv-rack  # Modular synth
    dexed  # FM synth
    cardinal  # Modular synth suite
    octasine  # FM synth
    mixxx  # DJ/mixing software

    # Desktop/Media
    floorp thunderbird libreoffice thunar xarchiver filezilla kate appimage-run
    chromium bitwarden obs-studio kdenlive  # Repeated for category

    # Hardware/Firmware
    amd-ucode intel-ucode linux-firmware sof-firmware nvme-cli smartmontools hdparm
    ethtool usbutils lsscsi sdparm tpm2-tools i2c-tools

    # Dev/Tools
    rust python gcc clang cmake cpupower

    # Security/Network
    ufw gufw openssh nmap tcpdump wpa_supplicant iwd bind dnsmasq rsync cryptsetup
    timeshift sudo

    # Misc
    thunar ffmpeg ufw yt-dlp realtime-privileges libffado xarchiver
  ];

  # Flatpak for extras
  services.flatpak.enable = true;
}
