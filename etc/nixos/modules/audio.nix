{ config, pkgs, ... }:

{
  # PipeWire as primary audio server (replaces PulseAudio, supports JACK/ALSA)
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    jack.enable = true;
    pulse.enable = true;  # For compatibility
    wireplumber.enable = true;
  };

  # Disable PulseAudio to avoid conflicts
  hardware.pulseaudio.enable = false;

  # RT privileges for audio users
  users.groups.realtime = {};
}
