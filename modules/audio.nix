{ config, pkgs, ... }:

{
  # PipeWire as primary (low-latency config)
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    jack.enable = true;
    pulse.enable = true;
    wireplumber.enable = true;
    extraConfig.pipewire."92-low-latency" = {
      context.properties = {
        default.clock.rate = 48000;
        default.clock.quantum = 128;  # Low-latency quantum
        default.clock.min-quantum = 32;
        default.clock.max-quantum = 1024;
      };
    };
  };

  # Disable PulseAudio
  hardware.pulseaudio.enable = false;

  # RT groups (handled by Musnix, but explicit for clarity)
  users.groups.realtime = {};
}
