# modules/ArchibaldVM/modules/audio.nix
{ config, pkgs, lib, ... }:

{
  musnix.enable = true;
  musnix.kernel.realtime = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    jack.enable = true;
    extraConfig.pipewire."92-low-latency" = {
      "context.properties" = {
        "default.clock.rate" = 48000;
        "default.clock.quantum" = 32;
      };
    };
  };

  security.rtkit.enable = true;

  boot.kernelParams = [
    "threadirqs"
    "isolcpus=1"
    "nohz_full=1"
  ];

  boot.kernel.sysctl = {
    "vm.swappiness" = 0;
  };

  powerManagement.cpuFreqGovernor = "performance";
}
