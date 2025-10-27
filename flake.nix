{
  description = "ArchibaldOS Determinate NixOS flake for MIDI and real-time audio FOSS tools with Musnix RT kernel";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    musnix.url = "github:musnix/musnix";
  };

  outputs = { self, nixpkgs, musnix }: let
    system = "x86_64-linux";  # Adjust for your architecture (e.g., "aarch64-linux")
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;  # Required for some tools like Vital
    };

    # Separate package list for modularity
    audioPackages = with pkgs; [
      # DAWs
      ardour lmms zrythm stargate audacity mixxx

      # MIDI Tools
      musescore fluidsynth

      # Audio Processing Libraries/Tools
      portaudio rtaudio faust juce csound supercollider

      # Synths
      amsynth dexed surge vital vcvrack sfizz

      # Effects (examples; expand as needed)
      dragonfly-reverb

      # Modular & Node-Based
      pd plugdata cardinal

      # Other
      obs-studio
    ];
  in {
    nixosConfigurations = {
      audio-workstation = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          musnix.nixosModules.musnix
          ({ config, pkgs, ... }: {
            # System details: Musnix and RT kernel configuration
            musnix.enable = true;
            musnix.kernel.realtime = true;  # Applies CONFIG_PREEMPT_RT patch
            musnix.kernel.packages = pkgs.linuxPackages_latest_rt;  # Or specify version, e.g., pkgs.linuxPackages_6_11_rt
            musnix.alsaSeq.enable = true;  # For MIDI sequencing
            musnix.ffado.enable = false;   # Enable if using FireWire audio
            musnix.rtcqs.enable = true;    # Installs rtcqs for system checks
            musnix.soundcardPciId = "";    # Set to your soundcard PCI ID if applicable (e.g., "00:1b.0")
            musnix.das_watchdog.enable = true;  # Prevents RT process hangs
            musnix.rtirq.enable = true;    # Manages real-time IRQ priorities

            # Audio configuration with PipeWire for low-latency
            sound.enable = true;
            hardware.pulseaudio.enable = false;
            services.pipewire = {
              enable = true;
              alsa.enable = true;
              alsa.support32Bit = true;
              pulse.enable = true;
              jack.enable = true;  # JACK support for pro audio
            };

            # Security and user setup for audio
            security.rtkit.enable = true;  # For real-time priorities
            users.users.yourusername = {  # Replace with your actual username
              isNormalUser = true;
              extraGroups = [ "audio" "jackaudio" ];
            };

            # Boot/kernel params for RT audio (optional tweaks)
            boot.kernelParams = [ "threadirqs" ];  # Enables threaded IRQs for RT

            # Install separated packages
            environment.systemPackages = audioPackages;
          })
        ];
      };
    };

    # Dev shell using the separated package list
    devShells.${system}.default = pkgs.mkShell {
      packages = audioPackages;
    };
  };
}
