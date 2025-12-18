# modules/profile-selector.nix
# Copyright 2025 DeMoD LLC
# SPDX-License-Identifier: BSD-3-Clause
#
# Profile selector: One-line build switching.
#   archibaldOS.profile = "audio-workstation";
#   archibaldOS.profile = "agentic-ai-workstation";
#   archibaldOS.profile = "neural-amp-studio";

{ config, pkgs, lib, ... }:

let
  cfg = config.archibaldOS.profile;

  # === Profile Definitions ===
  profiles = {
    # ── AUDIO ─────────────────────────────────────────────────────
    "audio-workstation" = {
      description = "Pro Audio Workstation (x86_64)";
      modules = [
        ./modules/base.nix
        ./modules/audio.nix
        ./modules/desktop.nix
        ./modules/users.nix
        ./modules/branding.nix
        ./modules/rt-kernel.nix
      ];
      config = {
        archibaldOS.rtKernel = { enable = true; variant = "cachyos-rt-bore"; };
        services.displayManager.autoLogin = { enable = true; user = "nixos"; };
        system.stateVersion = "24.11";
      };
    };

    "audio-live-iso" = {
      description = "Live Audio ISO (x86_64)";
      modules = [
        "${pkgs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-plasma6.nix"
        ./modules/base.nix
        ./modules/audio.nix
        ./modules/desktop.nix
        ./modules/users.nix
        ./modules/branding.nix
        ./modules/rt-kernel.nix
      ];
      config = {
        archibaldOS.rtKernel = { enable = true; variant = "standard"; };
        isoImage.squashfsCompression = "gzip -Xcompression-level 1";
        users.users.nixos.initialPassword = "nixos";
        services.displayManager.autoLogin = { enable = true; user = "nixos"; };
        system.stateVersion = "24.11";
      };
    };

    # ── AI ────────────────────────────────────────────────────────
    "agentic-ai-workstation" = {
      description = "Agentic Local AI Workstation (x86_64, GPU recommended)";
      modules = [
        ./modules/base.nix
        ./modules/audio.nix  # PipeWire for voice support
        ./modules/desktop.nix
        ./modules/users.nix
        ./modules/branding.nix
        ./modules/rt-kernel.nix
        ./modules/agentic-local-ai.nix  # New AI module
      ];
      config = {
        archibaldOS.rtKernel = { enable = true; variant = "cachyos-rt-bore"; };

        # Agentic AI configuration
        services.agentic-local-ai = {
          enable = true;
          preset = "high-vram";          # Balanced for professional GPUs
          acceleration = "cuda";         # Enable CUDA (set to "rocm" or null if needed)
          enableWebUI = true;
          voice.enable = true;
          multiAgent.enable = true;
          advanced.vllm.enable = true;   # Production inference
          advanced.fineTuning.enable = true;
        };

        services.displayManager.autoLogin = { enable = true; user = "nixos"; };
        system.stateVersion = "24.11";
      };
    };

    # ── NEURAL AMP MODELING ───────────────────────────────────────
    "neural-amp-studio" = {
      description = "Neural Amp Modeling Studio (x86_64 audio + Tone Assistant)";
      modules = [
        ./modules/base.nix
        ./modules/audio.nix
        ./modules/desktop.nix
        ./modules/users.nix
        ./modules/branding.nix
        ./modules/rt-kernel.nix
        (import ../tone-assistant.nix).nixosModules.tone-assistant  # Import the standalone flake's module
      ];
      config = {
        archibaldOS.rtKernel = { enable = true; variant = "cachyos-rt-bore"; };

        # Tone Assistant integration
        programs.tone-assistant = {
          enable = true;
          # package = defaults to the flake-provided emacsWithToneAssistant
        };

        services.displayManager.autoLogin = { enable = true; user = "nixos"; };
        system.stateVersion = "24.11";
      };
    };

    # ── ROBOTICS / DRONES ───────────────────────────────────────
    "drone-brain" = {
      description = "Drone Flight Controller (ARM SBC)";
      modules = [
        ./modules/base.nix
        ./modules/audio.nix
        ./modules/robotics.nix
        ./modules/lidar.nix
        ./modules/users.nix
        ./modules/rt-kernel.nix
      ];
      config = {
        archibaldOS.rtKernel = { enable = true; variant = "standard"; };
        archibaldOS.robotics = { enable = true; variant = "embedded"; px4.sitl.enable = true; };
        archibaldOS.lidar = { enable = true; variant = "embedded"; };
        users.users.audio-user.extraGroups = [ "dialout" "robotics" "lidar" ];
        system.stateVersion = "24.11";
      };
    };

    "lidar-station" = {
      description = "LIDAR Mapping Station (x86_64 or ARM)";
      modules = [
        ./modules/base.nix
        ./modules/lidar.nix
        ./modules/robotics.nix
        ./modules/desktop.nix
        ./modules/users.nix
        ./modules/rt-kernel.nix
      ];
      config = {
        archibaldOS.rtKernel.enable = true;
        archibaldOS.lidar = { enable = true; variant = "simulation"; };
        archibaldOS.robotics.enable = true;
        services.displayManager.autoLogin.enable = true;
        system.stateVersion = "24.11";
      };
    };

    # ── SECURE / DEFENSE ─────────────────────────────────────────
    "secure-router" = {
      description = "ITAR-Safe Secure Router";
      modules = [
        ./modules/base.nix
        ./modules/router.nix
        ./modules/secure-rt.nix
        ./modules/users.nix
      ];
      config = {
        archibaldOS.router = {
          enable = true;
          wanInterfaces = [ "eth0" ];
          lanInterfaces = [ "eth1" ];
          hardening = true;
        };
        archibaldOS.secureRT = { enable = true; mode = "defense"; lockdown = true; };
        system.stateVersion = "24.11";
      };
    };

    "dsp-coprocessor" = {
      description = "DSP Coprocessor (kexec, minimal)";
      modules = [
        ./modules/base.nix
        ./modules/dsp.nix
        ./modules/rt-kernel.nix
        ./modules/secure-rt.nix
      ];
      config = {
        archibaldOS.rtKernel = { enable = true; variant = "cachyos-rt-bore"; };
        archibaldOS.secureRT = { enable = true; mode = "dsp"; };
        system.stateVersion = "24.11";
      };
    };

    # ── CUSTOM ───────────────────────────────────────────────────
    "custom" = {
      description = "Custom profile (use with care)";
      modules = [ ];
      config = { };
    };
  };

in {
  options.archibaldOS.profile = lib.mkOption {
    type = lib.types.nullOr (lib.types.enum (builtins.attrNames profiles));
    default = null;
    description = ''
      Select a predefined system profile.
      Set to one of: ${lib.concatStringsSep ", " (builtins.attrNames profiles)}
      Or null to disable.
    '';
  };

  config = lib.mkIf (cfg != null) (lib.mkMerge [
    # Base assertion
    {
      assertions = [
        {
          assertion = builtins.hasAttr cfg profiles;
          message = "Unknown profile: ${cfg}. Valid: ${lib.concatStringsSep ", " (builtins.attrNames profiles)}";
        }
      ];
    }

    # Apply selected profile
    (profiles.${cfg}.config or { })

    # Inject modules
    {
      imports = profiles.${cfg}.modules or [ ];
    }

    # Optional: Print profile on boot
    {
      system.activationScripts.profile-info = lib.mkIf (cfg != null) ''
        echo "ArchibaldOS Profile: ${cfg} — ${profiles.${cfg}.description}" > /etc/profile-name
      '';
    }
  ]);
}
