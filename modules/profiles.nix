# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 DeMoD LLC. All rights reserved.
# Profile selector module
{ config, lib, pkgs, ... }:

with lib;

{
  options.profiles = {
    # ========================================================================
    # AUDIO PROFILE
    # ========================================================================
    audio = {
      enable = mkEnableOption "Audio production profile";

      latency = mkOption {
        type = types.enum [ "low" "ultra-low" ];
        default = "ultra-low";
        description = "Audio latency setting";
      };
    };

    # ========================================================================
    # ROBOTICS PROFILE
    # ========================================================================
    robotics = {
      enable = mkEnableOption "Robotics and control systems profile";

      ros = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = "Enable ROS 2 support (when available)";
        };
      };

      simulation = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = "Enable simulation tools (Gazebo, etc.)";
        };
      };

      hardware = {
        arduino = mkOption {
          type = types.bool;
          default = true;
          description = ''
            udev access for microcontroller boards and USB-serial adapters
            (Arduino, CH340, FTDI, CP210x, STM32 DFU, Teensy, any ttyUSB/ttyACM),
            granted to the `dialout` / `plugdev` groups — mode 0660, not 0666.
          '';
        };

        canbus = mkOption {
          type = types.bool;
          default = true;
          description = "Load the SocketCAN modules (can, can_raw, can_bcm, vcan, slcan).";
        };

        gpio = mkOption {
          type = types.bool;
          default = true;
          description = "Enable GPIO/I2C/SPI support";
        };
      };
    };

    # ========================================================================
    # NETWORKING PROFILE
    # ========================================================================
    networking = {
      enable = mkEnableOption "HydraMesh networking profile";

      mode = mkOption {
        type = types.enum [ "p2p" "client" "server" ];
        default = "p2p";
        description = "HydraMesh network mode";
      };
    };
  };

  config = mkMerge [
    # Audio profile configuration
    (mkIf config.profiles.audio.enable {
      # Audio groups
      users.groups.audio = {};
      users.groups.jackaudio = {};
      users.groups.realtime = {};

      # RT limits
      security.pam.loginLimits = [
        { domain = "@audio"; type = "-"; item = "rtprio"; value = "99"; }
        { domain = "@audio"; type = "-"; item = "memlock"; value = "unlimited"; }
        { domain = "@audio"; type = "-"; item = "nice"; value = "-19"; }
      ];
    })

    # Robotics profile configuration
    (mkIf config.profiles.robotics.enable {
      # Robotics groups
      users.groups.dialout = {};
      users.groups.plugdev = {};
      users.groups.gpio = {};
      users.groups.i2c = {};
      users.groups.spi = {};

      # RT limits for control loops
      security.pam.loginLimits = [
        { domain = "@realtime"; type = "-"; item = "rtprio"; value = "95"; }
        { domain = "@realtime"; type = "-"; item = "memlock"; value = "unlimited"; }
        { domain = "@realtime"; type = "-"; item = "nice"; value = "-15"; }
      ];

      users.groups.realtime = {};

      # Enable I2C
      hardware.i2c.enable = mkIf config.profiles.robotics.hardware.gpio true;

      # Board access goes to a group, never to everyone. These rules used to
      # live in flake.nix, duplicated per ISO, with MODE="0666" next to the
      # GROUP= — so the group was decorative: any local account, including a
      # service user, could write to an attached motor controller or flash a
      # board. The live user is in dialout and plugdev; add others explicitly.
      # (These options were declared before and wired to nothing.)
      services.udev.extraRules = mkIf config.profiles.robotics.hardware.arduino ''
        # Arduino, CH340, FTDI, Silicon Labs CP210x
        SUBSYSTEM=="tty", ATTRS{idVendor}=="2341", MODE="0660", GROUP="dialout"
        SUBSYSTEM=="tty", ATTRS{idVendor}=="1a86", MODE="0660", GROUP="dialout"
        SUBSYSTEM=="tty", ATTRS{idVendor}=="0403", MODE="0660", GROUP="dialout"
        SUBSYSTEM=="tty", ATTRS{idVendor}=="10c4", MODE="0660", GROUP="dialout"

        # STM32 (DFU), Teensy
        SUBSYSTEM=="usb", ATTRS{idVendor}=="0483", MODE="0660", GROUP="plugdev"
        SUBSYSTEM=="usb", ATTRS{idVendor}=="16c0", MODE="0660", GROUP="plugdev"

        # Generic USB serial
        KERNEL=="ttyUSB*", MODE="0660", GROUP="dialout"
        KERNEL=="ttyACM*", MODE="0660", GROUP="dialout"
      '';

      boot.kernelModules = mkIf config.profiles.robotics.hardware.canbus
        [ "can" "can_raw" "can_bcm" "vcan" "slcan" ];
    })

    # Networking profile configuration
    (mkIf config.profiles.networking.enable {
      services.hydramesh = {
        enable = true;
        mode = config.profiles.networking.mode;
        hardened = true;
      };
    })
  ];
}
