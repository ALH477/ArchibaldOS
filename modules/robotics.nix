# modules/robotics.nix

# Copyright 2025 DeMoD LLC
# SPDX-License-Identifier: BSD-3-Clause

{ config, pkgs, lib, ... }:

let
  cfg = config.archibaldOS.robotics;

  # ROS 2 Humble (officially supported on Ubuntu 22.04, works well on NixOS)
  ros2Humble = pkgs.rosPackages.humble;

  # PX4 Autopilot (SITL + firmware tools)
  px4 = pkgs.px4;

  # Core robotics packages (ARM-safe)
  baseRoboticsPackages = with pkgs; [
    # ROS 2 Core
    ros2Humble.ros-core
    ros2Humble.ros-base
    ros2Humble.rclcpp
    ros2Humble.rclpy
    ros2Humble.geometry-msgs
    ros2Humble.sensor-msgs
    ros2Humble.nav-msgs
    ros2Humble.tf2-ros
    ros2Humble.robot-state-publisher
    ros2Humble.joint-state-publisher
    ros2Humble.rviz2
    ros2Humble.ros2-launch
    ros2Humble.ros2-topic
    ros2Humble.ros2-node
    ros2Humble.ros2-service
    ros2Humble.ros2-param
    ros2Humble.ros2-bag

    # PX4 Autopilot
    px4
    px4-sitl
    px4-firmware
    px4-tools
    jmavsim  # Java-based simulator for PX4

    # Communication & Protocols
    mavlink
    mavros
    mavproxy

    # Vision & Perception
    opencv
    pcl
    apriltag

    # Hardware Interfaces
    i2c-tools
    libgpiod
    minicom
    screen
    python3Packages.pyserial

    # Utilities
    tmux
    htop
    usbutils
    pciutils
  ];

  # x86_64-only: heavy simulation & GUI tools
  x86RoboticsPackages = with pkgs; lib.optionals pkgs.stdenv.hostPlatform.isx86_64 [
    # Full desktop ROS tools
    ros2Humble.desktop
    ros2Humble.rviz-default-plugins
    ros2Humble.robot-localization
    ros2Humble.slam-toolbox
    ros2Humble.navigation2
    ros2Humble.nav2-bringup

    # Gazebo (Ignition) with ROS integration
    gazebo
    ros2Humble.gazebo-ros-pkgs
    ignition.gazebo6

    # 3D Modeling & Simulation
    blender
    freecad
  ];

  # ARM-embedded extras (lightweight)
  embeddedRoboticsPackages = with pkgs; [
    micropython
    platformio
    esptool
    arduino-cli
  ];

  # Final package selection
  roboticsPackages = baseRoboticsPackages
    ++ lib.optionals pkgs.stdenv.hostPlatform.isx86_64 x86RoboticsPackages
    ++ lib.optionals (cfg.variant == "embedded") embeddedRoboticsPackages;

in {
  options.archibaldOS.robotics = {
    enable = lib.mkEnableOption "Enable robotics module with ROS 2 Humble and PX4 support";

    variant = lib.mkOption {
      type = lib.types.enum [ "basic" "simulation" "embedded" ];
      default = "basic";
      description = ''
        Robotics configuration variant:
        - basic: Core ROS 2 + PX4 for general use.
        - simulation: Full desktop + Gazebo (x86_64 only).
        - embedded: Lightweight for SBCs and drones.
      '';
    };

    px4.sitl = {
      enable = lib.mkEnableOption "Enable PX4 SITL (Software-in-the-Loop) simulation";
      vehicle = lib.mkOption {
        type = lib.types.str;
        default = "iris";
        description = "PX4 vehicle type for SITL (iris, plane, rover, etc.)";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = if cfg.variant == "simulation" then config.nixpkgs.system == "x86_64-linux" else true;
        message = "Robotics simulation variant is only supported on x86_64-linux.";
      }
    ];

    # === Environment Setup ===
    environment.systemPackages = roboticsPackages;

    # ROS 2 Environment Variables
    environment.variables = {
      ROS_DOMAIN_ID = "42";
      RMW_IMPLEMENTATION = "rmw_cyclonedds_cpp";
      CYCLONEDDS_URI = "${pkgs.writeText "cyclonedds.xml" ''
        <?xml version="1.0" encoding="UTF-8" ?>
        <CycloneDDS xmlns="https://cdds.io/config" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="https://cdds.io/config https://raw.githubusercontent.com/ros2/rmw_cyclonedds/master/cyclonedds.xsd">
          <Domain id="any">
            <General>
              <NetworkInterfaceAddress>lo</NetworkInterfaceAddress>
              <AllowMulticast>false</AllowMulticast>
            </General>
            <Discovery>
              <ParticipantIndex>auto</ParticipantIndex>
              <Peers>
                <Peer address="localhost"/>
              </Peers>
            </Discovery>
          </Domain>
        </CycloneDDS>
      ''}";
    };

    # === Hardware & Kernel ===
    hardware.enableRedistributableFirmware = true;
    boot.kernelModules = [ "uinput" "snd_usb_audio" ];
    boot.kernelParams = [
      "threadirqs"
      "isolcpus=1-3"
      "nohz_full=1-3"
      "rcu_nocbs=1-3"
    ] ++ lib.optionals pkgs.stdenv.hostPlatform.isAarch64 [
      "cma=256M"
      "coherent_pool=1M"
    ];

    powerManagement.cpuFreqGovernor = "performance";

    # === Real-time & Permissions ===
    security.rtkit.enable = true;
    security.pam.loginLimits = [
      { domain = "@robotics"; item = "rtprio"; type = "-"; value = "99"; }
      { domain = "@robotics"; item = "memlock"; type = "-"; value = "unlimited"; }
      { domain = "@robotics"; item = "nice";   type = "-"; value = "-19"; }
    ];

    users.groups.robotics = {};
    users.groups.dialout = {};  # For serial ports

    # === udev Rules for Robotics Hardware ===
    services.udev.extraRules = ''
      # FTDI USB-Serial (common for Pixhawk, ArduPilot)
      SUBSYSTEM=="usb", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6015", MODE="0666", GROUP="dialout"

      # CH340/CH341 USB-Serial
      SUBSYSTEM=="usb", ATTRS{idVendor}=="1a86", ATTRS{idProduct}=="7523", MODE="0666", GROUP="dialout"

      # CP210x USB-Serial
      SUBSYSTEM=="usb", ATTRS{idVendor}=="10c4", ATTRS{idProduct}=="ea60", MODE="0666", GROUP="dialout"

      # PX4 / Pixhawk
      KERNEL=="ttyACM*", ATTRS{idVendor}=="26ac", MODE="0666", GROUP="dialout"

      # I2C devices
      KERNEL=="i2c-[0-9]*", GROUP="i2c", MODE="0666"

      # GPIO access
      SUBSYSTEM=="gpio", KERNEL=="gpiochip*", GROUP="robotics", MODE="0660"
    '';

    # === PX4 SITL Service (optional) ===
    systemd.services.px4-sitl = lib.mkIf cfg.px4.sitl.enable {
      description = "PX4 Software-in-the-Loop Simulator";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      environment = {
        PX4_HOME_LAT = "47.641468";
        PX4_HOME_LON = "-122.140165";
        PX4_HOME_ALT = "10.0";
      };
      serviceConfig = {
        ExecStart = "${px4}/bin/px4 -i 0 -d -s ${px4}/etc/init.d-posix/rcS -w sitl_${cfg.px4.sitl.vehicle} ${cfg.px4.sitl.vehicle}";
        WorkingDirectory = "/tmp";
        Restart = "always";
        User = "root";
        StandardOutput = "journal";
        StandardError = "journal";
      };
    };

    # === JMavSim (Java MAVLink Simulator) ===
    environment.systemPackages = lib.mkIf cfg.px4.sitl.enable [ pkgs.jmavsim ];

    # === Sysctl Tuning ===
    boot.kernel.sysctl = {
      "fs.inotify.max_user_watches" = 524288;
      "vm.swappiness" = 0;
      "kernel.sched_rt_runtime_us" = 950000;
    };

    # === User Integration ===
    users.users.audio-user = lib.mkIf config.users.users ? audio-user {
      extraGroups = [ "robotics" "dialout" "i2c" ];
    };

    users.users.nixos = lib.mkIf config.users.users ? nixos {
      extraGroups = [ "robotics" "dialout" ];
    };

    # === ROS 2 Colcon Build Tools (for workspace development) ===
    environment.systemPackages = with pkgs; [
      colcon
      rosdep
      vcsTool
    ];

    # === Optional: Auto-source ROS 2 setup in bash ===
    programs.bash.interactiveShellInit = ''
      if [ -z "$ROS_DISTRO" ]; then
        source ${ros2Humble}/setup.bash 2>/dev/null || true
      fi
    '';
  };
}
