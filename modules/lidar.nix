# modules/lidar.nix
# Copyright 2025 DeMoD LLC
# SPDX-License-Identifier: BSD-3-Clause

{ config, pkgs, lib, ... }:

let
  cfg = config.archibaldOS.lidar;

  # ROS 2 Humble LIDAR drivers
  ros2 = pkgs.rosPackages.humble;

  # Core LIDAR packages (ARM-safe)
  baseLidarPackages = with pkgs; [
    # Drivers
    ros2.velodyne
    ros2.ouster-drivers
    ros2.hesai-driver
    ros2.livox-ros-driver2
    ros2.rplidar-ros

    # Point Cloud Processing
    pcl
    pcl-tools
    libpointmatcher
    python3Packages.open3d

    # Visualization
    ros2.rviz2
    foxglove-studio
    lgsvl-simulator

    # Utilities
    libpcap
    wireshark
    tcpdump
  ];

  # x86_64-only: heavy simulation & GUI tools
  x86LidarPackages = with pkgs; lib.optionals pkgs.stdenv.hostPlatform.isx86_64 [
    ros2.velodyne-simulator
    ros2.ouster-simulator
    ros2.pointcloud-to-laserscan
    ros2.robot-localization
    ros2.slam-toolbox
    ros2.navigation2
    ros2.nav2-bringup
    ignition.gazebo6
    ros2.gazebo-ros-pkgs
  ];

  # Final package list
  lidarPackages = baseLidarPackages
    ++ lib.optionals pkgs.stdenv.hostPlatform.isx86_64 x86LidarPackages;

in {
  options.archibaldOS.lidar = {
    enable = lib.mkEnableOption "Enable LIDAR module with ROS 2 drivers and real-time processing";

    variant = lib.mkOption {
      type = lib.types.enum [ "basic" "simulation" "embedded" ];
      default = "basic";
      description = ''
        LIDAR configuration variant:
        - basic: Drivers + PCL + RViz
        - simulation: Full simulation stack (x86_64 only)
        - embedded: Lightweight for SBCs
      '';
    };

    drivers = lib.mkOption {
      type = lib.types.attrsOf lib.types.bool;
      default = {
        velodyne = true;
        ouster = true;
        hesai = true;
        livox = true;
        rplidar = true;
      };
      description = "Enable specific LIDAR drivers";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = if cfg.variant == "simulation" then config.nixpkgs.system == "x86_64-linux" else true;
        message = "LIDAR simulation variant is only supported on x86_64-linux.";
      }
    ];

    # === Packages ===
    environment.systemPackages = lidarPackages;

    # === Driver-Specific udev Rules ===
    services.udev.extraRules = ''
      # Velodyne (VLP-16, Puck, etc.)
      SUBSYSTEM=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="5740", MODE="0666", GROUP="lidar"
      SUBSYSTEM=="net", KERNEL=="eth*", ATTR{address}=="00:80:48:*", NAME="lidar0", GROUP="lidar"

      # Ouster OS1/OS2
      SUBSYSTEM=="net", ATTR{address}=="00:02:0c:*", NAME="lidar1", GROUP="lidar"

      # Hesai Pandar
      SUBSYSTEM=="net", ATTR{address}=="b8:59:9f:*", NAME="lidar2", GROUP="lidar"

      # Livox MID-360 / AVIA
      SUBSYSTEM=="usb", ATTRS{idVendor}=="04b4", ATTRS{idProduct}=="00f0", MODE="0666", GROUP="lidar"

      # RPLIDAR (Slamtec)
      KERNEL=="ttyUSB*", ATTRS{idVendor}=="10c4", ATTRS{idProduct}=="ea60", MODE="0666", GROUP="lidar"
    '';

    # === Network Configuration (for Ethernet LIDARs) ===
    networking.interfaces = lib.mkIf (cfg.drivers.velodyne || cfg.drivers.ouster || cfg.drivers.hesai) {
      lidar0 = lib.mkIf cfg.drivers.velodyne {
        useDHCP = false;
        ipv4.addresses = [{ address = "192.168.1.100"; prefixLength = 24; }];
      };
      lidar1 = lib.mkIf cfg.drivers.ouster {
        useDHCP = false;
        ipv4.addresses = [{ address = "192.168.1.101"; prefixLength = 24; }];
      };
      lidar2 = lib.mkIf cfg.drivers.hesai {
        useDHCP = false;
        ipv4.addresses = [{ address = "192.168.1.102"; prefixLength = 24; }];
      };
    };

    # === Real-Time & Performance ===
    boot.kernelParams = [
      "threadirqs"
      "isolcpus=1-3"
      "nohz_full=1-3"
      "rcu_nocbs=1-3"
    ];

    powerManagement.cpuFreqGovernor = "performance";

    # === Security & Permissions ===
    security.rtkit.enable = true;
    security.pam.loginLimits = [
      { domain = "@lidar"; item = "rtprio"; type = "-"; value = "95"; }
      { domain = "@lidar"; item = "memlock"; type = "-"; value = "unlimited"; }
    ];

    users.groups.lidar = {};
    users.groups.netdev = {};  # For network interface access

    # === User Integration ===
    users.users.audio-user = lib.mkIf config.users.users ? audio-user {
      extraGroups = [ "lidar" "netdev" ];
    };
    users.users.nixos = lib.mkIf config.users.users ? nixos {
      extraGroups = [ "lidar" "netdev" ];
    };

    # === ROS 2 Environment ===
    environment.variables = {
      ROS_DOMAIN_ID = "42";
      RMW_IMPLEMENTATION = "rmw_cyclonedds_cpp";
    };

    programs.bash.interactiveShellInit = ''
      if [ -z "$ROS_DISTRO" ]; then
        source ${ros2}/setup.bash 2>/dev/null || true
      fi
    '';

    # === Optional: Auto-start RViz with LIDAR config ===
    systemd.user.services.rviz-lidar = lib.mkIf (cfg.variant == "simulation") {
      description = "RViz2 with LIDAR visualization";
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${ros2.rviz2}/bin/rviz2 -d ${./rviz/lidar.rviz}";
        Restart = "always";
      };
    };

    # === Sysctl Tuning ===
    boot.kernel.sysctl = {
      "fs.inotify.max_user_watches" = 524288;
      "net.core.rmem_max" = 134217728;
      "net.core.wmem_max" = 134217728;
      "net.core.netdev_max_backlog" = 5000;
    };

    # === Integration with robotics.nix ===
    archibaldOS.robotics = lib.mkIf config.archibaldOS.robotics.enable {
      enable = true;
      variant = if cfg.variant == "simulation" then "simulation" else "basic";
    };
  };
}
