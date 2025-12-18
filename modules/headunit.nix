# modules/headunit.nix
# 
# Module: archibaldos.headunit
# Description: ArchibaldOS Head Unit Edition - Automotive infotainment with Plasma Bigscreen,
#              PicoScope 7 diagnostics, low-latency audio, and vehicle integrations.
#
# Copyright © 2025 DeMoD LLC
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.archibaldos.headunit;
in
{
  options.archibaldos.headunit = {
    enable = mkEnableOption "ArchibaldOS Head Unit Edition: Automotive infotainment with Plasma Bigscreen, PicoScope 7 Automotive diagnostics, and real-time audio";

    autoLoginUser = mkOption {
      type = types.str;
      default = "driver";
      description = "User for auto-login (kiosk-style head unit experience).";
    };

    uiScaling = mkOption {
      type = types.float;
      default = 2.0;
      description = "Global UI scaling factor for large, finger-friendly controls.";
    };

    enableDiagnostics = mkOption {
      type = types.bool;
      default = true;
      description = "Enable automotive diagnostics tools (PicoScope, OBD-II, CAN).";
    };

    enableNavigation = mkOption {
      type = types.bool;
      default = true;
      description = "Enable offline navigation (OsmAnd) and GPS.";
    };

    touchscreenDevice = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "/dev/input/event2";
      description = "Path to touchscreen input device for calibration (if needed).";
    };

    gpsDevice = mkOption {
      type = types.str;
      default = "/dev/ttyUSB0";
      description = "GPS device path for gpsd.";
    };
  };

  config = mkIf cfg.enable {
    # Overlay for PicoScope 7 (latest stable series as of Dec 2025: 7.2.x)
    nixpkgs.overlays = mkIf cfg.enableDiagnostics [
      (final: prev: {
        picoscope = prev.stdenv.mkDerivation rec {
          pname = "picoscope";
          version = "7.2.10";  # Update to latest 7.2.x if needed

          src = prev.fetchurl {
            url = "https://labs.picotech.com/picoscope7/debian/pool/main/p/picoscope/picoscope_${version}_amd64.deb";
            sha256 = lib.fakeSha256;
          };

          nativeBuildInputs = with prev; [ dpkg autoPatchelfHook makeWrapper ];
          buildInputs = with prev; [ 
            gtk3 libusb1 qt6.qtbase qt6.qtwayland qt6.qtsvg stdenv.cc.cc.lib
          ];

          unpackPhase = "dpkg-deb -x $src .";
          installPhase = ''
            mkdir -p $out
            cp -r usr/* $out/
            if [ -d opt/picoscope ]; then
              mkdir -p $out/opt
              cp -r opt/picoscope $out/opt/
              mkdir -p $out/bin
              makeWrapper $out/opt/picoscope/bin/picoscope $out/bin/picoscope \
                --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [ prev.libusb1 prev.stdenv.cc.cc.lib ]}
            fi
          '';

          meta = with lib; {
            description = "PicoScope 7 Automotive - Professional oscilloscope GUI";
            homepage = "https://www.picoauto.com/downloads";
            license = licenses.unfree;
            platforms = [ "x86_64-linux" ];
            mainProgram = "picoscope";
          };
        };
      })
    ];

    # SDDM with Wayland and structured autologin
    services.displayManager.sddm = {
      enable = true;
      wayland.enable = true;
      autoLogin = {
        enable = true;
        user = cfg.autoLoginUser;
        session = "plasmabigscreen-wayland.desktop";  # Verify exact name on build if needed
      };
    };

    # System packages
    environment.systemPackages = with pkgs; [
      plasma-bigscreen
      kdePackages.plasma-workspace
    ] ++ optionals cfg.enableDiagnostics [
      picoscope
      python312Packages.obd
      can-utils
    ] ++ optionals cfg.enableNavigation [
      osmand
    ] ++ [
      vlc mpv ffmpeg libinput wev
    ];

    # Scaling and Wayland env
    environment.variables = {
      QT_SCALE_FACTOR = toString cfg.uiScaling;
      GDK_SCALE = toString (builtins.floor cfg.uiScaling);
      PLASMA_USE_QT_SCALING = "1";
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    };

    services.xserver.dpi = 200;

    # Power/logind (vehicle-friendly)
    services.logind.extraConfig = ''
      HandlePowerKey=ignore
      HandleLidSwitch=ignore
      HandleLidSwitchExternalPower=ignore
      IdleAction=ignore
    '';

    # Udev rules
    services.udev.extraRules = ''
      SUBSYSTEM=="usb", ATTR{idVendor}=="0ce9", MODE="0666", GROUP="plugdev"
      SUBSYSTEM=="usb", ATTR{idVendor}=="10c4", ATTR{idProduct}=="ea60", MODE="0666", GROUP="plugdev"
      SUBSYSTEM=="usb", ATTR{idVendor}=="0403", ATTR{idProduct}=="6001", MODE="0666", GROUP="plugdev"
      SUBSYSTEM=="net", KERNEL=="can*", GROUP="plugdev", MODE="0660"
    '';

    users.groups.plugdev = {};

    # User
    users.users.${cfg.autoLoginUser} = {
      isNormalUser = true;
      extraGroups = [ "wheel" "audio" "video" "plugdev" "networkmanager" "dialout" ];
      initialPassword = "changeme";
      description = "ArchibaldOS Head Unit Driver";
    };

    # Low-latency audio
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
      extraConfig.pipewire."10-headunit" = {
        "context.properties" = {
          "default.clock.rate" = 48000;
          "default.clock.quantum" = 512;
          "default.clock.min-quantum" = 256;
        };
      };
    };

    # Connectivity
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
    services.blueman.enable = true;

    services.gpsd = mkIf cfg.enableNavigation {
      enable = true;
      devices = [ cfg.gpsDevice ];
      readonly = true;
    };

    networking.networkmanager = {
      enable = true;
      wifi.powersave = false;
    };

    networking.firewall.enable = true;
    networking.firewall.allowedTCPPorts = [ 22 ];

    services.timesyncd.enable = true;

    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "picoscope" ];

    warnings = optional (!cfg.enableDiagnostics && cfg.enable)
      "Diagnostics disabled—PicoScope/OBD unavailable.";

    assertions = [
      { assertion = cfg.uiScaling >= 1.0 && cfg.uiScaling <= 4.0;
        message = "UI scaling must be between 1.0 and 4.0"; }
      { assertion = cfg.autoLoginUser != "root";
        message = "Auto-login user should not be root"; }
    ];
  };
}
