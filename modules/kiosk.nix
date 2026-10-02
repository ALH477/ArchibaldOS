# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# archibald.kiosk — a DeMoD UI app as the machine's front panel, when the
# machine has a touchscreen; nothing at all when it does not.
#
# cage (a single-app Wayland compositor) runs the program fullscreen on tty1
# as the audio user. It is NOT started by graphical.target: udev starts it
# when an input device with ID_INPUT_TOUCHSCREEN appears, at boot (coldplug)
# or when one is plugged in later. A rack unit with no panel stays headless and
# spends nothing on graphics; plug in a USB touch panel and the mixer comes up.
#
# The default program is DeMoD Mixer (DeMoD apps/mixer, MPL-2.0) with
# DEMOD_KIOSK=1: fullscreen, no cursor. `engine` says where its engine is:
# "local" (this box runs it), "remote:HOST" (the DSP host's demod-remote-bridge
# over DCF, through WireGuard), or "sim".
{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption mkEnableOption mkIf mkForce types optionalAttrs;
  cfg = config.archibald.kiosk;
in
{
  options.archibald.kiosk = {
    enable = mkEnableOption "a DeMoD UI kiosk on a touchscreen";
    user = mkOption { type = types.nullOr types.str; default = config.archibald.jack.user or null; defaultText = lib.literalExpression "config.archibald.jack.user"; description = "Runs the app (the audio user, so it can read the engine's meters)."; };
    package = mkOption {
      type = types.nullOr types.package;
      default = null;
      description = "The app's package: the demod flake's demod-mixer by default (set by the profile).";
    };
    program = mkOption {
      type = types.nullOr types.str;
      default = if cfg.package != null then lib.getExe cfg.package else null;
      defaultText = lib.literalExpression "lib.getExe config.archibald.kiosk.package";
      description = "What cage runs.";
    };
    engine = mkOption {
      type = types.str;
      default = "local";
      example = "remote:10.78.0.2";
      description = "DEMOD_MIXER_ENGINE for the app: local, remote:HOST[:PORT] or sim.";
    };
    onTouchscreenOnly = mkOption {
      type = types.bool;
      default = true;
      description = "Start only when a touchscreen is present. false: start at boot on any display.";
    };
    softwareRendering = mkOption {
      type = types.bool;
      default = false;
      description = "Render without a GPU (wlroots' pixman renderer, SDL's software renderer): boards with no mainline GPU driver.";
    };
    environment = mkOption { type = types.attrsOf types.str; default = { }; description = "Extra environment for the app."; };
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = cfg.user != null && cfg.program != null;
      message = "archibald.kiosk needs a user and a program (or package).";
    }];

    services.cage = {
      enable = true;
      user = cfg.user;
      program = cfg.program;
      extraArguments = [ "-d" ]; # no client-side decorations
      environment = {
        DEMOD_KIOSK = "1";
        DEMOD_MIXER_ENGINE = cfg.engine;
      } // optionalAttrs cfg.softwareRendering {
        WLR_RENDERER = "pixman";
        SDL_RENDER_DRIVER = "software";
      } // cfg.environment;
    };

    systemd.services."cage-tty1" = mkIf cfg.onTouchscreenOnly { wantedBy = mkForce [ ]; };
    systemd.targets.graphical.wants = mkIf cfg.onTouchscreenOnly (mkForce [ ]);
    services.udev.extraRules = mkIf cfg.onTouchscreenOnly ''
      # archibald.kiosk: a touchscreen brings the front panel up.
      ACTION=="add", SUBSYSTEM=="input", ENV{ID_INPUT_TOUCHSCREEN}=="1", TAG+="systemd", ENV{SYSTEMD_WANTS}+="cage-tty1.service"
    '';

    # The panel follows the box's engine: same account, same meters segment.
    users.users.${cfg.user}.extraGroups = [ "video" "input" ];
  };
}
