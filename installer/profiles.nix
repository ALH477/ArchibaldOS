# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# What the installer's profile page offers, in display order. The ids are
# flake.nix's `profiles.<id>`; install.json records the chosen one, and
# `nixosConfigurations.installed` builds that profile. Descriptions are shown
# to someone about to wipe a disk, so they say what costs what.
[
  {
    id = "audio";
    name = "Audio workstation";
    description = ''
      Real-time audio production: CachyOS kernel, Plasma 6, Ardour, Reaper,
      Zrythm, Surge, Faust, SuperCollider, Csound, Pure Data. Comfortable with
      8 GB of RAM or more.
    '';
  }
  {
    id = "robotics";
    name = "Robotics workstation";
    description = ''
      Real-time control and robotics: CachyOS kernel, Plasma 6, Octave,
      OpenCV, FreeCAD, KiCad, Arduino IDE, CAN bus, board access for the
      dialout/plugdev groups.
    '';
  }
  {
    id = "companion";
    name = "Companion (older hardware)";
    description = ''
      A headless music computer commanded by an Oligarchy host: JACK, zram,
      no desktop. 4 GB of RAM is enough. The CachyOS kernel is the build
      Chaotic's binary cache carries for this nixpkgs revision; if the
      install starts compiling a kernel anyway, stop it and let Oligarchy
      build. After install, run `oligarchy-companion enroll` on Oligarchy.
    '';
  }
  {
    id = "companion-surface";
    name = "Companion for Microsoft Surface";
    description = ''
      The companion with Surface support: linux-surface kernel, touch,
      thermald. That kernel is COMPILED FROM SOURCE. On a 4 GB Surface,
      install "Companion" instead and let Oligarchy build and push this one.
    '';
  }
  {
    id = "hydramesh";
    name = "HydraMesh node";
    description = "Headless HydraMesh P2P networking node (CachyOS kernel, no desktop).";
  }
  {
    id = "audio-musnix";
    name = "Audio workstation (musnix PREEMPT_RT)";
    description = ''
      The audio workstation on musnix's mainline PREEMPT_RT kernel instead of
      CachyOS. That kernel is compiled from source during install.
    '';
  }
  {
    id = "robotics-musnix";
    name = "Robotics workstation (musnix PREEMPT_RT)";
    description = ''
      The robotics workstation on musnix's mainline PREEMPT_RT kernel. That
      kernel is compiled from source during install.
    '';
  }
]
