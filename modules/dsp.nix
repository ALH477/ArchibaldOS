# modules/dsp.nix

# Copyright 2025 DeMoD LLC

# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



{ config, pkgs, lib, ... }:

{
  # ----------------------------------------------------------------------
  # Minimal DSP-coprocessor configuration (kexec-booted, headless)
  # ----------------------------------------------------------------------
  imports = [
    ./base.nix
    ./audio.nix
    ./rt-kernel.nix
  ];

  musnix.enable = lib.mkForce false;

  # ---- Boot ------------------------------------------------------------
  boot.loader.systemd-boot.enable = false;
  boot.loader.grub.enable = false;

  boot.kernelParams = [
    "console=ttyS0,115200n8"
    "threadirqs"
    "isolcpus=0"
    "nohz_full=0"
    "rcu_nocbs=0"
  ];

  # ---- Filesystem -------------------------------------------------------
  boot.initrd.availableKernelModules = [ "nvme" "usbhid" "usb_storage" "ext4" ];
  # REMOVED: fileSystems."/" — let disko handle it

  # ---- Networking (disabled) --------------------------------------------
  networking.useDHCP = false;
  networking.interfaces = { };

  # ---- Users ------------------------------------------------------------
  users.users.root = {
    shell = pkgs.bashInteractive;
    hashedPassword = null;  # no password
  };

  # ---- Console ----------------------------------------------------------
  services.getty.autologinUser = "root";
  services.getty.helpLine = ''
    *** ArchibaldOS DSP Coprocessor ***
    Real-time kernel loaded. Run `dsp-start` to begin JACK.
  '';

  # ---- Disable unnecessary services -------------------------------------
  services.xserver.enable = false;
  services.displayManager.enable = false;
  services.pipewire.enable = lib.mkForce false;
  hardware.bluetooth.enable = false;
  services.avahi.enable = false;
  services.cron.enable = false;
  services.nscd.enable = false;

  # ---- Real-time limits -------------------------------------------------
  security.pam.loginLimits = [
    { domain = "@audio"; item = "rtprio";  type = "-"; value = "99"; }
    { domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
    { domain = "@audio"; item = "nice";    type = "-"; value = "-19"; }
  ];

  users.groups.audio = {};
  users.groups.realtime = {};

  # ---- DSP Packages -----------------------------------------------------
  environment.systemPackages = with pkgs; [
    jack2
    faust faust2jack faust2alsa
    puredata
    csound
    supercollider
    alsa-utils
    vim
    (writeShellScriptBin "dsp-start" ''
      #!/bin/sh
      echo "Starting JACK DSP server..."
      exec ${pkgs.jack2}/bin/jackd -R -P99 -d dummy -r48000 -p32 -n2
    '')
  ];

  # ---- kexec payload generation -----------------------------------------
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    mkdir -p /boot/kexec
    cp ${config.boot.kernelPackages.kernel}/bzImage /boot/kexec/vmlinuz
    cp ${config.system.build.initialRamdisk}/initrd /boot/kexec/initrd
    echo "${lib.concatStringsSep " " config.boot.kernelParams}" > /boot/kexec/cmdline
  '';

  # ---- Disko layout (EFI + root) ----------------------------------------
  disko.devices = {
    disk.main = {
      type = "disk";
      device = "/dev/vda";  # change if needed
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            size = "512M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          };
          root = {
            size = "100%";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/";
            };
          };
        };
      };
    };
  };
}
