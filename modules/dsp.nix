# modules/dsp.nix
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
