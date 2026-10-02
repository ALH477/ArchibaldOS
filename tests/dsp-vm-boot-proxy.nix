# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# packages.dsp-vm-boot-proxy — does an image built by modules/dsp-vm-image.nix
# boot under SeaBIOS AND under OVMF?
#
# The DSP guest image was BIOS-only (no ESP), and the host that boots it,
# Oligarchy's vm-manager dsp-vm module, defaults to OVMF — where an image with
# no ESP falls through to PXE and never boots. Oligarchy hit that failure with
# the image this one replaced. So the claim "hybrid, boots on either firmware"
# is measured here rather than argued: the same image is booted twice, and the
# guest must report, from userspace, which firmware it came up under —
# `firmware=uefi` only exists if /sys/firmware/efi does, so an OVMF run that
# somehow fell back to legacy boot cannot pass as UEFI.
#
# A stock kernel stands in for the RT one: the layout and the GRUB install are
# what is under test, and neither depends on the kernel. The RT image itself
# remains [UNTESTED] under OVMF until it is booted (docs/security.md).
#
# Needs the `kvm` system feature, like make-disk-image itself; QEMU uses KVM
# when /dev/kvm exists and falls back to TCG (slow, but it boots).
{ pkgs, nixpkgs, system }:

let
  guest = nixpkgs.lib.nixosSystem {
    inherit system;
    modules = [
      ../modules/dsp-vm-image.nix
      ({ lib, pkgs, ... }: {
        system.stateVersion = "24.11";
        boot.supportedFilesystems.zfs = lib.mkForce false;
        boot.kernelParams = [ "console=ttyS0,115200" ];
        networking.hostName = "dsp-boot-proxy";
        networking.useDHCP = false;
        documentation.enable = false;
        documentation.nixos.enable = false;

        # Reaching multi-user.target is the proof; say so on the serial
        # console (with the firmware we came up under), then power off.
        systemd.services.boot-proof = {
          wantedBy = [ "multi-user.target" ];
          serviceConfig.Type = "oneshot";
          script = ''
            fw=bios
            [ -d /sys/firmware/efi ] && fw=uefi
            echo "ARCHIBALD-BOOT-OK firmware=$fw" > /dev/ttyS0
            ${pkgs.systemd}/bin/systemctl poweroff --no-block
          '';
        };
      })
    ];
  };
  image = guest.config.system.build.qcow2;
in
pkgs.runCommand "dsp-vm-boot-proxy"
  {
    nativeBuildInputs = [ pkgs.qemu_kvm pkgs.coreutils pkgs.gnugrep ];
    requiredSystemFeatures = [ "kvm" ];
    passthru = { inherit image guest; };
  }
  ''
    boot() { # boot <bios|uefi>
      fw=$1
      qemu-img create -q -f qcow2 -F qcow2 -b ${image}/nixos.qcow2 $fw.qcow2
      extra=""
      if [ "$fw" = uefi ]; then
        install -m 0644 ${pkgs.OVMF.fd}/FV/OVMF_VARS.fd vars.fd
        extra="-drive if=pflash,format=raw,unit=0,readonly=on,file=${pkgs.OVMF.fd}/FV/OVMF_CODE.fd
               -drive if=pflash,format=raw,unit=1,file=vars.fd"
      fi
      # shellcheck disable=SC2086
      timeout 1800 qemu-system-x86_64 -machine accel=kvm:tcg -cpu max -m 1024 \
        -display none -no-reboot -nic none \
        -serial file:$fw.log \
        -drive file=$fw.qcow2,if=virtio,format=qcow2 $extra \
        || true
      if grep -q "ARCHIBALD-BOOT-OK firmware=$fw" $fw.log; then
        echo "PASS: boots under $fw, userspace reports firmware=$fw" | tee -a $out
      else
        echo "FAIL: no 'ARCHIBALD-BOOT-OK firmware=$fw' on the serial console under $fw"
        echo "--- last 40 lines of $fw.log ---"; tail -40 $fw.log
        exit 1
      fi
    }
    boot bios
    boot uefi
  ''
