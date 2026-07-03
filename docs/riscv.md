<!-- SPDX-License-Identifier: BSD-3-Clause -->
# ArchibaldOS on RISC-V (StarFive JH7110)

A real-time-audio NixOS SD image for boards built on the **StarFive JH7110**
SoC (quad SiFive U74-MC @ 1.5 GHz) — the **VisionFive 2** and the
**DeepComputing Framework Laptop 13 RISC-V** mainboard.

- Config: `nixosConfigurations.archibaldOS-riscv`
- Image:  `packages.riscv64-linux.archibaldOS-riscv-sdimage`
  (also exposed as `packages.x86_64-linux.archibaldOS-riscv-sdimage` for cross builds)
- Kernel: mainline **Linux 6.12 with native `PREEMPT_RT`** (the CachyOS RT BORE
  kernel used on x86 does not build for riscv64).
- Modules: `modules/rt-audio-riscv.nix`, `hardware/jh7110.nix`,
  `modules/riscv-cross-overlay.nix`.

The riscv64 image is built from **nixpkgs-unstable** (the `nixpkgs-riscv` flake
input); the x86 configs stay on the 24.11 pin. riscv64 toolchain support in the
base system closure is not viable on the older pin.

## Option A — build natively on the board (preferred)

Building on the JH7110 board itself avoids emulation entirely and is the
recommended path. Three prerequisites:

1. **Lower `vm.mmap_min_addr` to 4096.** The nixpkgs riscv64 bootstrap gcc is a
   `Type: EXEC` binary with a `PT_LOAD` segment near `0xf000`; a distro default
   of `65536` makes the Nix build sandbox fail with a misleading `exit 139`
   (SIGSEGV) during bootstrap. On a Debian/Ubuntu bring-up host:
   `sudo sysctl vm.mmap_min_addr=4096` (the image sets this for you).
2. **Provide ≥ 8 GB of swap.** The riscv64 linker peaks around 3.2 GB during the
   kernel/link phases; a USB stick as swap (`mkswap`, `swapon -p 100`) is enough.
3. **(Optional) a binary cache with riscv64 substitutes** for the bootstrap
   toolchain, to skip a multi-hour local GCC bootstrap. The PREEMPT_RT kernel is
   generally not cached and builds locally (~30–60 min).

Then:

```bash
nix build .#packages.riscv64-linux.archibaldOS-riscv-sdimage
```

> If you use a remote builder, it must be **x86_64-linux** only. A mixed remote
> that also advertises riscv64 can drop x86 ELFs at riscv64 store paths.

## Option B — cross-build from x86_64 (fallback; slow)

Deprecated for production, but useful for CI/smoke. Enable binfmt qemu-user on
the build host:

```nix
# configuration.nix on the x86_64 build host
boot.binfmt.emulatedSystems = [ "riscv64-linux" ];
```

```bash
nix build .#packages.x86_64-linux.archibaldOS-riscv-sdimage
```

A handful of upstream test suites fail under qemu-user because it doesn't
translate certain syscalls (AF_ALG, `IPV6_MULTICAST_IF`, ptrace, …).
`modules/riscv-cross-overlay.nix` disables just those check phases and **only
when cross-compiling** — on a native build every test runs as authored. The
cross build takes hours; prefer Option A when you have hardware.

## Flashing

The build output is a zstd-compressed image:

```bash
zstd -d result/sd-image/*.img.zst -o archibaldos-rv.img
sudo dd if=archibaldos-rv.img of=/dev/sdX bs=4M conv=fsync status=progress
```

The JH7110 boards ship U-Boot in SPI flash and boot via
`generic-extlinux-compatible` from the SD root, so the image's firmware
partition is intentionally empty. Insert the card and power on.

Default login is `audio` / `archibald` (auto-login on tty1). **Change the
password immediately after first boot.**

## Notes

- The image is a **headless console-framebuffer** appliance: USB class-compliant
  audio, USB-attached I2C peripherals, no desktop. Wireless/VPN are not
  configured by default (NetworkManager is omitted — it pulls Haskell-dependent
  plugins that don't bootstrap on riscv64); it uses systemd-networkd DHCP.
- To add packages, keep to what builds for riscv64 — large audio apps
  (Ardour/Reaper/etc.) are an x86-only path today.
