ArchibaldOS
 
ArchibaldOS is a custom NixOS-based operating system inspired by the specifications from DeMoD LLC's Archibald project (as described on demod.ltd/archibald.html). It leverages the declarative power of NixOS for reproducible, maintainable configurations, integrated with the high-performance CachyOS Real-Time (RT) BORE kernel for low-latency audio production. This setup includes KDE Plasma as the desktop environment, Hyprland as an optional Wayland compositor, and a curated selection of the best Free and Open-Source Software (FOSS) audio production tools and drivers.
Designed for audio professionals, developers, and enthusiasts, ArchibaldOS aims to provide efficiency surpassing traditional OSes like macOS and Windows, with a focus on real-time audio workloads, modular configuration, and lean system optimization.
Features

Kernel: CachyOS RT-BORE kernel for burst-oriented responsiveness and real-time patches, ideal for low-latency audio (e.g., <5ms in tools like Ardour).
Desktop Environment: KDE Plasma 6 with SDDM display manager for a modern, customizable interface.
Compositor: Optional Hyprland Wayland setup with a lean configuration for performance and gestures.
Audio Stack: PipeWire with JACK/ALSA support, RTKit for privileges, and FOSS tools like Ardour, Audacity, MuseScore, LMMS, Carla, Guitarix, and more.
Packages: Curated list of ~100 essential packages for utilities, development, security, and media, mapped from the provided packages.x86_64 spec (e.g., Vim, Git, FFmpeg, LibreOffice, Bitwarden).
Optimization: Declarative Nix flakes for reproducibility, auto-optimization, and easy rollbacks. Ignored bloat like GRUB in favor of systemd-boot.
Security: UFW firewall, sudo configurations, and group-based realtime privileges.
Maintainability: Modular config structure (e.g., separate modules for packages, audio, KDE) for easy extension.

Requirements

x86_64 hardware (AMD/Intel CPUs supported via ucode).
EFI boot system.
NixOS installer ISO (download from nixos.org).
Basic knowledge of NixOS and flakes (experimental features enabled).

Installation

Boot NixOS Installer:

Download and boot the latest NixOS minimal ISO.
Mount your target disk (e.g., sudo mount /dev/sda1 /mnt).


Generate Hardware Config:
sudo nixos-generate-config --root /mnt

This creates /mnt/etc/nixos/hardware-configuration.nix.

Clone the Repo:
git clone https://github.com/yourusername/archibaldos.git /mnt/etc/nixos
cd /mnt/etc/nixos


Customize:

Edit configuration.nix for your user (e.g., replace youruser).
Adjust timezone, locale, or hardware specifics in configuration.nix.
For Hyprland, copy the example config to ~/.config/hypr/hyprland.conf post-install.


Install:
sudo nixos-install --flake .#archibald

Reboot into the new system.

Post-Install:

Update: nixos-rebuild switch --flake /etc/nixos#archibald.
Test audio: Run qjackctl or realtimeconfigquickscan to verify RT setup.
Enable Flatpak if needed: flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo.



Configuration Structure

flake.nix: Entry point with inputs (Nixpkgs unstable, Chaotic-Nyx for CachyOS kernel).
configuration.nix: Core system settings (boot, kernel, users, networking).
modules/:
packages.nix: System-wide packages from the spec.
audio.nix: PipeWire, JACK, RT optimizations.
kde.nix: KDE Plasma enablement.
hyprland.nix: Hyprland program enablement.


hardware-configuration.nix: Auto-generated hardware specifics.

Example Hyprland Config
The provided hyprland.conf is lean and optimized:

Minimal animations for performance.
Keybinds for terminals, browsers, and audio tools (e.g., SUPER+A for QJackCtl).
Window rules for floating audio apps.

Copy to ~/.config/hypr/ and customize as needed.
Usage

Rebuild System: nixos-rebuild switch for changes.
Audio Production:
Start a session in Ardour or Carla.
Use PipeWire for seamless JACK integration.
Monitor latency with jack_iodelay.


Desktop Switching: KDE is default; launch Hyprland via hyprland command or session manager.
Backups: Use Timeshift for snapshots.

Kernel Integration Details
The CachyOS RT-BORE kernel is sourced via Chaotic-Nyx overlay:

boot.kernelPackages = pkgs.linuxPackages_cachyos-rt-bore;
Params: preempt=full threadirqs for RT enhancements.
If build issues arise, fallback to source compilation or check Chaotic-Nyx cache status.

Contributing
Fork the repo, make changes (e.g., add modules), and submit a PR. Follow Nix best practices for modularity.
License
MIT License. See LICENSE for details.
