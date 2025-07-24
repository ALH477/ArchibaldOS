ArchibaldOS


ArchibaldOS is a custom NixOS-based operating system inspired by the specifications from DeMoD LLC's Archibald project (as described on demod.ltd/archibald.html). It leverages the declarative power of NixOS for reproducible, maintainable configurations, integrated with the high-performance Musnix Real-Time (RT) kernel for low-latency audio production. This setup includes KDE Plasma as the desktop environment, Hyprland as an optional Wayland compositor, and a curated selection of the best Free and Open-Source Software (FOSS) audio production tools and drivers.

Designed for audio professionals, developers, and enthusiasts, ArchibaldOS aims to provide efficiency surpassing traditional OSes like macOS and Windows, with a focus on real-time audio workloads, modular configuration, and lean system optimization.

Created by Asher LeRoy, Founder of DeMoD LLC.

Features

- Kernel: Musnix RT kernel for burst-oriented responsiveness and real-time patches, ideal for low-latency audio (e.g., <5ms in tools like Ardour). LTS backup via specialisations for reliability.
- Desktop Environment: KDE Plasma 6 with SDDM display manager for a modern, customizable interface.
- Compositor: Optional Hyprland Wayland setup with a lean configuration for performance and gestures.
- Audio Stack: PipeWire with JACK/ALSA support, RTKit for privileges, and FOSS tools like Ardour, Audacity, MuseScore, LMMS, Carla, Guitarix, and more (expanded for 2025 FOSS ecosystem).
- Packages: Curated list of ~140 essential packages for utilities, development, security, and media, mapped from the provided packages.x86_64 spec (e.g., Vim, Git, FFmpeg, LibreOffice, Bitwarden).
- Optimization: Declarative Nix flakes for reproducibility, auto-optimization, and easy rollbacks. Ignored bloat like GRUB in favor of systemd-boot.
- Security: UFW firewall, sudo configurations, and group-based realtime privileges.
- Maintainability: Modular config structure (e.g., separate modules for packages, audio, KDE) for easy extension. Post-install script for precision hardware setup during live boot.

Requirements

- x86_64 hardware (AMD/Intel CPUs supported via ucode).
- EFI boot system.
- NixOS installer ISO (download from nixos.org).
- Basic knowledge of NixOS and flakes (experimental features enabled).

Installation

1. Boot NixOS Installer:
   - Download and boot the latest NixOS minimal ISO.
   - Mount your target disk (e.g., sudo mount /dev/sda1 /mnt).

2. Generate Hardware Config:
   sudo nixos-generate-config --root /mnt
   This creates /mnt/etc/nixos/hardware-configuration.nix.

3. Clone the Repo:
   git clone https://github.com/yourusername/archibaldos.git /mnt/etc/nixos
   cd /mnt/etc/nixos

4. Customize:
   - Edit configuration.nix for your user (e.g., replace youruser).
   - Adjust timezone, locale, or hardware specifics in configuration.nix.
   - For Hyprland, copy the example config to ~/.config/hypr/hyprland.conf post-install.

5. Install:
   sudo nixos-install --flake .#archibald
   Reboot into the new system.

6. Post-Install:
   - Update: nixos-rebuild switch --flake /etc/nixos#archibald.
   - Run precision setup: sudo post-install-audio-setup.sh.
   - Test audio: Run qjackctl or realtimeconfigquickscan to verify RT setup.
   - Kernel switch: sudo nixos-rebuild boot --specialisation lts-backup for LTS backup.
   - Enable Flatpak if needed: flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo.

Configuration Structure

- flake.nix: Entry point with inputs (Nixpkgs unstable, Musnix for RT kernel).
- configuration.nix: Core system settings (boot, kernel, users, networking, specialisations).
- modules/:
  - packages.nix: System-wide packages from the spec.
  - audio.nix: PipeWire, JACK, RT optimizations.
  - kde.nix: KDE Plasma enablement.
  - hyprland.nix: Hyprland program enablement.
- hardware-configuration.nix: Auto-generated hardware specifics.
- post-install-audio-setup.sh: Bash script for precision hardware/audio config during live boot or post-install.
- hyprland.conf: Example Hyprland config (copy to ~/.config/hypr/).

Usage

- Rebuild System: nixos-rebuild switch for changes.
- Audio Production:
  - Start a session in Ardour or Carla.
  - Use PipeWire for seamless JACK integration.
  - Monitor latency with jack_iodelay.
- Desktop Switching: KDE is default; launch Hyprland via hyprland command or session manager.
- Backups: Use Timeshift for snapshots.
- Kernel Switching: Use specialisations for RT/LTS toggling (see script or manual).

Kernel Integration Details

Musnix provides declarative RT kernel support:
- boot.kernelPackages = pkgs.linuxPackages_rt; (handled by Musnix).
- Params: preempt=full threadirqs for RT enhancements.
- LTS backup via specialisation for failure recovery.

Contributing

Fork the repo, make changes (e.g., add modules), and submit a PR. Follow Nix best practices for modularity.

License

MIT License. See LICENSE for details.
