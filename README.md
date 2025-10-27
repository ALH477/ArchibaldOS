# ArchibaldOS

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![NixOS](https://img.shields.io/badge/NixOS-Unstable-blue.svg)](https://nixos.org/)
[![Hyprland](https://img.shields.io/badge/WM-Hyprland%20%2B%20DWM-green.svg)](https://hyprland.org/)
[![Real-Time Audio](https://img.shields.io/badge/RT-Audio-orange.svg)](https://musnix.org/)

**Copyright (c) 2025 DeMoD LLC**

**ArchibaldOS** is a minimalist, audio-centric NixOS distribution designed for musicians, sound designers, and real-time audio professionals. Built on the reproducible foundation of NixOS, it prioritizes low-latency audio processing, MIDI integration, and a seamless, customizable desktop experience. Whether you're composing with synthesizers, mixing in a DAW, or experimenting with modular DSP, ArchibaldOS provides a battle-tested environment optimized for performance and creativity.

ArchibaldOS combines the power of the Musnix real-time kernel with a curated selection of FOSS tools for MIDI sequencing, synthesis, and effects processing. It features a Hyprland-based Wayland desktop (with DWM fallback) for fluid, distraction-free workflows, and includes unique integrations like HydraMesh for P2P audio networking and StreamDB for metadata management.

> **Tagline**: *Reproducible Rhythms, Uncompromised Latency.*

## Features

- **Real-Time Audio Kernel**: Powered by Musnix with PREEMPT_RT patches for sub-millisecond latency—ideal for live performance and plugin-heavy sessions.
- **Curated Audio Toolchain**:
  - **DAWs**: Ardour, LMMS, Audacity, Mixxx, Zrythm, Stargate.
  - **MIDI Tools**: MuseScore, FluidSynth.
  - **DSP & Synthesis**: PortAudio, RtAudio, Faust, JUCE, Csound, SuperCollider, Surge, Dexed, VCV Rack.
  - **Effects & Modular**: Dragonfly Reverb, Pure Data (Pd).
  - **Streaming**: OBS Studio.
- **Desktop Environment**: Hyprland (Wayland) with Waybar status bar, Wofi launcher, and Kitty terminal. DWM available as an X11 alternative. Custom themes, keybindings, and scripts for monitor management, screenshotting, and theme switching.
- **Custom Integrations**:
  - **HydraMesh**: P2P mesh networking for collaborative audio sessions (toggle via `Super+Shift+H`).
  - **StreamDB**: Efficient storage for audio metadata and presets.
- **Installer**: A TUI-based live ISO (no Calamares) with Disko partitioning, auto-configured for hardware-agnostic setups (GPT/ext4, no LUKS).
- **Reproducibility**: Fully declarative via Nix flakes—build once, deploy anywhere.
- **Hardware Agnostic**: Works on x86_64 (with ARM64 support in progress); optimized for PipeWire/JACK/ALSA.

## Quick Start

### Prerequisites
- A compatible x86_64 machine with at least 4GB RAM and 20GB free disk space.
- Internet access for initial flake locking and builds.
- Enable Nix flakes: Add `experimental-features = nix-command flakes` to `~/.config/nix/nix.conf`.

### Building the Installer ISO
1. Clone the repository:
   ```
   git clone https://github.com/yourusername/archibaldos.git
   cd archibaldos
   ```
2. Lock dependencies:
   ```
   nix flake lock
   ```
3. Build the live ISO:
   ```
   nix build .#installer
   ```
   The ISO will be at `./result/iso/ArchibaldOS-audio-unstable-x86_64-linux.iso`.

### Installation
1. Boot from the ISO (USB or VM).
2. Log in automatically as `nixos` (password: `nixos`) in Hyprland.
3. Open a terminal (`Super+Q`) and run:
   ```
   sudo /etc/installer.sh
   ```
4. Follow the TUI prompts:
   - Select keyboard layout.
   - Choose the target disk (warning: erases data!).
   - Configure HydraMesh options (enable/disable, firewall/AppArmor).
   - Set locale, timezone, hostname, username, and passwords.
5. Reboot into your new system.

Post-install, log in via SDDM (select Hyprland or DWM). Use `Super+Shift+K` for a keybindings cheatsheet.

## Detailed Installation Guide

### From Source
If building from source:
- Ensure `../HydraMesh` and `../StreamDB` are sibling directories (flakes or sources).
- Place your wallpaper at `../wallpaper.jpg` (copied as `wall.jpg` in `~/Pictures`).
- Compute hashes for HydraMesh/StreamDB overlays via trial `nix build` (update `vendorHash`/`cargoHash`).

### Hardware Requirements
- **CPU**: Modern x86_64 (Intel/AMD); ARM64 experimental.
- **Storage**: 20GB+ SSD/HDD.
- **Audio**: ALSA-compatible interface recommended for RT performance.
- **Graphics**: Wayland-compatible GPU (Intel/AMD/NVIDIA with proprietary drivers if needed).

### Customizing the Installer
Edit `/etc/installer.sh` in the live environment before running:
- Add partitions via Disko config (`/tmp/disko.nix`).
- Extend prompts for additional options (e.g., Steam integration).

## System Overview

### Audio Stack
- **Kernel**: Linux RT (via Musnix) with `threadirqs` for prioritized audio IRQs.
- **Server**: PipeWire (ALSA/Pulse/JACK) for unified routing.
- **Tools**: See [Features](#features) for the full list. All configured for low-latency via `rtkit`.

### Desktop Experience
- **Hyprland Config**: `~/.config/hypr/hyprland.conf` includes:
  - Animations, bezier curves, and dwindle layout for smooth interactions.
  - Keybindings: Workspaces (`Super+1-0`), focus movement (`Super+H/L/K/J`), audio controls (`XF86Audio*`).
  - Scripts: Resolution toggle (`Super+Shift+W`), theme changer (`Super+Shift+T`), web app launcher (`Super+Shift+A`).
- **Waybar**: Custom bar with audio/network/HydraMesh status.
- **Wallpaper**: Set via Hyprpaper; supports multi-monitor.

### HydraMesh & StreamDB
- **HydraMesh**: Starts as a systemd service. Edit `/etc/hydramesh/config.toml`. Toggle with `hydramesh-toggle`.
- **StreamDB**: CLI tool; integrate with DAWs via scripts (e.g., preset export).

## Usage

### Daily Workflow
1. Boot and select Hyprland/SDDM.
2. Launch DAW (e.g., Ardour via menu or `Super+D`).
3. Connect MIDI devices (auto-detected via ALSA).
4. Monitor latency with `rtcqs` in terminal.
5. Collaborate: Enable HydraMesh for P2P session sharing.

### Key Shortcuts
- `Super+Q`: Terminal (Kitty).
- `Super+D`: Launcher (Wofi).
- `Super+E`: File Manager (Dolphin).
- `Super+Shift+PRINT`: Region screenshot.
- Full list: `Super+Shift+K`.

### Audio Testing
Run `jackd -R` for JACK server, then test with `aplay /usr/share/sounds/alsa/Front_Center.wav`.

## Customization

- **Packages**: Add to `audioPackages` in `flake.nix` and rebuild.
- **Hyprland**: Edit `~/.config/hypr/hyprland.conf` and reload (`hyprctl reload`).
- **Themes**: Use `theme_changer.py` for color schemes (green default).
- **HydraMesh**: Customize service in `systemd.services.hydramesh`.
- **Flake Overrides**: Pin inputs or add overlays for new tools.

For advanced tweaks, see [NixOS Manual](https://nixos.org/manual/nixos/stable/).

## Troubleshooting

- **High Latency**: Run `rtcqs` to diagnose; ensure `musnix.rtirq.enable = true`.
- **Hyprland Crashes**: Fallback to DWM; check logs with `journalctl -u sddm`.
- **MIDI Issues**: Verify `musnix.alsaSeq.enable = true`; test with `aconnect -l`.
- **Build Errors**: Update hashes for HydraMesh/StreamDB; use `nix flake check`.
- **ISO Boot Failure**: Verify USB with `dd` or Rufus; test in QEMU.

Report issues on GitHub with `nix log` output.

## Contributing

Contributions are welcome! Focus areas:
- New audio tools (PR to `audioPackages`).
- HydraMesh/StreamDB enhancements.
- ARM64 porting.
- Bug fixes for RT audio.

1. Fork and clone.
2. Create a feature branch (`git checkout -b feature/audio-tool`).
3. Commit (`git commit -m "Add new synth package"`).
4. Push and PR.

Follow [Nixpkgs style guide](https://nixos.org/manual/nixpkgs/stable/#chap-contributing). Code of Conduct: [Contributor Covenant](https://www.contributor-covenant.org/version/2/0/code_of_conduct/).

## License

This project is licensed under the MIT License - see [LICENSE](LICENSE) for details.

## Acknowledgments

- [NixOS](https://nixos.org/) for the declarative foundation.
- [Musnix](https://musnix.org/) for RT audio expertise.
- [Hyprland](https://hyprland.org/) for the dynamic WM.
- Community contributors to HydraMesh and StreamDB.

---

*Built with ❤️ for audio creators. Last updated: October 27, 2025.*
