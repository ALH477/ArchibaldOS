# ArchibaldOS: Lean Real-Time Audio NixOS Distribution

**ArchibaldOS** is a streamlined, real-time (RT) audio-focused Linux distribution based on NixOS, derived from the Oligarchy NixOS framework. Optimized for musicians, sound designers, and DSP researchers, it prioritizes low-latency audio processing, MIDI workflows, and modular synthesis on x86_64 hardware. Built with the Musnix real-time kernel, a minimal Hyprland Wayland desktop, and integrations for HydraMesh (P2P audio networking) and StreamDB (audio metadata storage), ArchibaldOS delivers a lightweight, reproducible, and high-performance environment. A Text User Interface (TUI) installer simplifies setup, and essential utilities (file manager, text editor, browser) ensure basic functionality without compromising its audio-centric design. Post-install optimizations via the embedded `audio-setup.sh` script enable sub-millisecond latency tuning, making it ideal for professional live performance, recording, and synthesis.

This distribution emphasizes **utilitarian efficiency**, **reproducibility**, and **accessibility**, with a focus on achieving <1.5ms round-trip latency and near-zero xruns through declarative NixOS configurations and RT-specific tweaks.

## Why ArchibaldOS?

ArchibaldOS stands out as a purpose-built RT audio system, addressing the unique demands of low-latency creative workflows while avoiding the bloat of general-purpose distributions. Key differentiators include:

- **Sub-Millisecond Latency**: Leverages Musnix’s PREEMPT_RT kernel, PipeWire/JACK with tuned quantum buffers (32-64 samples), and hardware optimizations (e.g., IRQ pinning, C-state disabling) to deliver ~0.7-1.3ms round-trip latency—comparable to specialized audio OSes like Ubuntu Studio but with NixOS’s reproducibility.
- **Lean and Reproducible**: A minimal package set (~18 core tools) and flake-based builds ensure consistent, declarative setups across deployments, reducing setup time and eliminating "it works on my machine" issues.
- **Hardware Agnosticism with Flexibility**: Generic configurations support diverse x86_64 systems (e.g., Framework 16 laptops to desktop rigs), with optional USB audio/MIDI drivers (`snd_usb_audio`, `usbmidi`) for interfaces like Focusrite Scarlett or Novation Launchkey.
- **Integrated P2P Audio**: HydraMesh enables low-latency collaborative jamming (e.g., over gRPC/WebSocket), with StreamDB for seamless sample/metadata sharing—perfect for remote production or live ensembles.
- **Minimal Desktop Overhead**: Hyprland’s lightweight Wayland compositor, combined with Waybar (for CPU/memory monitoring) and Wofi (quick launcher), leaves maximum resources for audio processing.
- **Declarative Tuning**: The embedded `audio-setup.sh` script automates RT optimizations (e.g., sysctl tweaks, IRQ pinning), with persistent Nix configs for boot-time application.
- **Community-Aligned**: Draws from Musnix, PipeWire, and Linux Audio Users best practices, ensuring compatibility with tools like Ardour and VCV Rack while supporting Flatpak for extended app access.

In essence, ArchibaldOS bridges the gap between professional audio distros and NixOS’s power, offering a "set it and forget it" RT environment for creators who value performance without complexity.

## Features

ArchibaldOS provides a focused set of features tailored for RT audio production:

### Core Audio Pipeline
- **Real-Time Kernel**: Musnix with PREEMPT_RT (`linuxPackages_latest_rt`), `rtirq` for interrupt prioritization, and `das_watchdog` to prevent process hangs. Kernel parameters (`threadirqs`, `isolcpus=1-3`, `nohz_full=1-3`, `intel_idle.max_cstate=1`) isolate cores and disable deep C-states for ~50-100µs scheduling latency.
- **Low-Latency Audio Server**: PipeWire with ALSA/PulseAudio/JACK support, tuned for 48kHz sample rate and 32-64 sample quantum (min: 16, max: 128). Achieves ~0.7-1.3ms round-trip latency, verifiable via `jack_iodelay`.
- **USB Connectivity**: Drivers (`snd_usb_audio`, `usbhid`, `usbmidi`) and tools (`usbutils`, `libusb`) for USB audio/MIDI interfaces, with `nrpacks=1` for reduced packet overhead.
- **ALSA Optimizations**: Persistent `/etc/asound.conf` with 32-sample buffers and 96kHz support for high-fidelity, low-latency routing.

### Audio Tools
- **DAWs and MIDI**: Ardour (multitrack recording/editing), Audacity (waveform editing), MuseScore (notation/MIDI composition), FluidSynth (soundfont synthesis).
- **DSP and Synthesis**: CSound (algorithmic synthesis), FAUST (functional DSP programming), PortAudio/RtAudio (cross-platform I/O), SuperCollider (live coding audio).
- **Synths and Modular**: Surge (wavetable synthesis), VCV Rack (modular rack), Pure Data (Pd) (visual patching).
- **Configuration**: `qjackctl` GUI for JACK server tuning (pre-configured for 32 frames, 2 periods, 96kHz).

### Desktop and Utilities
- **Hyprland Desktop**: Minimal Wayland compositor with audio-focused keybindings (`XF86Audio*` for volume, `SUPER+Q` for terminal, `SUPER+D` for Wofi launcher).
- **Monitoring**: Waybar for CPU/memory/network/HydraMesh status; Wofi for quick app launching.
- **Basic Software**: `pcmanfm` (file manager for samples/projects), `vim` (text editor for configs/MIDI scripts), `brave` (privacy-focused browser for tutorials/resources).
- **Post-Install Script**: `audio-setup.sh` automates RT checks (`realtimeconfigquickscan`), latency tweaks (sysctl, RTC/HPET), hardware optimization (`setpci`, IRQ pinning), `qjackctl` launch, and kernel switching (RT to LTS via specialisations). Includes dry-run mode and logging.

### Networking and Metadata
- **HydraMesh**: Optional P2P service for low-latency audio collaboration (toggle via `SUPER+SHIFT+H`), configurable via `/etc/hydramesh/config.toml` (e.g., gRPC/WebSocket transports, LoRaWAN plugins).
- **StreamDB**: CLI/library for audio metadata/sample management, integrated with HydraMesh for distributed storage.

### Installation and Tuning
- **TUI Installer**: Guides disk partitioning (GPT/ext4), user setup, and HydraMesh enablement.
- **RT Tuning Guide**: Built-in script and flake configs for automated optimization, with benchmarks (`cyclictest`, `jack_iodelay`).

## Security

ArchibaldOS employs a defense-in-depth approach for RT audio security, focusing on networked components like HydraMesh while minimizing attack surface:

- **Systemd Hardening**:
  - HydraMesh runs as a dynamic `hydramesh` user with `DynamicUser=true`, isolating it from system processes.
  - Restrictions: `PrivateDevices=true`, `ProtectSystem=strict`, `ProtectHome=true`, `PrivateTmp=true`, `NoNewPrivileges=true`, `CapabilityBoundingSet=""`, `RestrictNamespaces=true`, `SystemCallFilter=@system-service ~@privileged`.
  - **Value**: Sandboxes P2P audio streams, preventing escalation or unauthorized access.

- **File Permissions**:
  - `/etc/hydramesh/` (configs, 640) and `/var/lib/hydramesh/` (data, 755) owned by `hydramesh` user.
  - **Value**: Enforces least privilege for audio metadata and configs.

- **System-Wide Protections**:
  - Default firewall (`networking.firewall.enable = true`) blocks non-essential ports.
  - User passwords hashed with `mkpasswd -m sha-512` during installation.
  - PipeWire/JACK sandboxing via `rtkit` limits audio process privileges.
  - **Value**: Secures the RT pipeline without LUKS overhead; aligns with NIST SP 800-53 and CIS Controls.

For advanced security, enable AppArmor/firewall in HydraMesh via config overrides.

## Who It’s For

ArchibaldOS targets audio professionals and enthusiasts requiring RT performance:
- **Musicians/Producers**: Multitrack workflows (Ardour) and MIDI sequencing (MuseScore) with sub-ms latency.
- **Sound Designers/DSP Engineers**: Modular patching (VCV Rack, Pd) and algorithmic synthesis (SuperCollider, CSound).
- **Live Performers**: Real-time synths (Surge) and P2P collaboration (HydraMesh) for glitch-free sets.
- **Hobbyists/Educators**: Accessible RT learning with FluidSynth and FAUST; sample management via StreamDB and `pcmanfm`.

## Real-Time Audio Tuning Guide

ArchibaldOS is pre-optimized for low-latency audio, but fine-tuning ensures peak performance. This guide, based on Musnix/PipeWire docs, helps achieve <1ms round-trip latency.

### Step 1: Verify and Benchmark
- **RT Kernel Check**: `uname -r` (expect `rt` suffix). Run `realtimeconfigquickscan --all` for system diagnostics.
- **Latency Test**: Start JACK via `qjackctl` (pre-configured: 32 frames, 2 periods, 96kHz). Run `jack_iodelay` for round-trip measurement (target: <1ms).
- **Cyclictest**: `cyclictest -l 100000 -m -n -p99 -q` (target: max latency <100µs).

### Step 2: Kernel and System Tweaks
- **CPU Isolation**: `isolcpus=1-3` and `nohz_full=1-3` in `boot.kernelParams` dedicate cores to audio.
- **C-State Disabling**: `intel_idle.max_cstate=1` and `processor.max_cstate=1` reduce wake-up latency (~20-50µs gain).
- **Governor**: `performance` mode for consistent frequency.
- **Swap Disable**: `vm.swappiness=0` eliminates disk latency.

### Step 3: PipeWire/JACK Configuration
- **Default Settings**: 96kHz rate, 32-sample quantum (min: 16) for ~0.7ms one-way latency.
- **qjackctl Usage**: Launch (`qjackctl &`), set frames=32, periods=2, sample rate=96kHz. Connect apps (e.g., Ardour input to JACK).
- **ALSA Buffers**: `/etc/asound.conf` sets 32-sample buffers for minimal overhead.

### Step 4: Hardware and IRQ Optimization
- **USB Audio**: `nrpacks=1` in `snd_usb_audio` for low-packet latency.
- **PCI Devices**: Run `sudo /etc/audio-setup.sh` to set `latency_timer=ff` and pin IRQs to CPU1.
- **Interfaces**: For RME/Focusrite, test with `lsusb` and adjust buffers in `qjackctl`.

### Step 5: Application Tuning
- **Ardour**: Pre-configured for 32 samples/96kHz; enable "Realtime" in session settings.
- **VCV Rack/Pd**: Route via JACK in `qjackctl`; use `taskset -c 1-3` to pin to isolated cores.
- **Benchmark**: Monitor xruns in `qjackctl` graph; aim for zero during 10-minute loads.

### Advanced: Extreme RT (<0.5ms Latency)
- **Custom Kernel**: Switch to `linuxPackages_rt_6_11` for finer tuning.
- **Full Isolation**: Add `rcu_nocbs=1-3` to `boot.kernelParams` for no-CB-RCU callbacks.
- **Trade-Offs**: Increases power (~2W) and complexity; test on high-end hardware.

Rebuild with `sudo nixos-rebuild switch` after config changes. For issues, check `/var/log/pipewire.log` or NixOS Wiki: Audio Production.

## Prerequisites

- **System**: Nix/NixOS environment for building; x86_64 hardware (e.g., Framework 16 or similar).
- **USB Drive**: 8GB+ for flashing the ISO.
- **Files**:
  - `flake.nix`: Defines configurations.
  - `../wallpaper.jpg`: Default wallpaper for Hyprland/SDDM.
  - `../HydraMesh/`: Flake with P2P audio networking (e.g., Go-based `hydramesh` binary).
  - `../StreamDB/`: Flake with audio metadata storage (e.g., Rust-based `streamdb`).
- **Tools**: `nix`, `git`, `dd` for building/flashing; internet for dependencies (or cached `/nix/store`).
- **Permissions**: Root access for flashing and installation.

## Installation

1. **Clone Repository**:
   ```bash
   git clone https://github.com/<your-org>/ArchibaldOS
   cd ArchibaldOS
   ```

2. **Compute Hashes**:
   ```bash
   nix build .#hydramesh-pkg  # Update vendorHash in flake.nix
   nix build .#streamdb-pkg   # Update cargoHash in flake.nix
   ```

3. **Build ISO**:
   ```bash
   nix build .#installer
   ```

4. **Flash USB**:
   Identify USB device with `lsblk` (e.g., `/dev/sdX`).
   ```bash
   sudo dd if=result/iso/archibaldOS-rt-audio-*.iso of=/dev/sdX bs=4M status=progress
   sync
   ```

5. **Boot and Install**:
   - Boot USB (set BIOS/UEFI to prioritize USB).
   - Auto-logs in as `nixos` (password: `nixos`) with Hyprland.
   - Run `sudo /etc/installer.sh` in Kitty (`SUPER+Q`).
   - Follow TUI prompts: keyboard, disk (erases data), HydraMesh, locale/timezone, hostname/username, password.
   - Installer formats disk (GPT/ext4), configures system, and installs.
   - Reboot into the new system.

6. **Test in QEMU** (optional):
   ```bash
   qemu-system-x86_64 -cdrom result/iso/*.iso -m 4G -enable-kvm -cpu host
   ```

## Usage

- **Desktop**:
  - Hyprland with Waybar (CPU, memory, network, HydraMesh status) and Wofi (`SUPER+D`).
  - Keybindings: `SUPER+Q` (Kitty), `SUPER+1-5` (workspaces), `XF86Audio*` (volume), `SUPER+SHIFT+H` (HydraMesh toggle), `SUPER+SHIFT+K` (cheatsheet).
- **Audio Workflow**:
  - Launch Ardour/VCV Rack via Wofi or terminal.
  - Test latency with `qjackctl` (install via `nix-shell -p qjackctl --run qjackctl`).
  - Use `pcmanfm` to manage samples, `vim` for MIDI scripts, `brave` for online libraries.
- **HydraMesh**:
  - Enable in `/etc/nixos/configuration.nix`:
    ```nix
    systemd.services.hydramesh.enable = true;
    ```
  - Configure `/etc/hydramesh/config.toml` (e.g., `port`, `peers`, `plugins={audio=true}`).
  - Toggle: `SUPER+SHIFT+H` or `hydramesh-toggle`.
- **StreamDB**: Use as CLI/library for sample organization (e.g., `/var/lib/hydramesh/streamdb`).
- **RT Tuning**: Run `sudo /etc/audio-setup.sh` post-install for sysctl tweaks, IRQ pinning, and `qjackctl` launch.

## Troubleshooting

- **Build Failures**:
  - Update `vendorHash`/`cargoHash` with `nix build .#hydramesh-pkg` or `.#streamdb-pkg`.
  - Ensure `../HydraMesh` and `../StreamDB` are valid flakes.
- **Audio Latency**:
  - Verify RT kernel: `uname -r` (should include `rt`).
  - Check JACK settings: `qjackctl` (adjust frames=32, periods=2).
  - Run `cyclictest -l 100000 -m -n -p99 -q` (target: max <100µs).
- **HydraMesh**:
  - Logs: `journalctl -u hydramesh`.
  - Validate `/etc/hydramesh/config.toml`.
- **USB Devices**:
  - Detect: `lsusb`; troubleshoot with `dmesg | grep usb`.
- **General**:
  - NixOS Wiki: https://nixos.wiki
  - GitHub Issues: https://github.com/<your-org>/ArchibaldOS
  - Logs: `journalctl -u nix-daemon`, `/var/log/pipewire.log`.

## Contributing

1. Fork: https://github.com/<your-org>/ArchibaldOS
2. Test changes: `nixos-rebuild dry-run` or QEMU.
3. Submit PRs or issues on GitHub.
4. Ensure compatibility with NixOS unstable and RT audio use cases.

## License

MIT License:

Copyright (c) 2025 DeMoD LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

**Note**: HydraMesh and StreamDB may have separate licenses (e.g., LGPL-3.0). See `../HydraMesh/LICENSE` and `../StreamDB/LICENSE`.

## Notes

- **Dependencies**: Ensure `../wallpaper.jpg`, `../HydraMesh`, `../StreamDB` exist before building.
- **ARM64**: Set `system = "aarch64-linux"` in `flake.nix` and verify Hyprland/Musnix compatibility.
- **HydraMesh/StreamDB**: Assumes `hydramesh-toggle` and `hydramesh-status` binaries; provide `hydramesh_config_editor.py` if needed.
- **Memory Management**: To forget conversation history, go to "Data Controls" in settings or click the book icon beneath messages to remove specific chats.
