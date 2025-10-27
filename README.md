# ArchibaldOS: Lean Real-Time Audio NixOS Distribution

**ArchibaldOS** is a streamlined, real-time (RT) audio-focused Linux distribution based on NixOS, derived from the Oligarchy NixOS framework. Optimized for musicians, sound designers, and DSP researchers, it prioritizes low-latency audio processing, MIDI workflows, and modular synthesis on x86_64 hardware. Built with the Musnix real-time kernel, a minimal Hyprland Wayland desktop, and integrations for HydraMesh (P2P audio networking) and StreamDB (audio metadata storage), ArchibaldOS delivers a lightweight, reproducible, and high-performance environment. A Text User Interface (TUI) installer simplifies setup, and essential utilities (file manager, text editor, browser) ensure basic functionality without compromising its audio-centric design.

## Why ArchibaldOS?

ArchibaldOS is engineered for real-time audio production, offering:
- **Ultra-Low Latency**: Musnix’s PREEMPT_RT kernel and PipeWire/JACK deliver sub-millisecond audio performance for live recording, synthesis, and performance.
- **Minimal Footprint**: A curated set of ~18 packages (audio + utilities) minimizes resource usage, ideal for resource-constrained laptops or studio rigs.
- **Hardware Agnosticism**: Generic configurations ensure compatibility across x86_64 systems, including high-performance laptops like the Framework 16.
- **Reproducible Builds**: NixOS’s declarative model and flake-based inputs guarantee consistent setups across deployments.
- **Lightweight Desktop**: Hyprland with Waybar and Wofi provides a fast, Wayland-based interface, leaving CPU/GPU resources for audio tasks.
- **P2P Audio Networking**: Optional HydraMesh enables low-latency collaborative audio workflows, with StreamDB for sample/metadata management.
- **Streamlined Installation**: A TUI installer (no Calamares) configures disk, user, and audio settings with minimal overhead.
- **Basic Utilities**: Includes `pcmanfm` (file manager), `vim` (text editor), and `brave` (browser) for essential tasks like managing samples or accessing online resources.

## Features

- **Real-Time Audio**:
  - Musnix with `PREEMPT_RT` kernel (`linuxPackages_latest_rt`) for sub-millisecond latency.
  - PipeWire with ALSA, PulseAudio, and JACK support for professional audio workflows.
  - `rtirq` prioritizes audio interrupts; `alsaSeq` enables MIDI sequencing.
- **Minimal Desktop**:
  - Hyprland Wayland compositor with essential keybindings (`SUPER+Q` for terminal, `SUPER+D` for launcher, `XF86Audio*` for volume).
  - Waybar displays CPU, memory, network, and HydraMesh status; Wofi launches audio tools.
- **Audio Tools**:
  - DAWs: Ardour (multitrack), Audacity (editing), MuseScore (MIDI notation), FluidSynth (soundfont synthesis).
  - DSP: CSound, FAUST, PortAudio, RtAudio, SuperCollider for synthesis and processing.
  - Synths/Modular: Surge, VCV Rack, Pure Data (Pd) for creative sound design.
- **P2P and Metadata**:
  - Optional HydraMesh for P2P audio networking (toggle via `SUPER+SHIFT+H`).
  - StreamDB for audio metadata/sample storage, integrated as a CLI/library.
- **TUI Installer**: Guides through keyboard, disk (GPT/ext4, no LUKS), HydraMesh, locale/timezone, hostname/user, and password in a minimal TUI.
- **Customization**: Keybindings cheatsheet (`SUPER+SHIFT+K`) and wallpaper (`wall.jpg`) for a tailored UX.

## Security

ArchibaldOS prioritizes security for its networked audio components, particularly HydraMesh, following best practices (e.g., NIST SP 800-53, CIS Controls):
- **Systemd Hardening**:
  - HydraMesh runs as a `hydramesh` user with `DynamicUser=true`, isolating it from system processes.
  - Restrictions: `PrivateDevices=true`, `ProtectSystem=strict`, `ProtectHome=true`, `PrivateTmp=true`, `NoNewPrivileges=true`, `CapabilityBoundingSet=""`, `RestrictNamespaces=true`, `SystemCallFilter=@system-service ~@privileged`.
  - **Value**: Sandboxes P2P audio networking, preventing escalation or system access.
- **File Permissions**:
  - `/etc/hydramesh/` (configs, 640) and `/var/lib/hydramesh/` (data, 755) owned by `hydramesh` user.
  - **Value**: Enforces least privilege for audio metadata and configs.
- **System-Wide**:
  - Default firewall (`networking.firewall.enable = true`) protects the system.
  - User passwords hashed with `mkpasswd -m sha-512`.
  - **Value**: Secures the broader system without LUKS overhead.

## Who It’s For

- **Musicians/Producers**: Multitrack recording (Ardour), MIDI sequencing (MuseScore), and live mixing with low-latency JACK.
- **Sound Designers/DSP Engineers**: Modular synthesis (VCV Rack, Pd), algorithmic composition (SuperCollider), and DSP programming (FAUST).
- **Live Performers**: Real-time synths (Surge) and P2P jamming (HydraMesh) for low-latency workflows.
- **Hobbyists/Educators**: Learn MIDI/DSP with FluidSynth and CSound; manage samples with StreamDB and `pcmanfm`.

## Real-Time Audio Tuning Guide

ArchibaldOS is pre-configured for low-latency audio via Musnix and PipeWire, but fine-tuning can optimize performance for specific hardware and workflows. This guide draws from official Musnix documentation, NixOS Wiki, and community resources to help achieve sub-millisecond latency. Always test changes in a live environment before applying to your installed system.

### Step 1: Verify RT Kernel and Basics
- **Check Kernel**: After installation, run `uname -r` to confirm the RT suffix (e.g., `6.6.52-rt64`). If not, ensure `musnix.kernel.realtime = true;` in your `configuration.nix` and rebuild with `sudo nixos-rebuild switch`.
- **Enable Musnix Options**: In `configuration.nix`, add:
  ```nix
  musnix = {
    enable = true;
    kernel.realtime = true;
    alsaSeq.enable = true;  # For MIDI
    rtirq.enable = true;    # Prioritize audio interrupts
  };
  ```
  - **Logic**: `rtirq` assigns high priority to audio threads (e.g., IRQ 18 for sound cards), reducing xruns (buffer underruns).
- **Rebuild and Reboot**: `sudo nixos-rebuild switch` and reboot.

### Step 2: Configure PipeWire and JACK
- **PipeWire Setup**: ArchibaldOS uses PipeWire by default. For RT tuning, edit `/etc/pipewire/pipewire.conf` (or override via Nix):
  ```nix
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    jack.enable = true;
    extraConfig.pipewire."92-low-latency" = {
      "context.properties" = {
        "default.clock.rate" = 48000;  # Sample rate (higher for quality, lower for latency)
        "default.clock.quantum" = 64;  # Buffer size (smaller = lower latency, but risk xruns)
        "default.clock.min-quantum" = 32;
        "default.clock.max-quantum" = 128;
      };
    };
  };
  ```
  - **Logic**: Lower `quantum` (buffer size) reduces latency but increases CPU load. Start at 64 and test.
- **JACK Integration**: Use `qjackctl` (install via `nix-shell -p qjackctl` for testing):
  - Set frame/period to 64/2 (low latency) and sample rate to 48000.
  - Connect apps (e.g., Ardour) to JACK outputs.
- **Test Latency**: Run `jack_iodelay` (via `nix-shell -p jack2`) and aim for <5ms round-trip.

### Step 3: Optimize System Settings
- **IRQ Priorities**: Edit `/etc/musnix/rtirq.conf` (created by Musnix):
  ```ini
  RTIRQ_NAME_LIST="timer rtc snd usb i915 amdgpu"  # Prioritize audio/USB/GPU
  RTIRQ_PRIO_HIGH=95
  RTIRQ_PRIO_DECR=5
  ```
  - Rebuild and check with `rtirq status`.
- **Governor and Limits**: Set CPU governor to `performance` for consistent RT:
  ```nix
  powerManagement.cpuFreqGovernor = "performance";
  ```
  - Increase audio group limits in `/etc/security/limits.d/audio.conf`:
    ```
    @audio - rtprio 99
    @audio - memlock unlimited
    @audio - nice -20
    ```
- **Disable Interfering Services**: In `configuration.nix`:
  ```nix
  systemd.services.NetworkManager-wait-online.enable = false;  # Speeds boot, reduces interference
  ```

### Step 4: Monitoring and Troubleshooting
- **Tools**: Use `htop` or `rtirq status` to monitor priorities; `pw-top` for PipeWire stats; `jack_midi_dump` for MIDI testing.
- **Common Issues**:
  - **Xruns**: Increase buffer size (`quantum`) or disable Wi-Fi/Bluetooth during sessions.
  - **High CPU**: Lower sample rate or use `nice` on non-audio processes.
  - **MIDI Dropouts**: Ensure `alsaSeq.enable = true;` and test with `aconnect -lio`.
- **Benchmark**: Connect a MIDI controller, run Ardour with effects, and monitor latency with `jack_iodelay`. Target <2ms for professional RT.
- **Resources**: Refer to [Musnix GitHub](https://github.com/musnix/musnix) for advanced configs; [NixOS Wiki: Audio Production](https://wiki.nixos.org/wiki/Audio_production) for PipeWire tips.

For custom tuning, edit `configuration.nix` and rebuild. Test in the live ISO to avoid disrupting your setup.

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

5. **Boot and Install**: Boot USB (set BIOS/UEFI to prioritize USB).
   - Auto-logs in as `nixos` (password: `nixos`) with Hyprland.
   - Run `sudo /etc/installer.sh` in Kitty (`SUPER+Q`).
   - Follow TUI prompts (keyboard, disk, HydraMesh, locale/timezone, hostname/username, password).
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

## Troubleshooting

- **Build Failures**:
  - Update `vendorHash`/`cargoHash` with `nix build .#hydramesh-pkg` or `.#streamdb-pkg`.
  - Ensure `../HydraMesh` and `../StreamDB` are valid flakes.
- **Audio Latency**:
  - Verify RT kernel: `uname -r` (should include `rt`).
  - Check JACK settings: `nix-shell -p qjackctl --run qjackctl`.
- **HydraMesh**:
  - Logs: `journalctl -u hydramesh`.
  - Validate `/etc/hydramesh/config.toml`.
- **General**:
  - NixOS Wiki: https://nixos.wiki
  - GitHub Issues: https://github.com/<your-org>/ArchibaldOS
  - Logs: `journalctl -u nix-daemon`

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
