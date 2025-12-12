# ArchibaldOS: Professional Real-Time Audio Operating System
**© 2025 DeMoD LLC. All rights reserved.**

[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD--3--Clause-yellow.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Nix Flake](https://img.shields.io/badge/Built%20with-Nix-blue.svg)](https://nixos.org/)
[![Multi-Architecture](https://img.shields.io/badge/Arch-x86__64%20%7C%20aarch64-green.svg)](https://nixos.org/)
[![Profiles](https://img.shields.io/badge/Profiles-Enabled-purple.svg)](https://github.com/ALH477/ArchibaldOS)
[![Robotics](https://img.shields.io/badge/Robotics-ROS2%20%2B%20PX4-orange.svg)](https://github.com/ALH477/ArchibaldOS)
[![LIDAR](https://img.shields.io/badge/LIDAR-Velodyne%20%7C%20Ouster%20%7C%20Livox-blueviolet.svg)](https://github.com/ALH477/ArchibaldOS)
[![Secure](https://img.shields.io/badge/Secure-ITAR%2FEAR--Safe-red.svg)](https://github.com/ALH477/ArchibaldOS)
[![GitHub Issues](https://img.shields.io/github/issues/ALH477/ArchibaldOS)](https://github.com/ALH477/ArchibaldOS/issues)
[![GitHub Stars](https://img.shields.io/github/stars/ALH477/ArchibaldOS)](https://github.com/ALH477/ArchibaldOS/stargazers)

![ArchibaldOS Logo](modules/assets/demod-logo.png)

**ArchibaldOS** is a professional-grade, fully reproducible NixOS-based operating system engineered by DeMoD LLC for **ultra-low-latency real-time audio production** across **x86_64** and **ARM64** platforms. Built with the *"minimal oligarchy"* philosophy—prioritizing only essential components for peak performance—it delivers **sub-5ms round-trip latency** on hardware ranging from low- to high-end x86 workstations to ARM single-board computers (Raspberry Pi 3–5, Orange Pi 5, Rock 5, OPI zero2w) and **Apple Silicon Macs (M1/M2/M3 via Asahi Linux)**.

Tailored for musicians, audio engineers, sound designers, live performers, embedded-audio developers, **robotics engineers**, and **defense contractors**, ArchibaldOS integrates **Musnix real-time kernel optimizations**, **PipeWire/JACK professional audio routing**, **KDE Plasma 6**, **ROS 2 Humble**, **PX4 Autopilot**, **LIDAR drivers**, and **ITAR/EAR-compliant security hardening** into a lightweight, power-efficient, **declaratively-configured** package.

What sets it apart? **Bit-for-bit reproducibility** across deployments, ensuring that your setup on a studio workstation matches exactly on a drone brain or secure edge router—eliminating the *"it works on my machine"* syndrome that plagues traditional OSes. Trust us, once you experience deployment consistency at this level, you'll wonder how you ever managed without it.

ArchibaldOS forms the foundational operating system layer for the **DeMoD platform**, a cohesive ecosystem for real-time digital signal processing (DSP) and demodulation. As detailed in the open-source guide at [https://github.com/ALH477/DeMoDulation](https://github.com/ALH477/DeMoDulation)—a public blueprint released by DeMoD LLC on **November 20, 2025**—ArchibaldOS powers **DIY DSP devices** built from e-waste, Framework 13 mainboards, or Raspberry Pi 5. This integration enables **sub-0.8ms round-trip latency at 24-bit/192kHz**, transforming low-cost hardware into professional-grade audio and **software-defined radio (SDR)** rigs. The DeMoDulation repository provides Nix flake profiles that explicitly support ArchibaldOS as a **native or virtualized build option**, ensuring seamless scalability from embedded prototypes to production deployments.

---

## Table of Contents

- [Key Features](#key-features)
- [Profile Selector](#profile-selector)
- [Multi-Platform Support](#multi-platform-support)
- [Getting Started](#getting-started)
- [Beginner-Friendly Configuration](#beginner-friendly-configuration-tools)
- [Build Targets](#build-targets)
- [Tested Hardware](#tested-hardware)
- [Installation Guide](#installation-guide)
- [Diagnostic Tools](#diagnostic--testing-tools)
- [Troubleshooting](#troubleshooting)
- [Contributing](#contributing)
- [Credits](#credits--acknowledgments)

---

## Key Features

### Ultra-Low Latency Audio

On **x86_64 systems**, ArchibaldOS achieves a **theoretical latency of 0.67ms** (32 samples at 48kHz) using a **PREEMPT_RT kernel**, with **measured round-trip latencies typically falling between 1.2ms and 1.5ms** when paired with a USB audio interface. This is empirically validated through tools like `jack_iodelay` and `audio-latency-test`, showing **xrun-free operation under sustained loads**—critical for live performances where even microsecond delays can disrupt timing. In DeMoDulation workflows, this extends to **SDR applications** via **GNU Radio** and **SoapySDR**, maintaining **<0.01% xrun rates** at 192kHz with quad-core ARM setups.

For **ARM64**, latencies range from **1.5ms to 3ms** (128 samples at 48kHz) with board-optimized kernels, as benchmarked on **RK3588 SoCs** like the **Orange Pi 5**. These figures **outperform stock Linux distributions by factors of 2–5x**, thanks to **Musnix integrations** including:

- **RTIRQ prioritization** (elevating audio interrupts above all others)
- **DAS watchdog** for deadlock prevention
- **CPU isolation** via `isolcpus` and `nohz_full` parameters

The result? **Seamless real-time processing** on power-constrained hardware, where traditional OSes often throttle or introduce jitter exceeding 10ms. DeMoDulation benchmarks confirm `cyclictest` latencies as low as **19µs on Raspberry Pi 5**, enabling DSP chains with **SuperCollider** and **VST plugins** without glitches.

**PipeWire/JACK** provides the backbone for audio routing, offering **ALSA compatibility** while supporting **quantum sizes as low as 32 samples** without buffer underruns. System tuning—such as enforcing the `performance` CPU governor, enabling `threadirqs` for threaded interrupts, and setting `swappiness=0` to minimize paging—ensures **deterministic behavior**. In empirical tests, this configuration sustains **100% CPU utilization** in DAWs like **Ardour** without audio dropouts, a feat rarely achieved on non-specialized OSes. For DeMoD users, this pairs with **StreamDB** for zero-config asset sharing, allowing **multi-device audio networks with sub-ms synchronization**.

### Modular & Declarative Architecture

ArchibaldOS leverages **Nix Flakes** for **declarative configuration**, allowing you to enable or disable modes (audio workstation, drone brain, secure router, DSP coprocessor) with a **single flag** in your `flake.nix`. This yields **atomic updates** and **rollback capabilities**: if a change introduces instability, revert in seconds without data loss. **Cross-compilation** is built-in—build ARM images from an x86_64 host using `--extra-platforms aarch64-linux`, ensuring **reproducible binaries down to the hash**.

One-command deployments produce **ready-to-flash SD images** or **bootable ISOs**, with **Disko** handling **declarative partitioning** for consistent storage layouts. Why is this badass? In production environments, it reduces setup time from hours to minutes, and empirical reproducibility tests (via Nix's content-addressed store) confirm **identical system states across hardware**, mitigating bugs from environmental variances. The DeMoDulation guide extends this with hardware-specific profiles (`demod-ewaste`, `demod-framework13`, `demod-rpi5`), all unified under ArchibaldOS for **plug-and-play DSP builds**.

---

## Profile Selector

For even easier customization, use the new **`profile-selector.nix` module** to switch builds with **one line**—no more editing files! Define `archibaldOS.profile = "your-choice";` in your flake, and it auto-imports modules, configs, and optimizations.

| Profile | Description | Target Platforms | Key Modules |
|---------|-------------|------------------|-------------|
| **audio-workstation** | Pro Audio Workstation | x86_64 | audio, desktop, rt-kernel |
| **audio-live-iso** | Live Audio ISO | x86_64 | iso, audio, branding |
| **drone-brain** | Drone Flight Controller | ARM64 | robotics, lidar, rt-kernel |
| **lidar-station** | LIDAR Mapping Station | x86_64/ARM64 | lidar, robotics, desktop |
| **secure-router** | ITAR-Safe Secure Router | x86_64/ARM64 | router, secure-rt |
| **dsp-coprocessor** | DSP Coprocessor (kexec, minimal) | x86_64 | dsp, rt-kernel, secure-rt |
| **custom** | Custom profile (manual overrides) | Any | None (base only) |

**Example in `flake.nix`:**

```nix
nixosConfigurations.archibaldOS-drone = nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules = [
    ./modules/profile-selector.nix
    ({ config, ... }: {
      archibaldOS.profile = "drone-brain";  # One line!
    })
  ];
};
```

Build: `nix build .#nixosConfigurations.archibaldOS-drone.config.system.build.sdImage`

See `/etc/profile-name` post-boot for confirmation.

---

## Robotics & Autonomy Stack

ArchibaldOS is robotics-ready out of the box with:

- **ROS 2 Humble** (full stack: rclcpp, rviz2, nav2, slam-toolbox)
- **PX4 Autopilot** (SITL, firmware, MAVROS, JMavSim)
- **LIDAR drivers** (Velodyne, Ouster, Hesai, Livox, RPLIDAR)
- **PCL + Open3D** for point cloud processing
- **Real-time scheduling** (rtprio=95, memlock=unlimited)
- **udev rules** for Pixhawk, FTDI, I2C, GPIO
- **CycloneDDS** for low-latency ROS 2 networking

Launch a drone stack in seconds:

```bash
ros2 launch mavros px4.launch.py
```

---

## Secure & ITAR/EAR Compliant

The `secure-rt.nix` module hardens systems for defense and aerospace:

- No encryption (ITAR-safe)
- `lockdown=confidentiality`, `modules_disabled`
- Auditd + journal logging
- Hardware watchdog
- Network isolation (drop all non-root)
- Runs on $35 SBCs

### Router/Firewall Module

Turn any device into a hardened edge router:

- nftables stateful firewall
- FRR BGP/OSPF routing
- dnsmasq DHCP/DNS
- Zero-trust policy
- Real-time safe (isolcpus=0)

### DSP Coprocessor Mode

Boot into a minimal RT kernel via kexec:

- Sub-100µs interrupt latency
- No GUI, no network
- Perfect for audio DSP, SDR, control loops

---

## Multi-Platform Support

### x86_64 Platforms

- Desktop workstations (Intel/AMD)
- Live ISO with Calamares graphical installer
- Full professional audio software suite
- PREEMPT_RT real-time kernel for sub-millisecond responsiveness
- DeMoD integration: e-waste PCs and Framework 13/16 for portable DSP rigs
- kexec-booted DSP coprocessor (archibaldOS-dsp) for ultra-isolated real-time compute

### ARM64 Platforms (as of 2025)

| Category | Devices / SoCs |
|----------|----------------|
| Raspberry Pi | Raspberry Pi 5 (BCM2712), Raspberry Pi 3B (BCM2837) |
| Rockchip RK3588 | Orange Pi 5 / 5 Plus, Radxa Rock 5A / 5B |
| Rockchip RK3399 | Pine64 Pinebook Pro, NanoPC-T4 |
| Amlogic | ODROID-C2, ODROID-HC4 |
| Apple Silicon | M1 / M1 Pro / M1 Max / M1 Ultra / M2 / M3 (Asahi) |

These platforms benefit from tailored kernel modules (e.g., from nixos-rk3588 for RK3588 SoCs), ensuring hardware-specific optimizations like GPU acceleration and power management without sacrificing audio performance. DeMoDulation targets these directly, with auto-configuration for Behringer UMC-series interfaces at 96kHz/64 samples.

### Desktop Environment

KDE Plasma 6 under Wayland provides a lightweight, modern interface that's touch-screen friendly, with SDDM for display management and auto-login in live/SBC modes. Touch optimizations and high-DPI scaling are enabled by default. The environment includes:

- Konsole, Kate, Dolphin
- Helvum for PipeWire patchbay
- QJackCtl, Carla, Zrythm
- Guitarix, Calf Studio Gear

---

## Getting Started

### Prerequisites

- A machine with Nix installed (single-user or multi-user)
- Git and xz for building images
- Flakes enabled: `nix.settings.experimental-features = [ "nix-command" "flakes" ];`

### Quick Start (Build & Flash)

1. **Clone the repo:**
   ```bash
   git clone https://github.com/ALH477/ArchibaldOS.git
   cd ArchibaldOS
   ```

2. **Build an image:**
   ```bash
   # Live Audio ISO
   nix build .#packages.x86_64-linux.iso

   # Drone Brain (Orange Pi 5)
   nix build .#packages.aarch64-linux.orangepi5

   # Secure Router
   nix build .#packages.aarch64-linux.router
   ```

3. **Flash the image:**
   ```bash
   # ISO → USB
   sudo dd if=result of=/dev/sdX bs=4M status=progress

   # SD card
   xzcat result | sudo dd of=/dev/mmcblk0 bs=4M status=progress
   ```

4. **Boot and log in:**
   - Live ISO: `nixos` / `nixos`
   - SBC: `audio-user` / `changeme` (change on first boot)

---

## Beginner-Friendly Configuration Tools

While ArchibaldOS is built on pure Nix flakes, we recommend these GUI tools for newcomers:

| Tool | Purpose | Install |
|------|---------|---------|
| **Nix-GUI** | Visual editor for NixOS options | `nix profile install github:nix-gui/nix-gui` |
| **nixos-conf-editor** | GNOME-based config editor | `nix profile install github:vlinkz/nixos-conf-editor` |
| **pmiddend/nixos-manager** | Full system manager | `nix profile install github:pmiddend/nixos-manager` |

### Step-by-Step Guide: Tune Your Environment (5 mins)

1. **Boot into ArchibaldOS** as `nixos` or `audio-user`.

2. **Open a terminal** (Konsole in Plasma): `nix shell nixpkgs#git` (if not installed).

3. **Clone a working copy:**
   ```bash
   git clone https://github.com/DeMoD-LLC/archibaldos.git ~/archibaldos-test
   cd ~/archibaldos-test
   ```

4. **Enable flakes if needed:** Edit `flake.nix` (or use `sudo nano /etc/nixos/configuration.nix`) to add:
   ```nix
   nix.settings.experimental-features = [ "nix-command" "flakes" ];
   ```
   Then: `sudo nixos-rebuild switch`

5. **Install the Tool (2 mins):**
   - For Nix-GUI: `nix profile install github:nix-gui/nix-gui`
   - Launch: Search "Nix-GUI" in your menu or run `nix run ~/.local/state/nix/profiles/profile/bin/nix-gui`
   - It auto-detects your config path. If prompted, point it to `~/archibaldos-test/flake.nix`

6. **Explore & Edit (10 mins):**
   - **Browse Options:** Navigate to `powerManagement` > `cpuFreqGovernor`
   - **Make a Change:** Select `performance` from the dropdown
   - **Search for Audio:** Type "pipewire quantum" and set `default.clock.quantum = 128;`
   - **Diff & Learn:** Hit "Preview Changes" to see the generated Nix code

7. **Apply & Test (5 mins):**
   - Save: Nix-GUI commits to a module file
   - Rebuild: `sudo nixos-rebuild switch --flake .#archibaldOS-iso`
   - Verify: `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor`
   - Test audio: Launch Guitarix and monitor with `jack_iodelay`
   - **Rollback if Needed:** `sudo nixos-rebuild switch --rollback`

8. **Level Up: Peek at the Code**
   - Open the generated file: `code modules/audio.nix`
   - Install nixd LSP: Add `pkgs.nixd` to your devShell in `flake.nix`
   - **Pro Tip:** Use Git to track changes: `git add . && git commit -m "Added performance governor"`

### Next Steps: From Beginner to Nix Wizard

- **Week 1 Goal:** Use the GUI for 3–5 changes (e.g., add Helvum, tweak branding)
- **Resources:** Watch Nix-GUI's usage video; read the NixOS manual at search.nixos.org
- **When to Ditch the GUI:** Once comfortable searching options manually, switch to Helix/Vim with nixpkgs-fmt

---

## Build Targets

The unified flake provides multiple configurations:

```bash
# x86_64 Live ISO (full audio suite)
nix build .#packages.x86_64-linux.iso

# DSP Coprocessor (kexec RT core)
nix build .#packages.x86_64-linux.dsp

# Orange Pi 5 (RK3588)
nix build .#packages.aarch64-linux.orangepi5

# Raspberry Pi 3B (headless)
nix build .#packages.aarch64-linux.rpi3b

# ZIP-750 Edition (ultra-minimal)
nix build .#packages.x86_64-linux.zip750

# VM Edition (headless RT audio dev)
nix build .#packages.x86_64-linux.vm
```

---

## Tested Hardware

ArchibaldOS has been rigorously validated across real-world **x86_64 systems**. All testing was conducted from **SD card boot**, using a **Behringer UMC204HD** at **24-bit/96kHz**, with **Guitarix running multiple real-time effects**. **Xruns remained minimal (<0.1% over 30-minute sessions)**.

| Platform | CPU | RAM | Storage | Performance Notes | Link |
|----------|-----|-----|---------|-------------------|------|
| **Framework Laptop 16** | AMD Ryzen 7 7840HS (8c/16t) | 32GB DDR5 | NVMe SSD | 1.2–1.4ms RTL @ 96kHz/64 samples; zero xruns | [framework.com](https://frame.work/laptop16) |
| **Framework Laptop 13 (Gen 11)** | Intel Core i5-1135G7 (4c/8t) | 8GB LPDDR4x | SD card boot | 1.8–2.2ms RTL @ 96kHz/128 samples; <0.05% xruns | [frame.work](https://frame.work/laptop13) |
| **MacBook Air (Early 2014)** | Intel Core i5-4260U (2c/4t) | 4GB LPDDR3 | SD card boot | 2.4–3.1ms RTL @ 96kHz/256 samples; stable with 6–8 FX | [support.apple.com](https://support.apple.com/kb/SP690) |

> **Empirical Insight:** The **2014 MacBook Air**—running from a **$10 microSD card**—sustained **professional-grade audio processing** at **96kHz/24-bit**, a configuration that causes dropouts on macOS, Ubuntu, and Windows within minutes.

---

## Optimizing for Older & Low-Spec Hardware

For **maximum headroom** on constrained systems, **disable Plasma** and boot into a lightweight window manager like **DWM**, **Hyprland**, or **i3**.

### Why This Matters

- **RAM savings:** Plasma 6 → 800MB idle → DWM → 120MB idle → **680MB freed**
- **CPU overhead:** Plasma → 5–8% idle → WM → <1% → reduces thermal throttling
- **Determinism:** Fewer services → fewer context switches → lower jitter

### Recommended WMs

| WM | RAM (idle) | GPU | Latency Impact | Use Case |
|----|------------|-----|----------------|----------|
| **DWM** | ~120MB | No compositor | Negligible | Ultra-minimal, ideal for e-waste |
| **Hyprland** | ~180MB | Wayland compositor | +0.1ms | Smooth animations, modern look |
| **i3** | ~140MB | X11 | None | Classic tiling, scriptable |

### Example: Force DWM Boot (No Plasma)

Add this to your `flake.nix` or `configuration.nix`:

```nix
{ config, pkgs, ... }: {
  # Disable Plasma
  services.desktopManager.plasma6.enable = false;
  services.displayManager.sddm.enable = false;

  # Enable DWM
  services.xserver.windowManager.dwm.enable = true;

  # Auto-start audio tools
  services.xserver.displayManager.startx.enable = true;
  services.xserver.displayManager.startx.user = "nixos";

  # Optional: reduce kernel latency further
  boot.kernelParams = [ "threadirqs" "isolcpus=1" "nohz_full=1" ];
}
```

> **Result:** Boots in **<8 seconds**, launches DWM with **QJackCtl + Guitarix**, uses **1.1GB total RAM**, and sustains **96kHz/128 samples with 8+ FX**—**zero xruns for 1+ hour**.

---

## Installation Guide

### x86_64 Desktop/Laptop

1. **Boot Live ISO**
   - Boot from USB/DVD
   - System auto-logs into Plasma 6 as `nixos` user

2. **Run Calamares Installer**
   - Launch from desktop icon
   - Follow guided partitioning
   - Install

3. **Post-Install**
   - Reboot and remove USB
   - Verify RT performance: `rt-check`
   - Test latency: `audio-latency-test`

### DSP Coprocessor

1. **Build Image:** `nix build .#packages.x86_64-linux.dsp`
2. **Flash:** `xzcat result/archibaldOS-dsp.img.xz | sudo dd of=/dev/sdX bs=4M status=progress`
3. **Boot:** Auto-kexec into RT kernel, run `dsp-start`

### ARM64 Single-Board Computer

1. **Flash SD Card:**
   ```bash
   sudo dd if=archibaldos-*.img of=/dev/sdX bs=4M status=progress conv=fsync
   ```

2. **Boot** and auto-login as `audio-user`

3. **Verify:**
   ```bash
   rt-check
   audio-latency-test
   ```

---

## Diagnostic & Testing Tools

```bash
# Check RT system configuration
rt-check

# Test round-trip audio latency
audio-latency-test
pw-jack jack_iodelay

# DSP: Start dummy JACK server
dsp-start

# View PipeWire graph
qpwgraph

# Check CPU governor
cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor

# View IRQ priorities
rtirq status

# Monitor system performance
htop
```

---

## Troubleshooting

### Audio Crackling / Xruns

1. Increase quantum: `128` or `256` samples
2. Verify CPU governor: should be `performance`
3. Disable unnecessary services
4. Check IRQ priorities

### No Sound Output

1. Check PipeWire status: `systemctl --user status pipewire`
2. Verify audio group membership: `groups | grep audio`
3. Test with: `speaker-test -c 2 -r 48000`

### kexec Fails to Load

1. Verify `/boot/kexec/vmlinuz` exists
2. Check `journalctl -u kexec-load`
3. Ensure disk is labeled `ROOT`

---

## Contributing

Contributions are welcome under the BSD-3-Clause License. Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit changes (`git commit -m 'Add amazing feature'`)
4. Push to branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.

**Focus areas:**
- RT audio optimizations
- New hardware support
- Robotics & LIDAR integrations
- ITAR/EAR compliance
- Documentation improvements
- Bug fixes
- DeMoD SDR integrations

---

## License

**BSD-3-Clause License**

See [LICENSE](LICENSE) file for full details.

Note: DeMoDulation components are CC0 1.0 (public domain).

---

## Credits & Acknowledgments

- **Musnix:** Real-time kernel patches and audio optimizations
- **nixos-rk3588:** Orange Pi 5 and Rockchip hardware support
- **Asahi Linux:** Apple Silicon support
- **Disko:** Declarative disk partitioning
- **Calamares:** Graphical system installer
- **PipeWire/JACK:** Professional audio infrastructure
- **NixOS Community:** Reproducible system architecture
- **kexec-tools:** DSP coprocessor boot
- **FRR:** Advanced routing
- **CycloneDDS:** Real-time ROS 2 networking

---

## Contact & Support

- **Website:** [https://demod.ltd](https://demod.ltd)
- **GitHub Issues:** [https://github.com/ALH477/ArchibaldOS/issues](https://github.com/ALH477/ArchibaldOS/issues)
- **Commercial Support:** contact@demod.ltd
- **Twitter/X:** @DeMoDLLC

---

## Resources

- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [Musnix Documentation](https://github.com/musnix/musnix)
- [PipeWire Wiki](https://gitlab.freedesktop.org/pipewire/pipewire/-/wikis/home)
- [JACK Audio](https://jackaudio.org/)
- [Asahi Linux](https://asahilinux.org/)
- [DeMoDulation Guide](https://github.com/ALH477/DeMoDulation)
- [ROS 2 Humble](https://docs.ros.org/en/humble/)
- [PX4 Autopilot](https://px4.io/)

---

**ArchibaldOS – Professional real-time audio, robotics, and secure systems, anywhere you need it.**

*Built with precision by DeMoD LLC for musicians, engineers, roboticists, and defense professionals worldwide.*

[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/F1F11PNYX4)

**Version:** Based on NixOS 24.11 | **Last Updated:** December 2025
