# ArchibaldOS Community Edition

Real-time workstation for audio production and robotics + HydraMesh P2P networking.

**Flake URI:** `github:ALH477/ArchibaldOS`

## License

BSD-3-Clause. See [LICENSE](LICENSE).

## Quick Start

```bash
# Build Audio Workstation ISO (CachyOS RT BORE)
nix build github:ALH477/ArchibaldOS#iso

# Build Robotics Workstation ISO (CachyOS RT BORE)
nix build github:ALH477/ArchibaldOS#robotics-iso

# Build HydraMesh Networking ISO
nix build github:ALH477/ArchibaldOS#hydramesh-iso

# Fallback: musnix PREEMPT_RT kernel variants
nix build github:ALH477/ArchibaldOS#iso-musnix
nix build github:ALH477/ArchibaldOS#robotics-iso-musnix
```

## Kernel Options

| Kernel | Scheduler | Use Case |
|--------|-----------|----------|
| **CachyOS RT** (default) | BORE | Best latency + responsiveness |
| **musnix PREEMPT_RT** (fallback) | CFS | Mainline RT, max compatibility |

Both kernels use the same RT parameters:
- `threadirqs` - Threaded IRQ handlers
- `isolcpus=1-3` - Isolated CPU cores
- `nohz_full=1-3` - Full tickless
- `intel_idle.max_cstate=1` - Disable deep C-states

## Profiles

| Profile | ISO | Description |
|---------|-----|-------------|
| **Audio** | `iso` | RT audio production with DAWs, synths, DSP tools |
| **Robotics** | `robotics-iso` | RT control systems, simulation, hardware I/O |
| **HydraMesh** | `hydramesh-iso` | Headless P2P networking node |

## Audio Profile

- **Kernel**: CachyOS RT with BORE scheduler
- **Latency**: 32 samples @ 96kHz (~0.33ms)
- **DAWs**: Ardour, Audacity, Zrythm, Reaper
- **Synths**: Surge, Helm, Carla
- **DSP**: Csound, Faust, SuperCollider, Pure Data
- **Desktop**: Plasma 6 with Wayland

## Robotics Profile

Same RT kernel optimized for control systems:

- **Simulation**: Gazebo, Blender
- **CAD/EDA**: FreeCAD, OpenSCAD, KiCad
- **Development**: CMake, GCC, Clang, Python, VS Code
- **Hardware**: Arduino IDE, serial tools, CAN bus
- **Vision**: OpenCV
- **Control**: Octave, NumPy, SciPy, control library

### Hardware Support

Preconfigured udev rules for:
- Arduino (all variants)
- FTDI USB-serial
- STM32 (DFU mode)
- Teensy
- Generic USB serial

## HydraMesh P2P

Sub-10ms latency networking:

```nix
services.hydramesh = {
  enable = true;
  mode = "p2p";
  peers = [ "192.168.1.100:7777" ];
};
```

## Community vs Pro

| Feature | Community | Pro |
|---------|-----------|-----|
| CachyOS RT BORE kernel | ✅ | ✅ |
| musnix PREEMPT_RT fallback | ✅ | ✅ |
| Audio Profile | ✅ | ✅ |
| Robotics Profile | ✅ | ✅ |
| HydraMesh P2P | ✅ | ✅ |
| x86_64 Desktop ISOs | ✅ | ✅ |
| **ARM Support** | ❌ | ✅ Orange Pi 5, RPi |
| **Thunderbolt/USB4** | ❌ | ✅ 40Gbps |
| **Auto-updates** | ❌ | ✅ With rollback |
| **AppArmor + audit** | ❌ | ✅ |
| **DSP Coprocessor** | ❌ | ✅ |
| **Enterprise configs** | ❌ | ✅ |

Pro: https://github.com/ALH477/archibaldos-pro

## Development

```bash
# Audio dev shell
nix develop github:ALH477/ArchibaldOS

# Robotics dev shell
nix develop github:ALH477/ArchibaldOS#robotics
```

## Credits

- [CachyOS](https://cachyos.org) - BORE scheduler and optimized kernels
- [musnix](https://github.com/musnix/musnix) - Real-time audio NixOS module
- [chaotic-nyx](https://github.com/chaotic-cx/nyx) - CachyOS packages for NixOS
- [NixOS](https://nixos.org) - The reproducible Linux distribution

---

Copyright (c) 2025 DeMoD LLC. All rights reserved.
