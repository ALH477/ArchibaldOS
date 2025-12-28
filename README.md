# ArchibaldOS Community Edition

Real-time workstation for audio production and robotics + HydraMesh P2P networking.

**Flake URI:** `github:ALH477/ArchibaldOS`

## License

BSD-3-Clause. See [LICENSE](LICENSE).

## Quick Start

```bash
# Build Audio Workstation ISO
nix build github:ALH477/ArchibaldOS#iso

# Build Robotics Workstation ISO
nix build github:ALH477/ArchibaldOS#robotics-iso

# Build HydraMesh Networking ISO
nix build github:ALH477/ArchibaldOS#hydramesh-iso
```

## Profiles

| Profile | ISO | Description |
|---------|-----|-------------|
| **Audio** | `iso` | RT audio production with DAWs, synths, DSP tools |
| **Robotics** | `robotics-iso` | RT control systems, ROS, simulation, hardware I/O |
| **HydraMesh** | `hydramesh-iso` | Headless P2P networking node |

All profiles use the same **musnix RT kernel** for deterministic timing.

## Audio Profile

- **RT Kernel**: musnix PREEMPT_RT
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

### Groups

Robotics user automatically added to:
- `dialout` - Serial ports
- `plugdev` - USB devices  
- `gpio`, `i2c`, `spi` - Hardware buses
- `input` - Input devices

## HydraMesh P2P

Sub-10ms latency networking:

```nix
services.hydramesh = {
  enable = true;
  mode = "p2p";
  peers = [ "192.168.1.100:7777" ];
};
```

### CLI Tools
- `hydramesh-status` - Container status
- `hydramesh-logs` - Follow logs
- `hydramesh-pull` - Update image

## Community vs Pro

| Feature | Community | Pro |
|---------|-----------|-----|
| RT Kernel (musnix) | ✅ | ✅ |
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

- [musnix](https://github.com/musnix/musnix) - Real-time audio NixOS module
- [NixOS](https://nixos.org) - The reproducible Linux distribution

---

Copyright (c) 2025 DeMoD LLC. All rights reserved.
