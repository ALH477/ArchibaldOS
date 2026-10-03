# ArchibaldOS Community Edition (v.1.2-Omega-Alpha-Chad-Syndrome-Maestro)

Old codebase is in a zip file in the release section. I updated the repo to be easier to maintain and learn.

Real-time workstation for audio production and robotics + HydraMesh P2P networking.

What sets it apart? **Bit-for-bit reproducibility** across deployments, ensuring that your setup on a studio workstation matches exactly on a drone brain or secure edge router—eliminating the *"it works on my machine"* syndrome that plagues traditional OSes. Trust us, once you experience deployment consistency at this level, you'll wonder how you ever managed without it.

ArchibaldOS forms the foundational operating system layer for the **DeMoD platform**, a cohesive ecosystem for real-time digital signal processing (DSP) and demodulation. As detailed in the open-source guide at [https://github.com/ALH477/DeMoDulation](https://github.com/ALH477/DeMoDulation)—a public blueprint released by DeMoD LLC on **November 20, 2025**—ArchibaldOS powers **DIY DSP devices** built from e-waste, Framework 13 mainboards, or Raspberry Pi 5. This integration enables **sub-0.8ms round-trip latency at 24-bit/192kHz**, transforming low-cost hardware into professional-grade audio and **software-defined radio (SDR)** rigs. The DeMoDulation repository provides Nix flake profiles that explicitly support ArchibaldOS as a **native or virtualized build option**, ensuring seamless scalability from embedded prototypes to production deployments.

## Who Is ArchibaldOS For?

ArchibaldOS is a **specialized, expert-oriented operating system** designed for users who need **deterministic performance, reproducibility, and deep system control**. It is not intended to be a general-purpose or beginner-friendly Linux distribution.

### **This is for you if you are:**

#### **Professional Audio Engineers & DSP Developers**

* Working with **real-time audio**, live performance rigs, or studio setups
* Comfortable tuning **buffer sizes, IRQ priorities, and CPU governors**
* Requiring **sub-5ms round-trip latency** with measurable, reproducible results
* Building **DSP chains, neural amp models, or SDR-based audio systems**

#### 🤖 **Robotics & Autonomous Systems Engineers**

* Developing with **ROS 2, PX4, LIDAR, and real-time sensor fusion**
* Targeting **ARM SBCs** (Raspberry Pi, Orange Pi, RK3588, etc.)
* Needing **deterministic scheduling** for control loops and autonomy stacks
* Integrating **RF/SDR, audio, and robotics** into a unified real-time system

#### **AI & Systems Engineers (On-Prem / Edge)**

* Running **local LLMs and agentic workflows** without cloud dependency
* Managing **GPU acceleration, memory constraints, and inference pipelines**
* Integrating **voice, audio, and real-time I/O** with AI agents
* Valuing **reproducible, declarative infrastructure** over convenience

#### **Advanced Linux / NixOS Users**

* Already familiar with **NixOS or declarative system management**
* Comfortable editing `flake.nix` and rebuilding systems
* Wanting **bit-for-bit reproducibility** across machines and deployments
* Building custom systems rather than installing off-the-shelf distros

#### **Embedded, Edge, and Defense-Oriented Developers**

* Building **secure, minimal, ITAR/EAR-safe** systems
* Deploying on **e-waste, SBCs, or custom hardware**
* Needing **auditable configurations and deterministic behavior**
* Prioritizing **reliability over UX polish**

---

### **This is probably *not* for you if you are:**

* New to Linux or uncomfortable using the terminal
* Looking for a plug-and-play audio workstation
* Expecting GUI tools for all configuration tasks
* Unwilling to read documentation or debug low-level issues
* Seeking a “daily driver” desktop OS with minimal maintenance
* Unfamiliar with concepts like **xruns, PREEMPT_RT, or JACK/PipeWire tuning**

---

### **Design Philosophy**

ArchibaldOS follows a **“minimal oligarchy” philosophy**:

> Only components that directly contribute to performance, determinism, or reproducibility are included.

Convenience, abstraction, and mass-market usability are **intentionally deprioritized** in favor of:

* Measurable real-time performance
* Declarative, auditable configuration
* Cross-architecture reproducibility
* System-level transparency

If you want an OS that **gets out of your way** and lets you build **serious real-time systems**, ArchibaldOS is for you.

---


**Flake URI:** `github:ALH477/ArchibaldOS`

## License

BSD-3-Clause, copyright DeMoD LLC. See [LICENSE](LICENSE). Per-file
licensing, including third-party portions, is in [REUSE.toml](REUSE.toml) and
[LICENSES/](LICENSES/), and CI checks it with `reuse lint`.

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

# The installer, without Calamares (also on every ISO)
nix run github:ALH477/ArchibaldOS#archibaldos-install -- --help

# RISC-V SD image for StarFive JH7110 boards (VisionFive 2 / Framework 13 RV)
# Build natively on the board (preferred):
nix build github:ALH477/ArchibaldOS#packages.riscv64-linux.archibaldOS-riscv-sdimage
# ...or cross-build from x86_64 under binfmt qemu-user (slow) — see docs/riscv.md
```

## RISC-V (StarFive JH7110)

A headless RT-audio SD image for the **VisionFive 2** and **DeepComputing
Framework 13 RISC-V** mainboard (StarFive JH7110). Mainline Linux 6.12 with
native `PREEMPT_RT` (CachyOS RT doesn't build for riscv64). Build it on the
board or cross-build from x86_64 — full guide in **[docs/riscv.md](docs/riscv.md)**.

## Kernel Options

| Kernel | Scheduler | Use Case |
|--------|-----------|----------|
| **CachyOS RT** (default) | BORE | Best latency + responsiveness |
| **musnix PREEMPT_RT** (fallback) | CFS | Mainline RT, max compatibility |

The desktop profiles use the same RT parameters with either kernel:
- `threadirqs` - Threaded IRQ handlers
- `isolcpus=1-3` - Isolated CPU cores
- `nohz_full=1-3` - Full tickless
- `intel_idle.max_cstate=1` - Disable deep C-states

The companion profile does not. It uses `threadirqs preempt=full`, with no
isolated cores and no C-state cap, because both cost more than they buy on a
2-core, passively cooled machine (see [docs/companion.md](docs/companion.md)).

## Profiles

| Profile | ISO | Description |
|---------|-----|-------------|
| **Audio** | `iso` | RT audio production with DAWs, synths, DSP tools |
| **Robotics** | `robotics-iso` | RT control systems, simulation, hardware I/O |
| **HydraMesh** | `hydramesh-iso` | Headless P2P networking node |
| **Companion** | installed from any ISO | Headless music computer for older 4 GB hardware, commanded by Oligarchy |
| **Companion (Surface)** | installed from any ISO | The companion on the linux-surface kernel |
| **Rack unit / mixer** | installed from any ISO | The companion with the DeMoD engine on board |
| **Companion SD images** | `companion-pi4`, `companion-pi5`, `companion-riscv` | Raspberry Pi 4/5 and StarFive JH7110 as companions |

## Installing

The ISOs install ArchibaldOS itself. Earlier ISOs used the stock NixOS
Calamares step, which wrote a generic `configuration.nix`, so what landed on
the disk was plain NixOS. The installer now offers a profile page (every
profile above, plus plain NixOS), copies this flake to `/etc/nixos` on the
target, and installs `/etc/nixos#installed`. On the installed machine:

```bash
sudo nixos-rebuild switch --flake /etc/nixos#installed
```

Your own settings go in `/etc/nixos/hosts/installed/local.nix`. Details,
including the CLI installer for the minimal ISO, are in
[docs/installer.md](docs/installer.md).

## Companion

A headless music computer for older hardware (4 GB, 2 cores; first target an
older Surface Pro), with JACK, zram and earlyoom and no desktop. An Oligarchy
host commands it over WireGuard: `dsp-ctl` drives JACK and the DSP stack, and
`oligarchy-companion deploy` builds on Oligarchy and switches the companion.
Install steps, enrolment and what is still untested are in
[docs/companion.md](docs/companion.md).

## Laptops, embedded boards, rack units

The same role runs on an x86 laptop, a Raspberry Pi 4/5, a JH7110 board or a
rack PC. With a touchscreen attached, the front panel is **DeMoD Mixer** in
kiosk mode; with none, the box is headless. Its DSP runs on the box (a rack
unit) or on the Oligarchy DSP VM. Audio reaches the VM over NetJack2 inside
WireGuard (wired boxes), and the mixer drives the VM's engine over DCF.
[docs/form-factors.md](docs/form-factors.md) has the matrix, the link, and
what is measured.

## Audio Profile

- **Kernel**: CachyOS RT with BORE scheduler
- **Latency**: 32 samples @ 96kHz (~0.33ms)
- **DAWs**: Ardour, Audacity, Zrythm (REAPER is unfree and not
  redistributable, so no image ships it; add it on your own machine, see
  [docs/installer.md](docs/installer.md))
- **Synths**: Surge, Helm, Carla
- **DSP**: Csound, Faust, SuperCollider, Pure Data
- **Desktop**: Plasma 6 with Wayland

## Robotics Profile

Same RT kernel optimized for control systems:

- **Simulation**: Gazebo, Blender
- **CAD/EDA**: FreeCAD, OpenSCAD, KiCad
- **Development**: CMake, GCC, Clang, Python (VS Code is unfree and not
  redistributable, so no image ships it; add it on your own machine)
- **Hardware**: Arduino IDE, serial tools, CAN bus
- **Vision**: OpenCV
- **Control**: Octave, NumPy, SciPy, control library

### Hardware Support

Preconfigured udev rules (`profiles.robotics.hardware.arduino`, granted to the
`dialout` / `plugdev` groups at mode 0660 — add users to those groups) for:
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
  peers = [ "10.100.0.2:7777" ];
  bindAddress = "10.100.0.5";   # your WireGuard address: the DCF wire is plaintext
};
```

Docker-published ports bypass the NixOS firewall, so `bindAddress` is the
access control; the gRPC API is published on loopback only
(`grpcBindAddress`). Leaving `bindAddress` at `0.0.0.0`, or `image` unpinned by
digest, builds with a warning. See [docs/security.md](docs/security.md).

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

## Security

[docs/security.md](docs/security.md) states what each image exposes, what was
fixed, and what is still open. Changes you will notice:

- **DSP guest:** ssh takes keys only (add one with
  `users.users.dsp.openssh.authorizedKeys.keys`); the control bridge runs as
  `dsp`, not root, and accepts only `archibald.dsp.control.allowFrom` (default
  QEMU user-net's host, `10.0.2.2/32`); the firewall is on; the image boots
  under UEFI (OVMF) as well as BIOS.
- **HydraMesh:** gRPC on loopback by default; `bindAddress` for the mesh port.
- **Robotics:** board access is `0660` to `dialout`/`plugdev`, not `0666`.

- **Companion:** password SSH only until Oligarchy enrols it, then keys only;
  sudo is limited to the six `systemctl` commands `dsp-ctl` sends; the control
  bridge listens on the WireGuard interface for the commander's address only.

`nix flake check` runs the gates (`checks.rt-exec`, `checks.dsp-vm-contract`,
`checks.robotics-contract`, `checks.installed-contract`,
`checks.installer-unit`, `checks.netjack2`, `checks.roles-contract`); `nix build .#dsp-vm-boot-proxy` boots the DSP
image layout under SeaBIOS and OVMF. See [tests/README.md](tests/README.md).
`.github/workflows/check.yml` runs `nix flake check` on every pull request and
every push to `main`; the KVM boot proxy stays on demand.

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
