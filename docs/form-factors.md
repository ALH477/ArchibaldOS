<!-- SPDX-License-Identifier: BSD-3-Clause -->
# Form factors: laptop, embedded, rack

One role, three kinds of machine. A **companion** is an ArchibaldOS machine
with JACK on its audio interface, commanded by an Oligarchy host
(`docs/companion.md`). Whatever the box, two things decide what it does:

- **A touchscreen attached → the DeMoD Mixer kiosk.** No touchscreen → headless.
- **Where the DSP runs:** on the box itself (a rack unit), or on the Oligarchy
  DSP VM, with the box's audio streamed there and back over NetJack2.

| machine | image | engine | panel |
|---|---|---|---|
| laptop / tablet (x86) | installer, profile `companion` (Surface: `companion-surface`) | DSP VM, or none | its touchscreen, if any |
| rack PC, mini-PC, mixer-like box (x86) | installer, profile `rack` | **on the box** | a USB/HDMI touch panel, if attached |
| Raspberry Pi 4 / 5 | `nix build .#packages.aarch64-linux.companion-pi{4,5}-sdimage` | DSP VM | the official 7" panel or any USB touch |
| StarFive JH7110 (VisionFive 2, Framework 13 RV) | `nix build .#packages.riscv64-linux.companion-riscv-sdimage` | DSP VM | a USB touch panel (software rendering) |

## The kiosk (`modules/kiosk.nix`)

cage, a single-app Wayland compositor, runs DeMoD Mixer fullscreen with no
cursor (`DEMOD_KIOSK=1`) on tty1, as the audio user. **Nothing starts it at
boot.** A udev rule starts it when an input device with
`ID_INPUT_TOUCHSCREEN` appears, at boot (coldplug) or when a panel is plugged
in later. A box with no panel spends nothing on graphics.

The mixer drives:
- the box's own engine on a `rack` unit (`local`);
- the DSP VM's engine through its `demod-remote-bridge`, over DCF inside
  WireGuard, when `archibald.companion.dsp.host` is set (`remote:HOST`);
- otherwise nothing: it shows **SIMULATOR** and keeps retrying.

What it shows and how it is used is in DeMoD's `apps/mixer/README.md`.

Another app instead of the mixer: `archibald.kiosk.program`. TERMINUS is
PolyForm Shield (DeMoD `LICENSING.md`), so it is not the default in a
BSD-licensed image; choose it per device.

## The link to the DSP VM

```nix
# hosts/installed/local.nix on a companion
{
  archibald.companion.dsp = {
    host = "10.78.0.2";   # the DSP VM, as Oligarchy routes it
    netjack = true;       # wired boxes only
  };
}
```

- **Control.** The DSP VM's address is added to the WireGuard peer's allowed
  IPs, so it is reachable through the commander's tunnel and nothing else is
  added. The kiosk drives its engine. Oligarchy keeps commanding the box
  itself (`dsp-ctl`, `oligarchy-companion`).
- **Audio** (`netjack = true`, `modules/netjack.nix`). The box's JACK loads
  jack2's `netadapter` and joins the VM's `netmanager` by address (unicast:
  multicast does not cross a routed tunnel). On the VM the box appears as a
  client named after its host. jack-router (`tools/jack-router`, rules in
  `archibald.jack.routes`) wires every box's channels 1-2 into `demod-rt` and
  its output back, including boxes that join later. On the box it wires the
  interface's inputs to the adapter and the return to the interface's outputs.
  The adapter resamples between the two clocks, so many boxes can join one VM.
  Wired only: over Wi-Fi, the stream's jitter becomes xruns.

## The engine on a rack unit (`modules/demod-engine.nix`)

The orchestrator runs with demod-rt as its child, the way DeMoD runs it. The
older `services.demod-rt` started demod-rt alone, which cannot work: it needs
the orchestrator's shared memory. That unit now warns when enabled.
jack-router wires the interface through demod-rt.
`archibald.engine.remote.enable` adds `demod-remote-bridge`, so a DeMoD app
elsewhere can drive the engine; bind it to the tunnel address.

## SD images

The card is the system; there is no installer. The companion user is
`archibald` with the password `archibald`. It is published, and expired at
first boot: the first login, on the console or over SSH, must change it.
Then enrol the board from Oligarchy (`oligarchy-companion enroll`), which
ends password logins.

Pi kernels are nixos-hardware's board kernels, not CachyOS (x86 only). The
JH7110 keeps the RT kernel of `archibaldOS-riscv` and uses networkd: on
riscv64, NetworkManager pulls in GHC.

## Gates

| gate | what it does |
|---|---|
| `checks.netjack2` | Runs three real JACK servers in the sandbox, using the modules' **own** commands. A tone sent into a box comes back through the DSP host's `demod-rt` position: peak 0.4997 for a box that joined before the router, 0.4998 for one that joined after. It reads 0.0000 before the router exists, so the router is what makes the path. |
| `checks.roles-contract` | 15 evaluation checks over the companion variants, `rack`, Pi 4/5 and RISC-V: the kiosk is dormant until touch, the engine choice, the tunnel routes, the NetJack2 adapter and its firewall, the rack engine, board kernels, password expiry. A mutated copy fails exactly the three targeted checks. |
| `checks.dsp-vm-contract` | Now asserts that the DSP VM image runs the NetJack2 manager and that no unit runs `jack_netsource`. |

## Not verified

- `[UNTESTED]` A real touchscreen starting cage; the mixer on a real panel
  (rotation and calibration are not handled).
- `[UNTESTED]` The Pi and RISC-V images building and booting. They evaluate;
  this environment has no aarch64 or riscv64 builder.
- `[UNTESTED]` NetJack2 over a real network and a real interface, and its
  latency. The gate measures peaks on loopback with dummy drivers.
- `[UNTESTED]` The engine (orchestrator + demod-rt) starting on a box. The
  unit is evaluated, not run.
- `[OPEN]` Two faders at once on the panel. SDL's touch-to-mouse emulation
  follows the first finger.
- The Oligarchy side exists: its DSP guest imports this flake's `netjack`,
  `demod-engine` and `dsp-control-bridge` modules (`nixosModules`), sits at
  `10.78.0.2` on a routed tap, and Oligarchy forwards `wg-companions` to it
  (UDP and ICMP only). Oligarchy's `.#dsp-netjack-tests` runs a box's
  NetJack2 commands from these modules against the guest's in its build
  sandbox; `.#dsp-route-contract` checks the routing. See Oligarchy's
  `vm-manager/docs/dsp-vm.md`.
  - `[UNTESTED]` The guest booting under KVM, and the path over a real
    tunnel (WireGuard's MTU is 1420; NetJack2 sends 1500-byte packets by
    default).
