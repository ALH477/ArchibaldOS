<!-- SPDX-License-Identifier: BSD-3-Clause -->
# Companion: a music computer on older hardware, commanded by Oligarchy

The `companion` profile (`modules/companion.nix`) turns a 4 GB, 2-core
machine into a headless audio device. JACK runs on its interface, and an
Oligarchy host drives it over SSH and the DSP control bridge, inside
WireGuard. The first target is an older 4 GB Microsoft Surface Pro.

| | companion | the desktop audio profile |
|---|---|---|
| desktop | none (console, `nmtui` for Wi-Fi) | Plasma 6 |
| audio | JACK only (`jack2-alsa`, as your user, under `rt-exec`) | PipeWire + JACK |
| memory | zram (zstd, 50%), swappiness 100, earlyoom that spares jackd, demod-rt and sshd | none |
| RT | `threadirqs preempt=full`; JACK floats across both cores | `isolcpus=1-3 nohz_full=1-3`, C-states capped |
| commanded by | Oligarchy's `dsp-ctl` and `oligarchy-companion` | — |

Why no `isolcpus`: on a 2-core machine, isolating a core that nothing is
pinned to only takes it away from everything. If you measure xruns and want
a dedicated core, set both `audio.cpu = 1` and `audio.isolate = true`. Then the
CPU is isolated and jackd is pinned to it (`rt-exec --cpu 1`); the module
refuses `isolate` without `cpu`.

## Installing on the Surface

1. **Find the model.** UEFI shows it (hold **Volume Up** and press **Power**),
   and so does the label under the kickstand. The 4 GB models are the Surface
   Pro 3, 4 and 5 (2017) with an i3, m3 or i5.
2. **Turn Secure Boot off** in that UEFI menu. The ISO is not signed for
   Microsoft's keys. (Oligarchy's own Secure Boot path is lanzaboote; the
   companion does not use it yet.)
3. **Boot the ISO from USB:** hold **Volume Down** and press **Power**. Use the
   audio ISO (`nix build .#iso`, Calamares) or the minimal one
   (`.#hydramesh-iso`, `archibaldos-install`). Keep a USB keyboard at hand in
   case the Type Cover does not respond in the live session `[UNTESTED]`.
4. **Pick "Companion (older hardware)"**, not "Companion for Microsoft
   Surface". The Surface variant runs the linux-surface kernel, which nothing
   caches, so a 4 GB tablet would spend hours compiling it. Oligarchy builds it
   later (below).
5. **Set a user password you can type.** Until the machine is enrolled, SSH
   takes that password, which is how Oligarchy gets in the first time.

After the first boot, connect to Wi-Fi with `nmtui` and check that JACK is
running on your interface:

```sh
aplay -l                         # USB interfaces are usually card 1
systemctl status jack2-alsa      # the journal's rt-exec line says what RT it got
```

If your interface is not `hw:0`, add to `/etc/nixos/hosts/installed/local.nix`:

```nix
{ archibald.companion.audio.device = "hw:1"; }
```

You can also do that later from Oligarchy, which is the better place once the
machine is enrolled.

## Enrolling it with Oligarchy

On the Oligarchy host, `custom.companions.enable = true` gives you the
WireGuard hub (`wg-companions`, `10.77.0.1/24`, UDP 51877) and the
`oligarchy-companion` CLI. Then:

```sh
oligarchy-companion enroll surface asher@192.168.1.42
```

This does the following, using the password once:

1. Copies the companion's `/etc/nixos` to Oligarchy. That copy is what later
   deploys build from.
2. Writes `hosts/installed/commander.nix` into both copies: the hub's address
   and WireGuard key, your SSH key, and the companion's tunnel address.
3. Has the companion rebuild itself once (`sudo` asks for the password). From
   then on it accepts SSH keys only, root can log in only with your key, and
   it dials the hub.
4. Prints the `custom.companions.members.surface` entry to add to your
   Oligarchy configuration, with the companion's WireGuard public key. Rebuild
   Oligarchy and the tunnel comes up.

After that, everything goes over the tunnel:

```sh
oligarchy-companion status surface   # dsp-ctl over SSH: JACK, ports, latency
oligarchy-companion deploy surface   # build on Oligarchy, switch the companion
dsp-ctl --transport tcp --host 10.77.0.2 ping   # the control bridge, if demod-rt runs there
```

## Moving to the Surface kernel

On Oligarchy, in the enrolled copy (`oligarchy-companion path surface`), set
`"profile": "companion-surface"` in `hosts/installed/install.json`, then run
`oligarchy-companion deploy surface`. Oligarchy compiles linux-surface, which
takes minutes on a Framework 16 and hours on the tablet, and copies it over.
The deploy also syncs the copy back to the companion's `/etc/nixos`, so the
two stay identical.

## Touchscreen and DSP host

Attach a touchscreen and DeMoD Mixer comes up fullscreen on it. Without one,
the companion stays headless. To work with the Oligarchy DSP VM instead of
only its own interface, set `archibald.companion.dsp.host`, and on a wired
box `netjack = true`. Both are covered in [form-factors.md](form-factors.md).

## What dsp-ctl may do there

dsp-ctl's SSH transport runs `sudo systemctl start|stop|restart` on
`archibaldos-dsp.service` (the DSP stack; it pulls JACK in) and on
`demod-rt.service`. sudo allows exactly those six commands without a password,
and nothing else. `checks.installed-contract` asserts that list. The control
bridge (TCP 7777) listens only on the tunnel interface and admits only the
commander's tunnel address.

## Not verified

- `[UNTESTED]` That Chaotic's binary cache holds the exact CachyOS kernel
  build this nixpkgs revision asks for. It could not be reached from where
  this was written. If an install starts compiling a kernel, stop it, install
  with the `plain` profile or on another machine, and let Oligarchy deploy.
- `[UNTESTED]` Real Surface hardware: Wi-Fi (Marvell `mwifiex` on the Pro 3
  and 4 is known to be flaky on mainline), the Type Cover, and thermals under
  a sustained JACK load.
- NetJack2 to the Oligarchy DSP VM is now a role
  (`archibald.companion.dsp`, [form-factors.md](form-factors.md)).
  `checks.netjack2` runs it between real JACK servers.
  - `[UNTESTED]` On a real network.
  - The Oligarchy DSP VM side exists (Oligarchy's `modules/dsp-guest.nix`
    and `vm-manager`, guest `10.78.0.2`); see
    [form-factors.md](form-factors.md).
  - Wired boxes only: over Wi-Fi it needs its own latency budget, measured
    first.
- `[OPEN]` The desktop profiles' `isolcpus=1-3` has the same problem the
  companion avoids: nothing pins JACK onto the isolated cores. That is a
  change to those profiles and has not been made here.
