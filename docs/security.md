<!-- SPDX-License-Identifier: BSD-3-Clause -->
# Security posture

ArchibaldOS ships three kinds of image: live ISOs (audio, robotics, HydraMesh),
a RISC-V SD image, and the DSP coprocessor guest that Oligarchy's
`vm-manager` boots. This page states what each exposes, what changed to bring
them in line with the rules Oligarchy, Punctim and Exsecutor already work to,
and what is still open. Claims carry their evidence; anything not measured is
marked `[UNTESTED]` or `[OPEN]`.

The rules being applied, and where they come from:

- **Plaintext by design means the network is the security model.** DCF carries
  no encryption (EAR/ITAR). Punctim's `Documentation/DCF_SECURITY_EXPOSURE.md`
  and Oligarchy's `demod-talk` module both refuse a wildcard bind for it.
- **Who may talk to a control surface is decided in code**, not left to a
  firewall: Oligarchy's P2P substituter restricts its peers to private ranges
  in the daemon itself.
- **Authority is declared, not ambient** — Exsecutor's thesis. For a NixOS
  unit that means its user, capabilities, address families and syscalls are
  written down, and nothing runs as root by default.
- **A gate must exercise what the subsystem does, not the scaffolding around
  it** (Oligarchy's `CLAUDE.md`), and a check that inspects nothing is a FAIL.

## DSP coprocessor guest (`dsp-vm`, `dsp-vm-qcow2`)

| | before | now |
|---|---|---|
| control bridge (`socat`, TCP 7777 → engine control socket) | root, every address, any peer | user `dsp`, no capabilities, syscall + address-family allowlist; `socat range=` refuses any peer outside `archibald.dsp.control.allowFrom` (default `10.0.2.2/32`, QEMU user-net's host), repeated by `IPAddressAllow` |
| firewall | off (its port lists were dead config) | on: NETJACK 4713, the control port, ssh |
| ssh | passwords accepted; `dsp` is in wheel with the published password `dsp` | keys only, no root login |
| `jack2-alsa` | `Requires=pipewire.service`, which NixOS **masks** here (PipeWire is not system-wide) | no PipeWire dependency |
| `demod-rt` (when enabled) | `Requires=jack2-netjack.service`, which nothing defines; `NoNewPrivileges=false`; "rt-exec" that was a script running the engine directly; `dsp` added to wheel "for socket creation" | requires `jack2-alsa`; NNP on (its capabilities are ambient); really runs under `rt-exec`; no wheel |
| disk image | BIOS-only GRUB, no ESP | hybrid GPT: GRUB on `/dev/vda` and as the removable EFI loader, ESP at `/boot` |

Why the control bridge matters: the engine's control protocol has `load_fx`
and `synth.load`, which make it `dlopen` a path. Whoever reaches that port can
choose what the real-time engine loads.

Why the dependency chain matters: statically, `jack2-alsa` could not start,
so neither could `jack2-netjack-master` (which requires it) nor `demod-rt`.
That is read from the evaluated configuration (`checks.dsp-vm-contract`), not
observed on a booted guest — see the open items.

Why the image layout matters: Oligarchy's `vm-manager` `dsp-vm` module boots
with OVMF by default, and OVMF given an image with no EFI system partition
falls through to PXE. Oligarchy hit exactly that with the guest this image
replaced (its `dsp-vm-qcow` output: "`qcow-efi`, NOT `qcow`").

To reach the guest over ssh, add a key in your own module:

```nix
users.users.dsp.openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAA… you@host" ];
```

Over a tap or bridge instead of QEMU user networking, set the host's address:

```nix
archibald.dsp.control.allowFrom = "192.168.122.1/32";
```

## `rt-exec`

The wrapper JACK and `demod-rt` start under. Measured on the previous
version, by running it and reading the target's `/proc/self/status`:

- **THP was never disabled.** `prctl` was called without `<sys/prctl.h>`, so
  `PR_SET_THP_DISABLE` was undefined and the `#ifdef` compiled the step out,
  with no warning. Target: `THP_enabled: 1`.
- **`mlockall` locked the wrapper, not the target.** Memory locks and
  `MCL_FUTURE` do not survive `execve(2)`. Target: `VmLck: 0 kB`.
- **`RLIMIT_NICE` was set to 1**, which caps nice at 19 — the opposite of the
  "allow -20" its comment promised — and under systemd's `LimitNICE=-20` it
  succeeded, because lowering a limit needs no privilege.
- **Every failure was silent**, including all of the above.

Now it establishes only what survives `exec` (raised limits, `SCHED_FIFO`,
affinity, THP off, read back), prints one summary line naming every shortfall,
and `--strict` refuses to start the target on any. The target locks its own
memory (jackd under `-R`, demod-rt by design); the raised `RLIMIT_MEMLOCK` is
what lets it.

## HydraMesh (`services.hydramesh`)

- Docker-published ports **bypass `networking.firewall`** (Docker DNATs them in
  `PREROUTING`; they never reach the `INPUT` chain the NixOS firewall filters).
  So the published address is the access control. New: `bindAddress` (mesh UDP;
  default `0.0.0.0` for compatibility, **warned**) and `grpcBindAddress`
  (control API; default `127.0.0.1` — it used to be every interface).
- `image` is a mutable tag (`alh477/hydramesh:latest`). An unpinned reference
  is warned about; pin it as `name@sha256:<digest>`. No digest is pinned here
  because none has been verified for this repository.

The HydraMesh ISO therefore builds with two warnings. That is accurate: it
publishes the plaintext mesh on every interface and runs whatever `:latest` is.

## Robotics images

The udev rules paired `GROUP="dialout"` with `MODE="0666"`, which made the
group decorative: any local account, service users included, could write to
an attached motor controller or reflash a board. They are `0660` now, and they
moved from two copies in `flake.nix` into `profiles.robotics.hardware.arduino`
— an option that existed and did nothing, as did `hardware.canbus`. Both now
decide what they say.

## RISC-V image

`kernel.randomize_va_space = 0` (ASLR off) was set with no stated reason. ASLR
changes where mappings land, not how long the RT path takes. It is back at the
kernel default.

## Gates

| gate | what it does | cost |
|---|---|---|
| `checks.rt-exec` | runs `rt-exec` in the sandbox; reads THP, affinity and limits back from the exec'd process; unprivileged branch requires the `SCHED_FIFO` shortfall to be reported and `--strict` to refuse | seconds |
| `checks.dsp-vm-contract` | 14 assertions over the evaluated guest: every `Requires=` is defined and enabled, the bridge's user/caps/`range=`, firewall, sshd, UEFI loader | eval only |
| `checks.robotics-contract` | both robotics images: no `0666`, rules present, and the two options really remove what they say | eval only |
| `packages.dsp-vm-boot-proxy` | builds an image with `modules/dsp-vm-image.nix` and boots it under SeaBIOS and OVMF; the guest must report the firmware it came up under from userspace | minutes with KVM; long under TCG |

`nix flake check` runs the first three. `tests/README.md` records how each
was shown to fail on the tree before this change.

## Open

- `[UNTESTED]` The RT DSP image itself has not been booted under OVMF. The
  boot proxy uses the same layout module with a stock kernel.
- `[UNTESTED]` The DSP chain (`jack2-alsa` → `jack2-netjack-master` →
  `demod-rt`) has not been started on a booted guest since the dependency fix.
- `[OPEN]` PipeWire still runs in the guest's user session (autologin on the
  serial console) and can hold `hw:0` before `jackd` does. Oligarchy's own
  guest drops PipeWire for JACK alone; whether to follow is a latency decision.
- `[OPEN]` The engine's `load_fx` / `synth.load` accept any existing path. The
  bridge now limits *who* can ask; nothing limits *what* is loaded. That is a
  change in DeMoD's orchestrator (an allowlisted root, or store paths only).
- `[OPEN]` `permittedInsecurePackages = [ "qtwebengine-5.15.19" ]` in the
  desktop ISOs. Oligarchy keeps that list empty.
- `[OPEN]` Live ISOs autologin a wheel user with a published password and put
  it in `docker` (root-equivalent). Normal for live media; worth stating.
- `[OPEN]` Exsecutor's kernel roadmap (Oligarchy `docs/exsecutor-kernel-roadmap.md`,
  phase K0) lists "ArchibaldOS migrated to mainline `PREEMPT_RT`". The DSP guest
  (musnix) and the RISC-V image already are; the x86 ISOs run CachyOS BORE by
  choice. The same phase's sealed closure — EROFS + dm-verity, as Oligarchy's
  captive-portal guest already builds — fits the DSP guest well and is not done.
