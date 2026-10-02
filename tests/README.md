<!-- SPDX-License-Identifier: BSD-3-Clause -->
# tests/

Gates for the DSP guest, `rt-exec`, the robotics images and the installer.
`nix flake check` runs the seven `checks`; the boot proxy is a package because it boots VMs.

```sh
nix flake check                          # checks.{rt-exec,dsp-vm-contract,robotics-contract,installed-contract,installer-unit,netjack2,roles-contract}
nix build .#dsp-vm-boot-proxy            # needs the `kvm` system feature
bash tests/rt-exec-check.sh "$(nix build --print-out-paths .#rt-exec)/bin/rt-exec"
```

| file | gate |
|---|---|
| `rt-exec-check.sh`, `rt-exec.nix` | `checks.rt-exec` |
| `dsp-vm-contract.nix` | `checks.dsp-vm-contract` |
| `robotics-contract.nix` | `checks.robotics-contract` |
| `installed-contract.nix`, `fixtures/installed/` | `checks.installed-contract` |
| `installer-unit.nix`, `../installer/calamares/tests/` | `checks.installer-unit` |
| `netjack2/`, `roles.nix`, `../tools/jack-router/` | `checks.netjack2` |
| `roles-contract.nix`, `fixtures/installed/rack-efi/` | `checks.roles-contract` |
| `dsp-vm-boot-proxy.nix` | `packages.dsp-vm-boot-proxy` |

## Each gate fails on the tree before it

A gate that passes on the bug it was written for proves nothing, so each was
run against, or evaluated over, the previous tree:

- **`rt-exec-check.sh`** against the previous `rt-exec` (through a shim that
  drops the new `--`/`--cpu`/`--prio` flags it does not know): fails its first
  check, `THP_enabled is '1' in the target`. The new binary passes 7/7 as root
  (the `SCHED_FIFO` branch) and 7/7 as `nobody` with no capabilities (the
  shortfall-reported branch, which is also what the sandbox runs).
- **`dsp-vm-contract`**: its predicates evaluated over the previous tree give
  `jack2-alsa` broken deps `["pipewire.service"]`, `demod-rt` broken deps
  `["jack2-netjack.service"]`, bridge user `root`, no `range=`, firewall off,
  ssh password auth on, no `efiSupport`, no `/boot`, NNP off. Writing it this
  way found one vacuous predicate: "runs under rt-exec" first matched the old
  `…/rt-exec-wrapper/bin/rt-exec-wrapper` path as a substring. It now matches
  the `rt-exec` derivation's store path and is false on the old tree.
- **`robotics-contract`**: the previous `flake.nix` carried `MODE="0666"` in
  both robotics images, and the rules did not depend on
  `profiles.robotics.hardware.arduino` — by inspection of the old source, not
  by running the contract against it.
- **`installed-contract`, `installer-unit`**: there was no installer before
  them, so "fails on the old tree" is trivially true. Instead they were run
  against a copy of this tree with five mutations, each aimed at one
  guarantee:
  - sudo gets a bare `systemctl` rule;
  - the bridge port is opened on every interface;
  - the console keymap is no longer derived from xkb;
  - JACK is pinned to CPU 0;
  - the job installs `#nixos` instead of `#installed`.

  Exactly the targeted checks failed: four contract checks (16/20 passed),
  plus `test_profile_install_copies_the_flake_and_installs_it`. Nothing else
  changed state. `installer-unit` also builds a doctored upstream on every
  run and requires the drift guard to fail it.
- **`netjack2`**: before the DSP host's router starts, a box's tone comes back
  at 0.0000; with it, 0.4997, and 0.4998 for a box that joined later. Those
  numbers come from the modules' own commands. The NetJack2 units it replaced
  (`jack_netsource` on both sides) could not have formed a link at all.
- **`roles-contract`**: a copy with three mutations fails exactly the three
  targeted checks (12/15):
  - the kiosk wanted by `graphical.target`;
  - the DSP host not routed into the tunnel;
  - the kiosk on the simulator even on a rack unit.
- **`dsp-vm-boot-proxy`**: not run against the old layout. A BIOS-only image
  under OVMF is the PXE loop Oligarchy already recorded.

## What is not covered

The RT kernel image is not booted (the proxy uses a stock kernel for the
same layout), and nothing here starts JACK or the engine on a guest. See
`docs/security.md`, "Open".
