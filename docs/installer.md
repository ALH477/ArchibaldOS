<!-- SPDX-License-Identifier: BSD-3-Clause -->
# The installer

## What was wrong

The graphical ISOs are built on nixpkgs' Calamares installer. Its install
step (the `nixos` job from `calamares-nixos-extensions`) writes a generic
`/etc/nixos/configuration.nix`, with Plasma or GNOME as picked on its desktop
page, and runs `nixos-install`. So when you booted ArchibaldOS and installed,
you got **plain NixOS**: no CachyOS kernel, no RT tuning, no audio stack, no
ArchibaldOS modules. The minimal HydraMesh ISO had no installer at all.

## What the ISO installs now

The installer puts on the disk the same flake the ISO was built from, at the
same revision, building the profile you pick:

1. **Profile page.** This replaces upstream's desktop and unfree pages. It
   offers each `profiles.<id>` in `flake.nix` (`installer/profiles.nix`) and
   **Plain NixOS**. Each description says what it costs, for example whether
   the kernel compiles from source.
2. **Install step.** This is `installer/calamares/distroinstall/main.py`, and
   it replaces upstream's `nixos` job in the exec sequence:
   - It copies the flake source to `/mnt/etc/nixos`. Any `/etc/nixos` that was
     already on the target is moved aside to `/etc/nixos.before-ArchibaldOS`,
     not overwritten.
   - It writes `hosts/installed/hardware-configuration.nix` (the hardware scan)
     and `hosts/installed/install.json` (your answers).
   - It runs `nixos-install --flake /mnt/etc/nixos#installed`.
3. **Plain NixOS** hands the whole step to upstream's unmodified `nixos` job,
   which is imported, not copied. Picking it installs exactly what the stock
   NixOS installer would.

`install.json` is data, not Nix. `installer/installed.nix` maps it to options:
host name, time zone, locale, keyboard, the user and its groups, autologin,
the boot loader, and LUKS. `flake.nix`'s `mkInstalled` then adds the profile's
modules. Because the mapping is in Nix, `checks.installed-contract` can test it
against fixtures.

## On the installed machine

```sh
sudo nixos-rebuild switch --flake /etc/nixos#installed
```

| file under `/etc/nixos/hosts/installed/` | written by | edit it? |
|---|---|---|
| `install.json` | the installer | rarely: it is your install-time answers |
| `hardware-configuration.nix` | the installer's scan | when the disks change |
| `commander.nix` | Oligarchy's `oligarchy-companion enroll` (companions only) | no |
| `local.nix` | nothing | yes: your own settings, imported when it exists |

`/etc/nixos` is a plain copy, not a git checkout. To follow ArchibaldOS
upstream, either run `nix flake update` there, or replace everything except
`hosts/installed/` with a newer tree. If you want history, `git init` it,
and `git add hosts/installed`: a git flake sees only tracked files, and
without them `#installed` disappears.

## Without Calamares: `archibaldos-install`

Every ISO ships `archibaldos-install`. It is the only installer on the
minimal (HydraMesh) ISO, and it is also useful for scripted installs. It runs
the same job module with your answers taken from flags, so it cannot drift
from the graphical installer:

```sh
# partition, format and mount the target under /mnt first, as in the NixOS manual
sudo archibaldos-install --profile companion --user asher --hostname surface \
    --timezone Europe/Berlin --locale en_US.UTF-8 --keyboard us
archibaldos-install --profile companion --user asher --dry-run   # prints install.json, touches nothing
```

It refuses to run if `--root` is not a mount point. On a BIOS machine it
needs `--boot-device`. After installing, it asks for the user's password
inside the new system. It does not set up encrypted swap or GRUB with an
encrypted `/boot`; use the graphical installer for those. A LUKS root under
UEFI works, because the hardware scan records it.

## Upstream drift

`installer/calamares/extensions.nix` builds a replacement
`calamares-nixos-extensions`. It copies upstream, adds the job and the profile
page, and regenerates `settings.conf`. Before regenerating, it checks that
upstream's module instances and page sequence match one of the two shapes
it knows. Both ship as version 0.3.23: nixos-25.11's, and this tree's
nixos-unstable one, which adds a progress weight for the `nixos` job. If
nixpkgs changes either, the ISO build fails with
`calamares-nixos-extensions changed its ...` instead of shipping an installer
that skips a page. `checks.installer-unit` proves that this guard fires.

The job borrows `NixProgress` and `fix_btrfs_subvolumes` from upstream only
where they exist; the 25.11 job has neither. These files are shared with
Oligarchy, which runs 25.11 and vendors them byte-identically (Oligarchy
`installer/README.md`). A change here goes there too.

## Gates

| gate | what it does |
|---|---|
| `checks.installer-unit` | Runs the job's 9 unit tests against the real upstream `main.py`. These cover EFI, BIOS with LUKS and a keyfile, the copy, the scan, the `nixos-install` arguments, a pre-existing `/etc/nixos`, plain delegation, an unknown profile refused before the disk is touched, and a failed install reported. It also checks the generated `settings.conf`, that a doctored upstream sequence fails the build, and the CLI's `--dry-run`. |
| `checks.installed-contract` | Evaluates four installed fixtures (companion on UEFI, enrolled companion, companion-surface, audio on BIOS with LUKS) plus one with a future schema: 20 assertions. |

## Not verified

- `[UNTESTED]` A graphical Calamares run end to end. The job, the page
  sequence and the Nix mapping are tested separately; nobody has yet clicked
  through the installer and booted the result.
- `[UNTESTED]` That the installer finishes without a network connection. It
  does not: like upstream's job, `nixos-install` fetches what the ISO's store
  lacks (flake inputs, and any profile packages that differ from the live
  session's).

## An upstream observation

Upstream's `fix_btrfs_subvolumes` rewrites `subvol=` options in the hardware
scan. Its pattern, `fileSystems."<mp>"[^;]*"subvol=`, cannot get past the `;`
that ends the `device = "...";` line, so on real `nixos-generate-config`
output it changes nothing (checked against 0.3.23 with a `subvol=/home`
entry). The job calls it anyway, unchanged, so that a fix upstream reaches
ArchibaldOS too. The unit test asserts that parity rather than a rewrite.
