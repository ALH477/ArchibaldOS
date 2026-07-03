# modules/riscv-cross-overlay.nix — qemu-user cross-build test mitigations
# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025-2026 DeMoD LLC.
#
# When cross-building a riscv64 image from an x86_64 host under binfmt
# qemu-user, a handful of upstream package test suites fail because qemu-user
# does not translate certain syscalls (AF_ALG, IPV6_MULTICAST_IF, ptrace/
# PTRACE_SETOPTIONS, AF_UNIX store API, …). Those failures are emulation
# artifacts, not real bugs — on native riscv64 hardware the tests pass.
#
# This overlay disables just those flaky check phases, and ONLY when actually
# cross-compiling (build platform != host platform). On a native riscv64
# build the overrides become no-ops and every upstream test runs as authored.
#
# Each override is guarded with `prev ? <name>` so the overlay stays valid
# across nixpkgs versions where an attribute may be absent or renamed.

{ ... }:

{
  nixpkgs.overlays = [
    (final: prev:
      let
        isCross =
          prev.stdenv.buildPlatform.system != prev.stdenv.hostPlatform.system;
        isRiscv64 = prev.stdenv.hostPlatform.system == "riscv64-linux";

        # Disable check + installCheck phases (heaviest skip).
        skipChecks = pkg:
          pkg.overrideAttrs (_: {
            doCheck = false;
            doInstallCheck = false;
            checkPhase = "true";
            installCheckPhase = "true";
          });
        # Disable only the (unit) check phase.
        skipCheck = pkg: pkg.overrideAttrs (_: { doCheck = false; });

        # Apply `f` to prev.<name> if it exists (and we're cross-building),
        # producing a { name = ...; } fragment; otherwise {}.
        whenCross = f: names:
          if !isCross then {}
          else lib.foldl'
            (acc: n: acc // (if prev ? ${n} then { ${n} = f prev.${n}; } else {}))
            {} names;

        # A few skips also matter on native riscv64 (real upstream quirks,
        # not qemu artifacts) — apply when cross OR native riscv64.
        whenRiscv = f: names:
          if !(isCross || isRiscv64) then {}
          else lib.foldl'
            (acc: n: acc // (if prev ? ${n} then { ${n} = f prev.${n}; } else {}))
            {} names;

        lib = prev.lib;
      in
        # qemu-user test artifacts (skip only the check phase):
        (whenCross skipCheck [
          "libuv"          # udp_multicast_interface6: IPV6_MULTICAST_IF not translated
          "boehmgc"        # gctest SIGABRT under emulated stack scanning
          "openssl"        # 30-test_afalg.t: AF_ALG crypto socket not translated
          "coreutils"      # env-signal-handler: imperfect signal emulation
          "coreutils-full"
          "sqlite"         # like-14.2: locale/collation quirk under emulation
          "libseccomp"     # BPF simulation test (1/5118)
          "onetbb"         # test_malloc_compliance iteration count
          "gitMinimal"     # 4000+ shell tests: pass on hw, ~2h under qemu
          "libeatmydata"   # uses PTRACE_SETOPTIONS
          "valgrind"       # pulled by lilv's LV2 test suite; fails under qemu
        ])
        # Heavier packages: skip check + installCheck:
        // (whenCross skipChecks [
          "elfutils"       # backtrace/ptrace tests: ptrace not fully emulated
          "protobuf" "zeromq" "gettext" "opensp" "lttng-ust"
          "autoconf" "file" "cmocka" "rsync"
        ])
        # nix's own functional tests don't work under emulation, and on native
        # riscv64 they drag mercurial → numpy and burn hours:
        // (whenRiscv skipChecks [ "nix" ])
    )
  ];
}
