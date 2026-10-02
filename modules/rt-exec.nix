# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 DeMoD LLC. All rights reserved.
# Nix derivation for rt-exec — real-time wrapper for JACK2 and demod-rt.
# Raises rlimits, sets SCHED_FIFO, pins a CPU and disables THP before exec;
# reports every step it could not establish (--strict makes that fatal).
# What it does is measured by checks.rt-exec in flake.nix, not assumed.
{ stdenv, lib }:

stdenv.mkDerivation {
  name = "rt-exec";
  src = ./rt-exec.c;

  dontUnpack = true;

  # -Werror: the THP step was once compiled out by a missing header with no
  # diagnostic at all; a warning must not be able to hide the next one.
  buildPhase = ''
    $CC -O2 -std=c11 -Wall -Wextra -Werror -o rt-exec $src
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp rt-exec $out/bin/rt-exec
  '';

  meta = {
    description = "Real-time process wrapper — rlimits + SCHED_FIFO + CPU pin + THP off, loud on shortfall";
    license = lib.licenses.bsd3;
    platforms = lib.platforms.linux;
  };
}
