# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 DeMoD LLC. All rights reserved.
# Nix derivation for rt-exec — real-time wrapper for JACK2.
# Does mlockall(MCL_FUTURE), SCHED_FIFO(99), CPU pinning, THP disable,
# and max rlimits before exec'ing jackd. Eliminates page-fault xruns.
{ stdenv, gcc }:

stdenv.mkDerivation {
  name = "rt-exec";
  src = ./rt-exec.c;

  nativeBuildInputs = [ gcc ];

  dontUnpack = true;

  buildPhase = ''
    gcc -O2 -Wall -Wextra -o rt-exec $src
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp rt-exec $out/bin/rt-exec
  '';

  meta = with stdenv.lib; {
    description = "Real-time process wrapper — mlockall + SCHED_FIFO + CPU pin for audio";
    license = licenses.bsd3;
    platforms = platforms.linux;
  };
}
