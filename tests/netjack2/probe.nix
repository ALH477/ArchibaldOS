# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
{ stdenv, jack2 }:
stdenv.mkDerivation {
  pname = "jackprobe";
  version = "1";
  dontUnpack = true;
  buildInputs = [ jack2 ];
  buildPhase = "$CC -O2 -Wall -Wextra -o jackprobe ${./jackprobe.c} -ljack -lm";
  installPhase = "install -Dm755 jackprobe $out/bin/jackprobe";
}
