# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
{ stdenv, jack2 }:
stdenv.mkDerivation {
  pname = "jack-router";
  version = "1.0";
  dontUnpack = true;
  buildInputs = [ jack2 ];
  buildPhase = "$CC -O2 -Wall -Wextra -Werror -std=c11 -D_DEFAULT_SOURCE -o jack-router ${./jack-router.c} -ljack";
  installPhase = "install -Dm755 jack-router $out/bin/jack-router";
  meta.mainProgram = "jack-router";
}
