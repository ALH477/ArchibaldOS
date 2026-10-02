# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
# checks.rt-exec — run rt-exec-check.sh against the packaged rt-exec inside the
# build sandbox. Unprivileged there, so it takes the "SCHED_FIFO refused ->
# reported, --strict refuses" branch; the privileged branch runs wherever the
# script is run as root (tests/README.md).
{ pkgs, rt-exec }:

pkgs.runCommand "rt-exec-check"
  {
    nativeBuildInputs = with pkgs; [ bash coreutils gnugrep gnused util-linux ];
  }
  ''
    bash ${./rt-exec-check.sh} ${rt-exec}/bin/rt-exec | tee $out
  ''
