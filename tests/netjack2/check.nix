# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# checks.netjack2 — a box's audio reaches the DeMoD engine on a DSP host and
# comes back, over NetJack2, wired by jack-router, using the EXACT commands
# modules/netjack.nix and modules/jack-graph.nix generate (tests/roles.nix
# evaluates them; JACK_DEFAULT_SERVER picks which in-sandbox server each one
# talks to). Three real JACK servers (dummy drivers, --no-realtime), loopback:
#
#   dsp         jack-netmanager's command (listening on 0.0.0.0, the module's
#               default), an engine stand-in named demod-rt (copies in to
#               out), and jack-router with the DSP host's rules
#   box1, box2  jack-netadapter's command and jack-router with the box rules
#
# A 1 kHz tone is played into a box's netadapter:playback_1 and metered on the
# same box's netadapter:capture_1: box -> network -> from_slave_1 -> router ->
# demod-rt:in_L -> out_L -> router -> to_slave_1 -> network -> box. It must
# come back (peak ~0.5) for a box that joined before the DSP host's router
# started and for one that joined after, and must NOT come back before that
# router exists: the router makes the path, not the measurement.
#
# Unmeasured: a real interface, a real network, real-time scheduling, and
# demod-rt itself (the stand-in only copies). Peaks, not latency.
{ pkgs, roles }:

let
  probe = pkgs.callPackage ./probe.nix { };
  svc = cfg: name: cfg.config.systemd.services.${name}.serviceConfig.ExecStart;
  box1 = roles.adapter "box1";
  box2 = roles.adapter "box2";
in
pkgs.runCommand "netjack2-check"
{ nativeBuildInputs = [ pkgs.jack2 probe pkgs.gawk pkgs.gnugrep ]; }
  ''
    export JACK_NO_AUDIO_RESERVATION=1 HOME=$TMPDIR
    fail=0
    pass() { echo "PASS: $*"; }
    bad() { echo "FAIL: $*"; fail=1; }
    peak_ok() { awk -v p="$1" 'BEGIN { exit !(p > 0.4 && p < 0.6) }'; }
    on() { local s=$1; shift; JACK_DEFAULT_SERVER=$s "$@"; }
    roundtrip() { # SERVER: peak on its netadapter:capture_1 while a tone plays into playback_1
      jackprobe "$1" tone netadapter:playback_1 4 & local t=$!
      sleep 1; local p; p=$(jackprobe "$1" meter netadapter:capture_1 2); wait $t; echo "$p"
    }

    jackd -r -n dsp  -d dummy -r 48000 -p 256 > dsp.log  2>&1 & DSP=$!
    jackd -r -n box1 -d dummy -r 48000 -p 256 > box1.log 2>&1 & B1=$!
    jackd -r -n box2 -d dummy -r 48000 -p 256 > box2.log 2>&1 & B2=$!
    sleep 2

    echo "manager:  ${svc roles.manager "jack-netmanager"}"
    on dsp ${svc roles.manager "jack-netmanager"}
    jackprobe dsp engine demod-rt 600 & ENG=$!
    echo "adapter:  ${svc box1 "jack-netadapter"}"
    on box1 ${svc box1 "jack-netadapter"}
    on box1 ${svc box1 "jack-router"} 2> box1-router.log & BR1=$!
    sleep 4
    jackprobe dsp ports > ports1.txt
    if grep -qx 'box1:from_slave_1' ports1.txt; then pass "box1 joined the DSP host under its own name"
    else bad "box1 never appeared on the DSP host as box1"; cat ports1.txt; fi

    p0=$(roundtrip box1)
    if awk -v p="$p0" 'BEGIN { exit !(p < 0.01) }'; then pass "no DSP-host router, no path: box1 hears $p0"
    else bad "audio came back before the DSP host's router existed ($p0): the check would be vacuous"; fi

    echo "router:   ${svc roles.manager "jack-router"}"
    on dsp ${svc roles.manager "jack-router"} 2> dsp-router.log & R=$!
    sleep 2
    p1=$(roundtrip box1)
    if peak_ok "$p1"; then pass "box1 -> demod-rt -> box1 with the module's rules: peak $p1"
    else bad "box1 round trip: peak $p1"; cat dsp-router.log; fi

    on box2 ${svc box2 "jack-netadapter"}
    on box2 ${svc box2 "jack-router"} 2> box2-router.log & BR2=$!
    sleep 5
    p2=$(roundtrip box2)
    if peak_ok "$p2"; then pass "box2 joined after the router started and was wired anyway: peak $p2"
    else bad "box2 (late join) round trip: peak $p2"; jackprobe dsp ports; cat dsp-router.log; fi

    if grep -q 'system:capture_1 -> netadapter:playback_1' box1-router.log \
       && grep -q 'netadapter:capture_1 -> system:playback_1' box1-router.log; then
      pass "a box's router wires its interface to the adapter and back"
    else bad "box router did not wire system <-> netadapter"; cat box1-router.log; fi

    echo "DSP-host router log:"; sed 's/^/  /' dsp-router.log
    kill $R $BR1 $BR2 $ENG $DSP $B1 $B2 2>/dev/null || true
    [ $fail -eq 0 ] || exit 1
    touch $out
  ''
