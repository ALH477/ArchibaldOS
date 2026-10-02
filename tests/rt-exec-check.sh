#!/usr/bin/env bash
# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2026 DeMoD LLC.
#
# rt-exec-check.sh RT_EXEC — what rt-exec gives the process it execs, read back
# from that process's own /proc/self/status and limits. Not what rt-exec
# says it did: the version this replaced printed nothing and compiled its THP
# step out, so its target ran with THP enabled and VmLck 0 kB.
#
# Privilege-agnostic: inside a Nix build (no CAP_SYS_NICE) SCHED_FIFO must be
# REPORTED as a shortfall and --strict must refuse to start the target; with
# the privilege, the target must actually run SCHED_FIFO at the asked priority.
# Each branch asserts something, and the output says which branch ran.
# Exit 0 = every check passed.
set -u
RT=${1:?usage: rt-exec-check.sh /path/to/rt-exec}
W=$(mktemp -d); trap 'rm -rf "$W"' EXIT
n=0
pass() { n=$((n + 1)); echo "PASS: $*"; }
fail() { echo "FAIL: $*"; exit 1; }
note() { echo "NOTE: $*"; }

# ── 1. THP: off in the exec'd image ───────────────────────────────────────
base=$(grep -P '^THP_enabled:' /proc/self/status | cut -f2)
[ "$base" = 1 ] || fail "baseline THP_enabled is '$base', not 1: this check could not tell rt-exec from nothing"
"$RT" -- cat /proc/self/status >"$W/st" 2>"$W/err" || fail "rt-exec did not run its target: $(cat "$W/err")"
got=$(grep -P '^THP_enabled:' "$W/st" | cut -f2)
[ "$got" = 0 ] || fail "THP_enabled is '$got' in the target (baseline $base): the THP step did nothing"
pass "THP_enabled 1 -> 0 in the exec'd target"

# ── 2. CPU affinity: the asked CPU, in the target ─────────────────────────
allowed=$(grep -P '^Cpus_allowed_list:' /proc/self/status | cut -f2)
cpu=$(echo "$allowed" | tr ',' '\n' | tail -1 | sed 's/.*-//')
"$RT" --cpu "$cpu" -- cat /proc/self/status >"$W/st" 2>"$W/err" || fail "rt-exec --cpu $cpu failed: $(cat "$W/err")"
got=$(grep -P '^Cpus_allowed_list:' "$W/st" | cut -f2)
[ "$got" = "$cpu" ] || fail "target Cpus_allowed_list is '$got', asked for $cpu"
if [ "$allowed" = "$cpu" ]; then
    note "only CPU $cpu is available here, so affinity cannot be told from inheritance"
else
    pass "Cpus_allowed_list $allowed -> $cpu in the target"
fi

# ── 2b. --cpu any: affinity left exactly as inherited ───────────────────
"$RT" --cpu any -- cat /proc/self/status >"$W/st" 2>"$W/err" || fail "rt-exec --cpu any failed: $(cat "$W/err")"
got=$(grep -P '^Cpus_allowed_list:' "$W/st" | cut -f2)
[ "$got" = "$allowed" ] || fail "--cpu any changed affinity: '$allowed' -> '$got'"
grep -q 'cpu=any' "$W/err" || fail "--cpu any not named in the summary: $(cat "$W/err")"
pass "--cpu any leaves Cpus_allowed_list at $allowed"

# ── 3. The summary line is always printed ─────────────────────────────────
"$RT" --cpu "$cpu" -- cat /proc/self/status >/dev/null 2>"$W/err"
grep -q '^rt-exec: .* -> cat' "$W/err" || fail "no summary line on stderr: $(cat "$W/err")"
pass "one summary line names the target: $(grep '^rt-exec: .* -> cat' "$W/err")"

# ── 4. RLIMIT_MEMLOCK: soft raised (unlimited, or at least to hard) ───────
hard=$(bash -c 'ulimit -H -l')
got=$(bash -c "ulimit -S -l 64; exec \"$RT\" -- bash -c 'ulimit -S -l'" 2>"$W/err")
if [ "$hard" = unlimited ] || [ "$got" = unlimited ]; then
    [ "$got" = unlimited ] || fail "hard memlock is unlimited but the target's soft limit is $got"
    pass "soft memlock 64 KiB -> unlimited in the target"
else
    [ "$got" = "$hard" ] || fail "soft memlock 64 KiB stayed $got KiB (hard $hard): the target's own mlockall would be capped"
    grep -q '!memlock=' "$W/err" || fail "memlock is not unlimited and rt-exec did not say so: $(cat "$W/err")"
    pass "soft memlock 64 KiB -> hard ($hard KiB) in the target, and the shortfall is reported"
fi

# ── 5. SCHED_FIFO: established, or reported and --strict refuses ──────────
if chrt -f 1 true 2>/dev/null; then
    out=$("$RT" --prio 7 -- sh -c 'exec chrt -p $$' 2>"$W/err") || fail "rt-exec --prio 7 failed: $(cat "$W/err")"
    echo "$out" | grep -q 'SCHED_FIFO' || fail "target is not SCHED_FIFO: $out"
    echo "$out" | grep -q 'priority: 7' || fail "target priority is not 7: $out"
    pass "privileged: the target runs SCHED_FIFO at priority 7"
else
    "$RT" -- true 2>"$W/err" || fail "non-strict run did not start the target"
    grep -q '!SCHED_FIFO' "$W/err" || fail "SCHED_FIFO was refused here and rt-exec did not say so: $(cat "$W/err")"
    pass "unprivileged: the SCHED_FIFO shortfall is reported, not silent"
fi

# ── 6. --strict: a shortfall is fatal and the target never starts ─────────
"$RT" --strict -- touch "$W/ran" 2>"$W/err"; rc=$?
if grep -q ' !' "$W/err"; then
    [ "$rc" = 126 ] || fail "--strict with a shortfall exited $rc, not 126: $(cat "$W/err")"
    [ ! -e "$W/ran" ] || fail "--strict with a shortfall still ran the target"
    pass "--strict with a shortfall: exit 126, target not started"
else
    [ "$rc" = 0 ] && [ -e "$W/ran" ] || fail "--strict with no shortfall did not run the target (rc $rc)"
    pass "--strict with no shortfall: target ran"
fi

# ── 7. Usage errors are errors ────────────────────────────────────────────
for bad in "--cpu x true" "--cpu anything true" "--prio 0 true" "--prio 100 true" ""; do
    # shellcheck disable=SC2086
    "$RT" $bad >/dev/null 2>&1; rc=$?
    [ "$rc" = 2 ] || fail "rt-exec $bad exited $rc, not 2"
done
pass "malformed arguments exit 2 without running anything"

echo "$n checks passed"
