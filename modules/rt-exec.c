/*
 * SPDX-License-Identifier: BSD-3-Clause
 * Copyright (c) 2025-2026 DeMoD LLC. All rights reserved.
 *
 * rt-exec — set up a real-time process image, then exec the target in it.
 *
 *   rt-exec [--cpu N|any] [--prio P] [--strict] [--] PROGRAM [ARGS...]
 *   env:    RT_EXEC_CPU=N|any  RT_EXEC_PRIO=P  RT_EXEC_STRICT=1
 *
 * It establishes only what SURVIVES execve(2), because everything else is
 * undone before the target's first instruction:
 *
 *   1. RLIMIT_MEMLOCK, RLIMIT_RTPRIO and RLIMIT_NICE are raised (never
 *      lowered). Limits are inherited across exec, so the target's OWN
 *      mlockall(2) and SCHED_FIFO requests can then succeed.
 *   2. SCHED_FIFO at priority P (default 99). The policy survives exec.
 *   3. CPU affinity to N (default 0, the DSP guest's only vCPU). Survives.
 *      `any` leaves affinity alone: on a 2-core companion with no isolated
 *      CPU, pinning JACK to one core only takes the other one away from it.
 *   4. PR_SET_THP_DISABLE. Survives exec by design (prctl(2)), and is read
 *      back with PR_GET_THP_DISABLE rather than assumed.
 *
 * What it deliberately does NOT do, because it cannot work here:
 *
 *   - mlockall(MCL_CURRENT|MCL_FUTURE). Memory locks are removed by execve(2)
 *     and MCL_FUTURE does not carry over, so locking in the wrapper locks the
 *     wrapper. An earlier version did exactly that and reported success; the
 *     target ran with VmLck: 0 kB. The target must lock itself — jackd does
 *     under -R unless given -m, and demod-rt does — and step 1 is what lets it.
 *   - Writing cpufreq governors. That is machine policy, owned by the NixOS
 *     module (powerManagement.cpuFreqGovernor), and as the unprivileged audio
 *     user these writes always failed, silently.
 *
 * Every step that fails is reported on stderr, and one summary line is always
 * printed, so the journal says what the target actually got. With --strict a
 * shortfall is fatal: exit 126 and the target is NOT run, so a guest that
 * cannot provide RT fails loudly instead of running with xruns.
 *
 * The earlier version also called prctl() without <sys/prctl.h>, so
 * PR_SET_THP_DISABLE was undefined and the #ifdef silently compiled the THP
 * step out; and it set RLIMIT_NICE to 1, which caps nice at 19 — the opposite
 * of the "allow -20" its comment promised. checks.rt-exec in flake.nix runs
 * this binary and reads the target's /proc/self/status, so neither can recur
 * unnoticed.
 */
#define _GNU_SOURCE
#include <errno.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/resource.h>
#include <unistd.h>

#define DEFAULT_CPU  0
#define CPU_ANY      (-1)
#define DEFAULT_PRIO 99
#define EXIT_SHORT   126  /* --strict and a guarantee could not be established */
#define EXIT_USAGE   2

static int shortfalls = 0;
static char summary[512];

static void note(const char *fmt, const char *what) {
    size_t n = strlen(summary);
    snprintf(summary + n, sizeof(summary) - n, fmt, what);
}

static void fail(const char *step, int err) {
    fprintf(stderr, "rt-exec: %s: %s\n", step, strerror(err));
    note(" !%s", step);
    shortfalls++;
}

/* Raise a limit as far as this process may: (want, want) if privileged,
 * otherwise soft up to the current hard. Never lowers either value. Returns
 * the resulting soft limit. */
static rlim_t raise_limit(int res, rlim_t want) {
    struct rlimit cur;
    if (getrlimit(res, &cur) != 0) return 0;
    if (cur.rlim_cur != RLIM_INFINITY && (want == RLIM_INFINITY || cur.rlim_cur < want)) {
        struct rlimit up = cur;
        if (cur.rlim_max != RLIM_INFINITY && (want == RLIM_INFINITY || cur.rlim_max < want))
            up.rlim_max = want;
        up.rlim_cur = want;
        if (setrlimit(res, &up) != 0) {            /* unprivileged: soft = hard */
            up = cur;
            up.rlim_cur = cur.rlim_max;
            (void)setrlimit(res, &up);
        }
        (void)getrlimit(res, &cur);
    }
    return cur.rlim_cur;
}

static int parse_int(const char *s, int lo, int hi, int *out) {
    char *end;
    errno = 0;
    long v = strtol(s, &end, 10);
    if (errno || end == s || *end || v < lo || v > hi) return -1;
    *out = (int)v;
    return 0;
}

static int parse_cpu(const char *s, int *out) {
    if (strcmp(s, "any") == 0) { *out = CPU_ANY; return 0; }
    return parse_int(s, 0, CPU_SETSIZE - 1, out);
}

static void usage(void) {
    fprintf(stderr,
            "usage: rt-exec [--cpu N|any] [--prio 1-99] [--strict] [--] PROGRAM [ARGS...]\n"
            "       env RT_EXEC_CPU, RT_EXEC_PRIO, RT_EXEC_STRICT=1\n");
}

int main(int argc, char *argv[]) {
    int cpu = DEFAULT_CPU, prio = DEFAULT_PRIO, strict = 0;
    const char *e;
    if ((e = getenv("RT_EXEC_CPU")) && parse_cpu(e, &cpu)) {
        fprintf(stderr, "rt-exec: bad RT_EXEC_CPU '%s'\n", e); return EXIT_USAGE;
    }
    if ((e = getenv("RT_EXEC_PRIO")) && parse_int(e, 1, 99, &prio)) {
        fprintf(stderr, "rt-exec: bad RT_EXEC_PRIO '%s'\n", e); return EXIT_USAGE;
    }
    if ((e = getenv("RT_EXEC_STRICT")) && strcmp(e, "1") == 0) strict = 1;

    int i = 1;
    for (; i < argc; i++) {
        if (strcmp(argv[i], "--") == 0) { i++; break; }
        if (strcmp(argv[i], "--strict") == 0) { strict = 1; continue; }
        if (strcmp(argv[i], "--cpu") == 0) {
            if (i + 1 >= argc || parse_cpu(argv[i + 1], &cpu)) { usage(); return EXIT_USAGE; }
            i++;
            continue;
        }
        if (strcmp(argv[i], "--prio") == 0) {
            if (i + 1 >= argc || parse_int(argv[i + 1], 1, 99, &prio)) { usage(); return EXIT_USAGE; }
            i++;
            continue;
        }
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) { usage(); return 0; }
        break;  /* first non-option: the program */
    }
    if (i >= argc) { usage(); return EXIT_USAGE; }

    /* 1. Limits first: everything below, and the target's own requests, are
     *    checked against them. RLIMIT_NICE n allows nice down to 20 - n, so
     *    40 is what permits -20. */
    rlim_t memlock = raise_limit(RLIMIT_MEMLOCK, RLIM_INFINITY);
    if (memlock != RLIM_INFINITY) {
        char what[64];
        snprintf(what, sizeof(what), "memlock=%llu", (unsigned long long)memlock);
        fprintf(stderr, "rt-exec: RLIMIT_MEMLOCK is %llu bytes, not unlimited: "
                        "the target's own mlockall may fail\n", (unsigned long long)memlock);
        note(" !%s", what);
        shortfalls++;
    } else {
        note(" %s", "memlock=unlimited");
    }
    (void)raise_limit(RLIMIT_RTPRIO, (rlim_t)prio);
    (void)raise_limit(RLIMIT_NICE, 40);

    /* 2. SCHED_FIFO. No SCHED_RR fallback: it needs the same privilege. */
    struct sched_param sp;
    memset(&sp, 0, sizeof(sp));
    sp.sched_priority = prio;
    if (sched_setscheduler(0, SCHED_FIFO, &sp) != 0) fail("SCHED_FIFO", errno);
    else { char w[32]; snprintf(w, sizeof(w), "fifo:%d", prio); note(" %s", w); }

    /* 3. CPU affinity, unless `any`. */
    if (cpu == CPU_ANY) {
        note(" %s", "cpu=any");
    } else {
        cpu_set_t set;
        CPU_ZERO(&set);
        CPU_SET(cpu, &set);
        if (sched_setaffinity(0, sizeof(set), &set) != 0) fail("affinity", errno);
        else { char w[32]; snprintf(w, sizeof(w), "cpu%d", cpu); note(" %s", w); }
    }

    /* 4. Transparent hugepages off for this process and its exec'd image. */
    if (prctl(PR_SET_THP_DISABLE, 1, 0, 0, 0) != 0) fail("THP-disable", errno);
    else if (prctl(PR_GET_THP_DISABLE, 0, 0, 0, 0) != 1) fail("THP-disable(readback)", EIO);
    else note(" %s", "thp=off");

    fprintf(stderr, "rt-exec:%s -> %s%s\n", summary, argv[i],
            shortfalls ? (strict ? "  [strict: NOT started]" : "  [started with shortfalls]") : "");
    if (shortfalls && strict) return EXIT_SHORT;

    execvp(argv[i], &argv[i]);
    fprintf(stderr, "rt-exec: exec %s: %s\n", argv[i], strerror(errno));
    return 127;
}
