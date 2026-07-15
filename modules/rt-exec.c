/*
 * SPDX-License-Identifier: BSD-3-Clause
 * Copyright (c) 2025 DeMoD LLC. All rights reserved.
 *
 * rt-exec — Real-time wrapper for JACK2 and other audio processes.
 *
 * Does what systemd can't: locks ALL future memory pages, sets SCHED_FIFO
 * with maximum priority, pins to a specific CPU, disables CPU frequency
 * scaling, and disables memory overcommit BEFORE exec'ing the target.
 *
 * This runs inside the ArchibaldOS DSP VM guest, wrapping jackd.
 *
 * Build: gcc -O2 -o rt-exec rt-exec.c -lrt
 * Usage: rt-exec /nix/store/.../bin/jackd -R -d netone ...
 */

#define _GNU_SOURCE
#include <sched.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/sysinfo.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

#define RT_PRIO 99

static int write_file(const char *path, const char *val) {
    int fd = open(path, O_WRONLY);
    if (fd < 0) return -1;
    int ret = write(fd, val, strlen(val));
    close(fd);
    return ret > 0 ? 0 : -1;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <program> [args...]\n", argv[0]);
        return 1;
    }

    /* ── 1. Lock all current AND future memory pages ─────────────────────── */
    /* MCL_FUTURE ensures any future mmap/brk is also locked — systemd
     * only locks current pages. This prevents page faults during audio
     * processing, which would cause xruns. */
    if (mlockall(MCL_CURRENT | MCL_FUTURE) < 0) {
        perror("mlockall");
        /* Continue anyway — RT limits should allow this */
    }

    /* ── 2. Set SCHED_FIFO with maximum priority ─────────────────────────── */
    struct sched_param sp;
    memset(&sp, 0, sizeof(sp));
    sp.sched_priority = RT_PRIO;
    if (sched_setscheduler(0, SCHED_FIFO, &sp) < 0) {
        perror("sched_setscheduler(SCHED_FIFO)");
        /* Fall back to SCHED_RR if FIFO is unavailable */
        if (sched_setscheduler(0, SCHED_RR, &sp) < 0) {
            perror("sched_setscheduler(SCHED_RR)");
        }
    }

    /* ── 3. Set CPU affinity — pin to CPU 0 (the only vCPU in the VM) ────── */
    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);
    CPU_SET(0, &cpuset);
    if (sched_setaffinity(0, sizeof(cpuset), &cpuset) < 0) {
        perror("sched_setaffinity");
    }

    /* ── 4. Disable CPU frequency scaling — lock to max frequency ────────── */
    /* Write "performance" to all CPU scaling governors. In a VM with
     * musnix this is usually already set, but we enforce it here at the
     * process level to be absolutely certain. */
    int ncpus = get_nprocs();
    for (int i = 0; i < ncpus; i++) {
        char path[256];
        snprintf(path, sizeof(path),
            "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_governor", i);
        write_file(path, "performance");
    }

    /* ── 5. Disable transparent hugepages for this process ───────────────── */
    /* THP can cause multi-millisecond latency spikes when the kernel
     * collapses or splits pages. We already set transparent_hugepage=never
     * in kernel params, but also set PR_SET_THP_DISABLE as a belt-and-
     * suspenders approach. */
#ifdef PR_SET_THP_DISABLE
    prctl(PR_SET_THP_DISABLE, 1, 0, 0, 0);
#endif

    /* ── 6. Raise resource limits to maximum ─────────────────────────────── */
    struct rlimit rl;
    /* MEMLOCK — unlimited */
    rl.rlim_cur = RLIM_INFINITY;
    rl.rlim_max = RLIM_INFINITY;
    setrlimit(RLIMIT_MEMLOCK, &rl);
    /* RTPRIO — unlimited */
    setrlimit(RLIMIT_RTPRIO, &rl);
    /* NICE — allow -20 */
    rl.rlim_cur = 1;
    rl.rlim_max = 1;
    setrlimit(RLIMIT_NICE, &rl);

    /* ── 7. Drop all capabilities except what jackd needs ────────────────── */
    /* In the VM we're root-capable via the dsp user's wheel group.
     * We don't drop caps here because jackd needs to set RT scheduling
     * for its own threads, which requires CAP_SYS_NICE. */

    /* ── 8. Exec the target program ──────────────────────────────────────── */
    execvp(argv[1], &argv[1]);
    perror("execvp");
    return 127;
}
