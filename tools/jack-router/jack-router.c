/* SPDX-License-Identifier: BSD-3-Clause
 * Copyright (c) 2026 DeMoD LLC.
 *
 * jack-router — keep a JACK graph wired the way the system says it should be.
 *
 *   jack-router [-s SERVER] 'SRC -> DST' ...
 *
 * Each rule connects SRC to DST whenever both ports exist. A side written as
 * `*:port` matches that port on ANY client, which is how a NetJack2 manager
 * routes boxes it has never heard of: every box joins as a client named after
 * its host, with from_slave_N / to_slave_N ports. So
 *
 *   '*:from_slave_1 -> demod-rt:in_L'   '*:to_slave_1 <- demod-rt:out_L'
 *
 * is written here as two rules, one per direction:
 *
 *   '*:from_slave_1 -> demod-rt:in_L'   'demod-rt:out_L -> *:to_slave_1'
 *
 * Rules are applied at start and again whenever a port or client appears
 * (JACK's registration callbacks only set a flag; connections are made from
 * the main thread, never from a JACK callback). Existing connections are left
 * alone; nothing is ever disconnected. A rule whose ports do not exist yet is
 * simply waiting. Exits when the server goes away, so systemd restarts it with
 * the server.
 *
 * Why not jack_connect in a loop: it cannot see a port that appears later (a
 * box joining an hour after boot) without polling jack_lsp, and reacting to
 * JACK's own registration callbacks is what a client is for.
 */
#include <jack/jack.h>
#include <errno.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MAX_RULES 64

struct rule { char src[256]; char dst[256]; };
static struct rule rules[MAX_RULES];
static int nrules;
static atomic_int dirty = 1;
static atomic_int gone = 0;
static volatile sig_atomic_t stop = 0;

static void on_port(jack_port_id_t id, int reg, void *arg) { (void)id; (void)arg; if (reg) dirty = 1; }
static void on_client(const char *name, int reg, void *arg) { (void)name; (void)arg; if (reg) dirty = 1; }
static void on_shutdown(void *arg) { (void)arg; gone = 1; }
static void on_signal(int sig) { (void)sig; stop = 1; }

static char *trim(char *s) {
    while (*s == ' ' || *s == '\t') s++;
    char *e = s + strlen(s);
    while (e > s && (e[-1] == ' ' || e[-1] == '\t')) *--e = '\0';
    return s;
}

static int parse_rule(const char *text, struct rule *r) {
    char buf[600];
    snprintf(buf, sizeof(buf), "%s", text);
    char *arrow = strstr(buf, "->");
    if (!arrow) return -1;
    *arrow = '\0';
    char *src = trim(buf), *dst = trim(arrow + 2);
    if (!*src || !*dst || !strchr(src, ':') || !strchr(dst, ':')) return -1;
    if (src[0] == '*' && dst[0] == '*') return -1;   /* one side must be concrete */
    snprintf(r->src, sizeof(r->src), "%s", src);
    snprintf(r->dst, sizeof(r->dst), "%s", dst);
    return 0;
}

/* The port's short name: what follows the client's colon. */
static const char *short_of(const char *full) {
    const char *c = strchr(full, ':');
    return c ? c + 1 : full;
}

static void link_pair(jack_client_t *c, const char *src, const char *dst) {
    int rc = jack_connect(c, src, dst);
    if (rc == 0) fprintf(stderr, "jack-router: %s -> %s\n", src, dst);
    else if (rc != EEXIST) fprintf(stderr, "jack-router: cannot connect %s -> %s (%d)\n", src, dst, rc);
}

static void apply(jack_client_t *c) {
    const char **all = jack_get_ports(c, NULL, NULL, 0);
    for (int i = 0; i < nrules; i++) {
        const struct rule *r = &rules[i];
        int wild_src = r->src[0] == '*', wild_dst = r->dst[0] == '*';
        const char *concrete = wild_src ? r->dst : r->src;
        if (!jack_port_by_name(c, concrete)) continue;          /* waiting */
        if (!wild_src && !wild_dst) {
            if (jack_port_by_name(c, r->dst)) link_pair(c, r->src, r->dst);
            continue;
        }
        const char *want = short_of(wild_src ? r->src : r->dst);
        for (int k = 0; all && all[k]; k++) {
            if (strcmp(short_of(all[k]), want) != 0) continue;
            if (wild_src) link_pair(c, all[k], r->dst);
            else link_pair(c, r->src, all[k]);
        }
    }
    jack_free(all);
}

int main(int argc, char **argv) {
    const char *server = NULL;
    int i = 1;
    if (i + 1 < argc && strcmp(argv[i], "-s") == 0) { server = argv[i + 1]; i += 2; }
    for (; i < argc; i++) {
        if (nrules == MAX_RULES) { fprintf(stderr, "jack-router: more than %d rules\n", MAX_RULES); return 2; }
        if (parse_rule(argv[i], &rules[nrules])) {
            fprintf(stderr, "jack-router: not a rule: '%s' (want 'client:port -> client:port', '*:' on one side at most)\n", argv[i]);
            return 2;
        }
        nrules++;
    }
    if (nrules == 0) { fprintf(stderr, "usage: jack-router [-s SERVER] 'SRC -> DST' ...\n"); return 2; }

    jack_status_t st;
    jack_options_t opt = JackNoStartServer | (server ? JackServerName : 0);
    jack_client_t *c = jack_client_open("jack-router", opt, &st, server);
    if (!c) { fprintf(stderr, "jack-router: no JACK server%s%s (0x%x)\n", server ? " " : "", server ? server : "", st); return 1; }
    jack_set_port_registration_callback(c, on_port, NULL);
    jack_set_client_registration_callback(c, on_client, NULL);
    jack_on_shutdown(c, on_shutdown, NULL);
    if (jack_activate(c)) { fprintf(stderr, "jack-router: activate failed\n"); return 1; }
    signal(SIGTERM, on_signal);
    signal(SIGINT, on_signal);
    fprintf(stderr, "jack-router: %d rule(s)\n", nrules);

    while (!stop && !gone) {
        if (atomic_exchange(&dirty, 0)) apply(c);
        usleep(200 * 1000);
    }
    if (!gone) jack_client_close(c);
    return gone ? 1 : 0;
}
