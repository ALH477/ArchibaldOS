/* SPDX-License-Identifier: BSD-3-Clause
 * Copyright (c) 2026 DeMoD LLC.
 *
 * jackprobe — the three things checks.netjack2 needs from a JACK server,
 * written against libjack because jack-example-tools' `jack_lsp -s SERVER`
 * aborts under this nixpkgs' _FORTIFY_SOURCE ("buffer overflow detected");
 * plain jack_lsp, on the default server (as dsp-ctl runs it), works.
 *
 *   jackprobe SERVER ports                 list every port, one per line
 *   jackprobe SERVER tone  PORT SECONDS    play a 1 kHz sine into PORT
 *   jackprobe SERVER meter PORT SECONDS    print the peak seen on PORT
 *   jackprobe SERVER engine NAME SECONDS   stand in for demod-rt: a client named
 *                                          exactly NAME with in_L/in_R/out_L/out_R,
 *                                          copying each input to its output
 */
#include <jack/jack.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static jack_port_t *port;
static float peak;
static double phase;
static int mode_tone;
static jack_nframes_t rate;
static jack_port_t *eng_in[2], *eng_out[2];

static int process_engine(jack_nframes_t n, void *arg) {
    (void)arg;
    for (int ch = 0; ch < 2; ch++) {
        float *in = jack_port_get_buffer(eng_in[ch], n), *out = jack_port_get_buffer(eng_out[ch], n);
        memcpy(out, in, n * sizeof(float));
    }
    return 0;
}

static int process(jack_nframes_t n, void *arg) {
    (void)arg;
    float *buf = jack_port_get_buffer(port, n);
    for (jack_nframes_t i = 0; i < n; i++) {
        if (mode_tone) {
            buf[i] = 0.5f * (float)sin(phase);
            phase += 2.0 * M_PI * 1000.0 / rate;
            if (phase > 2.0 * M_PI) phase -= 2.0 * M_PI;
        } else if (fabsf(buf[i]) > peak) {
            peak = fabsf(buf[i]);
        }
    }
    return 0;
}

int main(int argc, char **argv) {
    if (argc < 3) { fprintf(stderr, "usage: jackprobe SERVER ports|tone|meter [PORT SECONDS]\n"); return 2; }
    jack_status_t st;
    char name[64];
    int engine = strcmp(argv[2], "engine") == 0;
    if (engine && argc >= 5) snprintf(name, sizeof(name), "%s", argv[3]);
    else snprintf(name, sizeof(name), "probe%d", (int)getpid());
    jack_client_t *c = jack_client_open(name, JackNoStartServer | JackServerName | (engine ? JackUseExactName : 0),
                                        &st, argv[1]);
    if (!c) { fprintf(stderr, "jackprobe: cannot open server %s (0x%x)\n", argv[1], st); return 1; }
    if (strcmp(argv[2], "ports") == 0) {
        const char **p = jack_get_ports(c, NULL, NULL, 0);
        for (int i = 0; p && p[i]; i++) printf("%s\n", p[i]);
        jack_free(p);
        jack_client_close(c);
        return 0;
    }
    if (argc < 5) { fprintf(stderr, "jackprobe: %s needs PORT SECONDS\n", argv[2]); return 2; }
    if (engine) {
        const char *io[2] = { "L", "R" };
        for (int ch = 0; ch < 2; ch++) {
            char pn[16];
            snprintf(pn, sizeof(pn), "in_%s", io[ch]);
            eng_in[ch] = jack_port_register(c, pn, JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
            snprintf(pn, sizeof(pn), "out_%s", io[ch]);
            eng_out[ch] = jack_port_register(c, pn, JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
        }
        jack_set_process_callback(c, process_engine, NULL);
        if (jack_activate(c)) { fprintf(stderr, "jackprobe: activate failed\n"); return 1; }
        sleep((unsigned)atoi(argv[4]));
        jack_client_close(c);
        return 0;
    }
    mode_tone = strcmp(argv[2], "tone") == 0;
    rate = jack_get_sample_rate(c);
    port = jack_port_register(c, mode_tone ? "out" : "in", JACK_DEFAULT_AUDIO_TYPE,
                              mode_tone ? JackPortIsOutput : JackPortIsInput, 0);
    jack_set_process_callback(c, process, NULL);
    if (jack_activate(c)) { fprintf(stderr, "jackprobe: activate failed\n"); return 1; }
    char me[128];
    snprintf(me, sizeof(me), "%s:%s", name, mode_tone ? "out" : "in");
    int rc = mode_tone ? jack_connect(c, me, argv[3]) : jack_connect(c, argv[3], me);
    if (rc) { fprintf(stderr, "jackprobe: cannot connect %s and %s (%d)\n", me, argv[3], rc); return 1; }
    sleep((unsigned)atoi(argv[4]));
    if (!mode_tone) printf("%.4f\n", peak);
    jack_client_close(c);
    return 0;
}
