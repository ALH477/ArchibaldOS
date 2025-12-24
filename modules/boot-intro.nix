{ config, pkgs, lib, ... }:

let
  # ────────────────────────────────────────────────
  # Bash launcher with volume safety
  # ────────────────────────────────────────────────
  introLauncher = pkgs.writeShellScriptBin "archibaldos-intro-launcher" ''
    #!/usr/bin/env bash
    set -euo pipefail

    # Volume safety ───────────────────────────────────
    get_default_sink() {
        pactl info 2>/dev/null | grep -i '^default sink:' | awk '{print $3}' || echo ""
    }

    get_sink_volume() {
        local sink="$1"
        pactl get-sink-volume "$sink" 2>/dev/null \
            | grep -oP 'Volume:.*?/\s*\K[0-9]+%' | head -1 \
            | tr -d '%' | awk '{printf "%.2f", $1/100}' || echo ""
    }

    set_sink_volume() {
        local sink="$1"; local vol="$2"
        pactl set-sink-volume "\( sink" "'' \){vol}%" 2>/dev/null || true
    }

    DEFAULT_SINK=$(get_default_sink)
    ORIGINAL_VOLUME=""

    if [ -n "$DEFAULT_SINK" ]; then
        ORIGINAL_VOLUME=$(get_sink_volume "$DEFAULT_SINK")
        if [ -n "\( ORIGINAL_VOLUME" ] && (( \)(echo "$ORIGINAL_VOLUME > 0.60" | bc -l) )); then
            echo "Lowering volume to 45% for boot intro..." >&2
            set_sink_volume "$DEFAULT_SINK" "0.45"
        fi
    fi

    # Launch Python intro
    python3 "$HOME/intro.py" || echo "Boot intro failed" >&2

    # Restore volume
    if [ -n "$DEFAULT_SINK" ] && [ -n "$ORIGINAL_VOLUME" ]; then
        echo "Restoring original volume (${ORIGINAL_VOLUME})..." >&2
        set_sink_volume "$DEFAULT_SINK" "$ORIGINAL_VOLUME"
    fi
  '';

  # ────────────────────────────────────────────────
  # The full retro-professional Python animation script
  # ────────────────────────────────────────────────
  introPythonScript = pkgs.writeText "intro.py" ''
    #!/usr/bin/env python3
    """
    ArchibaldOS Launch Intro – Retro-professional boot sequence (2025 edition)

    Design goals:
    • Monochrome/limited palette (amber/green/cyan/white on black)
    • Blocky, chunky figlet fonts (DOS-era style)
    • CRT-like scanlines + subtle glitch/flicker (light, tasteful)
    • Old-school boot sequence vibe: scrolling text, system info reveal
    • Matrix-style subtle movement: plasma rain + sparse stars
    • Controlled pacing, no excessive flashing
    • Centered, readable text with smooth transitions
    • Clear branding: "ARCHIBALDOS" as focal point
    """

    import sys
    import time
    import subprocess
    import signal
    import random
    from pathlib import Path
    import glob
    import mido
    from asciimatics.screen import Screen
    from asciimatics.scene import Scene
    from asciimatics.effects import Cycle, Print, Scroll, Plasma, Stars, Effect
    from asciimatics.renderers import FigletText, StaticRenderer, Box
    from asciimatics.exceptions import ResizeScreenError, StopApplication

    # ────────────────────────────────────────────────
    # Configuration
    # ────────────────────────────────────────────────
    HOME = Path.home()
    SF2_HIGHFI = HOME / "sf2/highfi.sf2"
    SF2_LOFI   = HOME / "sf2/lofi.sf2"
    MIDI_DIR   = HOME / "midi"
    FRAMES_DIR = HOME / "intro-frames"

    GAIN       = 0.75
    POLYPHONY  = 128
    PERIOD_SIZE = 64
    BANK_OFFSET_LOFI = 20

    MAX_DURATION = 28

    RETRO_COLOURS = [
        Screen.COLOUR_YELLOW,    # amber glow
        Screen.COLOUR_GREEN,     # classic terminal green
        Screen.COLOUR_CYAN,      # subtle cyan accents
        Screen.COLOUR_WHITE      # clean highlights
    ]

    # ────────────────────────────────────────────────
    # Custom Glitch Effect – light, tasteful
    # ────────────────────────────────────────────────
    class Glitch(Effect):
        def __init__(self, screen, **kwargs):
            super().__init__(screen, **kwargs)
            self._timer = 0
            self._glitch_active = False
            self._glitch_duration = 0
            self._next_glitch = random.randint(90, 180)

        def reset(self):
            self._timer = 0
            self._glitch_active = False

        def _update(self, frame_no):
            self._timer += 1
            if self._timer >= self._next_glitch:
                self._glitch_active = True
                self._glitch_duration = random.randint(4, 9)
                self._next_glitch = self._timer + random.randint(140, 280)
                return True
            if self._glitch_active:
                self._glitch_duration -= 1
                if self._glitch_duration <= 0:
                    self._glitch_active = False
                return True
            return False

        def process_event(self, event):
            return event

        def _render(self, frame_no):
            if not self._glitch_active:
                return
            glitch_type = random.choice([1, 2, 3])
            if glitch_type == 1:
                shift = random.randint(-3, 3)
                for y in range(self._screen.height):
                    line = self._screen.get_from(0, y, self._screen.width)
                    if shift != 0:
                        self._screen.print_at(" " * abs(shift), 0 if shift > 0 else self._screen.width + shift, y)
                        for x in range(self._screen.width - abs(shift)):
                            char, fg, attr, bg = line[x if shift > 0 else x + abs(shift)]
                            self._screen.print_at(chr(char), x + shift if shift > 0 else x, y, fg, attr, bg)
            elif glitch_type == 2:
                for y in range(0, self._screen.height, 2):
                    if random.random() < 0.35:
                        for x in range(self._screen.width):
                            char, fg, attr, bg = self._screen.get_from(x, y)
                            if char:
                                new_fg = random.choice([Screen.COLOUR_GREEN, Screen.COLOUR_BLACK])
                                self._screen.print_at(chr(char), x, y, new_fg, Screen.A_DIM, bg)
            elif glitch_type == 3:
                for _ in range(20):
                    x = random.randint(0, self._screen.width - 1)
                    y = random.randint(0, self._screen.height - 1)
                    self._screen.print_at(random.choice(["░", "░", " "]), x, y, Screen.COLOUR_GREEN, Screen.A_DIM)

    # ────────────────────────────────────────────────
    # CRT Scanlines – subtle row dimming
    # ────────────────────────────────────────────────
    class CRTScanlines(Effect):
        def __init__(self, screen, **kwargs):
            super().__init__(screen, **kwargs)

        def _render(self, frame_no):
            for y in range(self._screen.height):
                if y % 2 == 1:
                    for x in range(self._screen.width):
                        char, fg, attr, bg = self._screen.get_from(x, y)
                        if char:
                            self._screen.print_at(chr(char), x, y, fg, Screen.A_DIM, bg)

    # ────────────────────────────────────────────────
    # Load user ASCII frames (boxed retro style)
    # ────────────────────────────────────────────────
    def load_frame_renderers():
        renderers = []
        i = 1
        while True:
            p = FRAMES_DIR / f"frame_{i:03d}.txt"
            if not p.exists():
                break
            try:
                text = p.read_text(encoding="utf-8").rstrip('\n')
                renderers.append(Box(StaticRenderer([text]), border=True, colour=Screen.COLOUR_GREEN))
            except Exception:
                pass
            i += 1
        return renderers

    # ────────────────────────────────────────────────
    # Main retro-professional scenes
    # ────────────────────────────────────────────────
    def retro_demo(screen):
        scenes = []
        height, width = screen.dimensions

        plasma = Plasma(height, width, RETRO_COLOURS, speed=0.28, attenuation=0.95)

        # Scene 1: Old-school boot sequence with scrolling text
        boot_lines = [
            "ARCHIBALDOS v1.0 – Real-Time Audio Operating System",
            "Kernel: CachyOS RT-BORE (PREEMPT_RT patched)",
            "Target latency: < 1ms @ 64 samples / 96 kHz",
            "PipeWire + JACK bridge initialized",
            "NixOS declarative configuration loaded",
            "Booting musician-first environment..."
        ]

        scenes.append(
            Scene([
                plasma,
                CRTScanlines(screen),
                Scroll(
                    screen,
                    StaticRenderer(boot_lines),
                    y=height//4,
                    colour=Screen.COLOUR_GREEN,
                    speed=4,
                    direction=1
                ),
                Print(
                    screen,
                    FigletText("ARCHIBALDOS", font="slant"),
                    height//6,
                    colour=Screen.COLOUR_YELLOW,
                    speed=2
                ),
                Stars(screen, 60),
                Glitch(screen)
            ], duration=10, clear=True)
        )

        # Scene 2: Branding reveal with frame cycle
        frame_renderers = load_frame_renderers()
        effects = [
            plasma,
            CRTScanlines(screen),
            Stars(screen, 50),
            Glitch(screen)
        ]

        if frame_renderers:
            for renderer in frame_renderers:
                effects.append(
                    Cycle(
                        screen,
                        renderer,
                        height//2 - renderer.max_height//2,
                        12,
                        colour=Screen.COLOUR_CYAN
                    )
                )
        else:
            effects.append(
                Cycle(
                    screen,
                    FigletText("A R C H I B A L D O S", font="big"),
                    height//3,
                    20,
                    colour=Screen.COLOUR_YELLOW
                )
            )

        scenes.append(
            Scene(effects, duration=MAX_DURATION - 18, clear=True)
        )

        # Scene 3: Clean finish
        scenes.append(
            Scene([
                plasma,
                CRTScanlines(screen),
                Print(
                    screen,
                    FigletText("READY", font="big"),
                    height//3,
                    colour=Screen.COLOUR_GREEN
                ),
                Print(
                    screen,
                    StaticRenderer([
                        "Ultra-low-latency • Reproducible • Musician-first",
                        "Press any key to continue..."
                    ]),
                    height - 6,
                    colour=Screen.COLOUR_CYAN,
                    speed=1
                )
            ], duration=8, clear=True)
        )

        screen.play(scenes, stop_on_resize=True, repeat=False)

    # ────────────────────────────────────────────────
    # MIDI playback in background
    # ────────────────────────────────────────────────
    def play_intro(screen):
        midi_files = glob.glob(str(MIDI_DIR / "intro_*.mid"))
        fs_proc = None

        if midi_files:
            selected = Path(random.choice(midi_files))
            print(f"Playing random motif: {selected.name}", file=sys.stderr)

            try:
                cmd = [
                    "fluidsynth", "-ni", "-a", "alsa", "-m", "alsa_seq",
                    "-o", f"synth.gain={GAIN}",
                    "-o", f"synth.polyphony={POLYPHONY}",
                    "-o", f"audio.period-size={PERIOD_SIZE}",
                    str(SF2_HIGHFI),
                    "-o", f"synth.bank-offset={BANK_OFFSET_LOFI}",
                    str(SF2_LOFI),
                    str(selected),
                ]
                fs_proc = subprocess.Popen(
                    cmd,
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL,
                    start_new_session=True
                )
                time.sleep(1.5)
            except Exception as e:
                print(f"Audio setup failed: {e}", file=sys.stderr)

        try:
            retro_demo(screen)
        except ResizeScreenError:
            pass
        except StopApplication:
            pass

        if fs_proc and fs_proc.poll() is None:
            fs_proc.terminate()
            try:
                fs_proc.wait(timeout=4.0)
            except subprocess.TimeoutExpired:
                fs_proc.kill()
                fs_proc.wait(timeout=2.0)

        raise StopApplication("Intro complete")

    # ────────────────────────────────────────────────
    # Entry point
    # ────────────────────────────────────────────────
    if __name__ == "__main__":
        while True:
            try:
                Screen.wrapper(play_intro, catch_interrupt=True)
                sys.exit(0)
            except ResizeScreenError:
                continue
            except StopApplication:
                sys.exit(0)
  '';

in {
  # ────────────────────────────────────────────────
  # Runtime dependencies
  # ────────────────────────────────────────────────
  environment.systemPackages = with pkgs; [
    fluidsynth
    soundfont-fluid
    (python3.withPackages (ps: with ps; [
      mido
      asciimatics
    ]))
    pulseaudio  # for pactl in launcher (works with PipeWire)
    bc          # for volume comparison in bash
  ];

  # ────────────────────────────────────────────────
  # Place files in /etc/skel → /home/nixos for live user
  # ────────────────────────────────────────────────
  environment.etc = {
    "skel/intro.py".source = introPythonScript;
    "skel/intro-launcher".source = introLauncher;

    # High-fidelity SoundFont (default)
    "skel/sf2/highfi.sf2".source = "${pkgs.soundfont-fluid}/share/soundfonts/FluidR3_GM2-2.sf2";

    # Lo-fi / retro SoundFont example (replace with real one)
    # "skel/sf2/lofi.sf2".source = pkgs.fetchurl {
    #   url = "https://musical-artifacts.com/.../module89.sf2";
    #   sha256 = "sha256-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=";
    # };

    # Example MIDI files (add your own MuseScore exports)
    "skel/midi/intro_01.mid".source = ./assets/midi/intro_01.mid;  # ← your files here
    "skel/midi/intro_02.mid".source = ./assets/midi/intro_02.mid;
    # ...

    # Optional: example ASCII frame (fallback)
    "skel/intro-frames/frame_001.txt".text = ''
      ╔════════════════════════════════════╗
      ║         ArchibaldOS v1.0           ║
      ║   Ultra-low-latency Real-Time OS   ║
      ╚════════════════════════════════════╝
    '';
  };

  # ────────────────────────────────────────────────
  # Make scripts executable & prepare directories
  # ────────────────────────────────────────────────
  system.activationScripts.archibaldosIntro = lib.stringAfter [ "users" "writeBoundary" ] ''
    chmod +x /etc/skel/intro.py
    chmod +x /etc/skel/intro-launcher

    mkdir -p /etc/skel/sf2 /etc/skel/midi /etc/skel/intro-frames
    chmod 755 /etc/skel/sf2 /etc/skel/midi /etc/skel/intro-frames
  '';

  # ────────────────────────────────────────────────
  # KDE Plasma autostart (runs after desktop loads)
  # ────────────────────────────────────────────────
  environment.etc."skel/.config/autostart/archibaldos-intro.desktop".text = ''
    [Desktop Entry]
    Type=Application
    Name=ArchibaldOS Boot Intro
    Exec=/home/nixos/intro-launcher
    Hidden=false
    NoDisplay=false
    X-GNOME-Autostart-enabled=true
    X-KDE-Autostart-after=plasma-desktop
    StartupNotify=false
  '';

  # ────────────────────────────────────────────────
  # Ensure PipeWire/Pulse compatibility for pactl
  # ────────────────────────────────────────────────
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    jack.enable = true;  # good for audio production focus
  };

  # Optional: make intro discoverable
  environment.variables.ARCHIBALDOS_INTRO = "/home/nixos/intro.py";
}
