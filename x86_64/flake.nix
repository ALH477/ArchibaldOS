{
  description = "Lean RT Audio ArchibaldOS: Minimal Oligarchy NixOS variant for real-time audio with Musnix, Hyprland, HydraMesh, and StreamDB";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    musnix.url = "github:musnix/musnix";
    hyprland.url = "github:hyprwm/Hyprland";
    hydramesh.url = "path:../HydraMesh";  # Assumed flake in sibling dir
    streamdb.url = "path:../StreamDB";    # Assumed Rust crate flake
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, musnix, hyprland, hydramesh, streamdb, disko }: let
    system = "x86_64-linux";  # x86_64 focus
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;  # For any unfree audio tools if needed
      overlays = [
        (final: prev: rec {
          hyprland = hyprland.packages.${system}.hyprland;
          hydramesh-pkg = hydramesh.packages.${system}.default or (pkgs.buildGoModule rec {
            pname = "hydramesh";
            version = "0.1.0";
            src = hydramesh;
            vendorHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";  # Compute with `nix build`
            meta = with pkgs.lib; { description = "P2P mesh for audio"; license = licenses.mit; };
          });
          streamdb-pkg = streamdb.packages.${system}.streamdb or (pkgs.rustPlatform.buildRustPackage rec {
            pname = "streamdb";
            version = "0.1.0";
            src = streamdb;
            cargoHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";  # Compute with `nix build`
            meta = with pkgs.lib; { description = "StreamDB for audio metadata"; license = licenses.mit; };
          });
        })
      ];
    };

    # Lean audio package list: Core RT audio essentials only
    audioPackages = with pkgs; [
      # DAWs/MIDI
      ardour audacity fluidsynth musescore

      # DSP/Tools
      csound faust portaudio rtaudio supercollider qjackctl guitarix

      # Synths/Modular
      surge vcvrack pd
    ];

    # Basic software additions: Minimal file manager, text editor, browser
    basicPackages = with pkgs; [
      pcmanfm  # Lightweight file manager
      vim      # Text editor (console-based for leanness)
      brave    # Browser
    ];

    wallpaperSrc = ../wallpaper.jpg;
  in {
    nixosConfigurations = {
      archibaldOS = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          musnix.nixosModules.musnix
          hyprland.nixosModules.default
          ({ config, pkgs, ... }: {
            # Optimized Musnix RT setup: Full low-latency audio optimizations
            musnix.enable = true;
            musnix.kernel.realtime = true;
            musnix.kernel.packages = pkgs.linuxPackages_latest_rt;
            musnix.alsaSeq.enable = true;
            musnix.rtirq.enable = true;
            musnix.das_watchdog.enable = true;  # Prevents RT process hangs

            # PipeWire for RT audio
            sound.enable = true;
            hardware.pulseaudio.enable = false;
            services.pipewire = {
              enable = true;
              alsa.enable = true;
              pulse.enable = true;
              jack.enable = true;
              extraConfig.pipewire."92-low-latency" = {
                "context.properties" = {
                  "default.clock.rate" = 96000;  # Higher quality, lower latency
                  "default.clock.quantum" = 32;
                  "default.clock.min-quantum" = 16;
                  "default.clock.max-quantum" = 64;
                };
              };
            };

            security.rtkit.enable = true;  # RT priorities for audio users

            # Minimal user for audio focus
            users.users.audio-user = {
              isNormalUser = true;
              extraGroups = [ "audio" "jackaudio" "video" "wheel" ];
              home = "/home/audio-user";
              createHome = true;
              shell = pkgs.bash;
              packages = [
                (pkgs.runCommand "wallpaper" {} ''
                  mkdir -p $out/Pictures
                  cp ${wallpaperSrc} $out/Pictures/wall.jpg
                '')
              ];
            };

            # Optimized kernel params and sysctl for RT audio
            boot.kernelParams = [ "threadirqs" "isolcpus=1-3" "nohz_full=1-3" "intel_idle.max_cstate=1" "processor.max_cstate=1" ];  # Disable CPU C-states
            boot.kernel.sysctl = {
              "vm.swappiness" = 0;  # Disable swap for RT
              "fs.inotify.max_user_watches" = 600000;
            };
            environment.etc."sysctl.d/99-audio.conf".text = ''
              dev.rtc.max-user-freq = 2048
              dev.hpet.max-user-freq = 2048
            '';
            powerManagement.cpuFreqGovernor = "performance";  # Consistent RT performance

            # ALSA configuration for low-latency
            environment.etc."asound.conf".text = ''
              defaults.pcm.dmix.rate 96000
              defaults.pcm.dmix.format S32_LE
              defaults.pcm.dmix.buffer_size 32
            '';

            # Ardour pre-configuration for low-latency
            environment.etc."ardour6/ardour.rc".text = ''
              <JACK buffer-size="32" sample-rate="96000" periods="2"/>
            '';

            # Disable non-essential services for RT audio
            systemd.services."disable-non-essential" = {
              description = "Disable non-essential services for RT audio";
              wantedBy = [ "multi-user.target" ];
              serviceConfig = {
                Type = "oneshot";
                ExecStart = "${pkgs.systemd}/bin/systemctl stop NetworkManager bluetooth";
                RemainAfterExit = true;
              };
            };

            # USB libraries/drivers for audio/MIDI interfaces
            hardware.usb.enable = true;
            boot.kernelModules = [ "snd_usb_audio" "usbhid" "usbmidi" ];
            boot.extraModprobeConfig = ''
              options snd_usb_audio nrpacks=1 low_latency=1
            '';
            environment.systemPackages = audioPackages ++ basicPackages ++ [ pkgs.usbutils pkgs.libusb pkgs.alsa-firmware pkgs.alsa-tools ];  # USB and audio firmware/tools

            services.xserver.enable = true;
            services.displayManager.sddm.enable = true;
            services.displayManager.sddm.wayland.enable = true;
            services.displayManager.sddm.settings = {
              General.Background = "/etc/hypr/wallpaper.jpg";
            };
            environment.etc."hypr/wallpaper.jpg".source = wallpaperSrc;

            programs.hyprland.enable = true;

            # Minimal Hyprland config: Core UX with RT audio focus
            environment.etc."hypr/hyprland.conf".text = ''
              exec-once = kitty
              exec-once = waybar
              exec-once = hyprpaper
              monitor=,preferred,auto,1
              $terminal = kitty
              $menu = wofi --show drun
              general {
                gaps_in = 5
                gaps_out = 10
                border_size = 2
                col.active_border = rgba(33ff33aa) rgba(00ff00aa) 45deg
                col.inactive_border = rgba(595959aa)
                layout = dwindle
              }
              decoration {
                rounding = 12
                blur {
                  enabled = true
                  size = 8
                  passes = 2
                }
              }
              animations {
                enabled = true
              }
              dwindle {
                pseudotile = true
                preserve_split = true
              }
              input {
                kb_layout = us
                follow_mouse = 1
                touchpad {
                  natural_scroll = true
                }
              }
              gestures {
                workspace_swipe = true
              }
              $mainMod = SUPER
              bind = $mainMod, Q, exec, $terminal
              bind = $mainMod, C, killactive,
              bind = $mainMod, M, exit,
              bind = $mainMod, D, exec, $menu
              bind = $mainMod, H, movefocus, l
              bind = $mainMod, L, movefocus, r
              bind = $mainMod, K, movefocus, u
              bind = $mainMod, J, movefocus, d
              bind = $mainMod, 1, workspace, 1
              bind = $mainMod, 2, workspace, 2
              bind = $mainMod, 3, workspace, 3
              bind = $mainMod, 4, workspace, 4
              bind = $mainMod, 5, workspace, 5
              bind = $mainMod, P, exec, hyprctl keyword decoration:blur:enabled 0  # Disable blur for audio
              bindel = ,XF86AudioRaiseVolume, exec, wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+
              bindel = ,XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-
              bindel = ,XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
              bind = $mainMod, PRINT, exec, grim ~/Pictures/Screenshots/screenshot-$(date +%F-%T).png
              bind = $mainMod SHIFT, PRINT, exec, grim -g "$(slurp)" ~/Pictures/Screenshots/screenshot-$(date +%F-%T).png
              bind = $mainMod SHIFT, H, exec, hydramesh-toggle
              bind = $mainMod SHIFT, K, exec, ~/.config/hypr/keybindings_cheatsheet.sh
            '';
            environment.etc."hypr/hyprpaper.conf".text = ''
              preload = ~/Pictures/wall.jpg
              wallpaper = ,~/Pictures/wall.jpg
            '';
            environment.etc."waybar/config".text = ''
              {
                  "layer": "top",
                  "position": "top",
                  "height": 32,
                  "modules-center": ["hyprland/workspaces", "cpu", "memory", "clock", "network", "custom/hydramesh"],
                  "hyprland/workspaces": {
                      "format": "{icon}",
                      "persistent-workspaces": {"*": 5}
                  },
                  "cpu": {
                      "format": "\uf2db {usage}%"
                  },
                  "memory": {
                      "format": "\uf538 {used:0.1f}GiB"
                  },
                  "clock": {
                      "format": "\uf017 {:%H:%M}"
                  },
                  "network": {
                      "format-wifi": "\uf1eb {essid}",
                      "format-ethernet": "\uf6ff {ipaddr}",
                      "format-disconnected": "\u26a0"
                  },
                  "custom/hydramesh": {
                      "format": "{}",
                      "exec": "${pkgs.hydramesh-pkg}/bin/hydramesh-status",
                      "interval": 5
                  }
              }
            '';
            environment.etc."waybar/style.css".text = ''
              * {
                  font-family: "FiraCode Nerd Font", monospace;
                  font-size: 14px;
              }
              window#waybar {
                  background: transparent;
                  color: #ffffff;
              }
              .modules-center {
                  background: rgba(10,10,10,0.85);
                  border-radius: 16px;
                  padding: 0 20px;
              }
              #workspaces button.active {
                  background: rgba(51,255,51,0.3);
                  border-radius: 8px;
              }
              #cpu, #memory, #clock, #network, #custom-hydramesh {
                  padding: 0 12px;
                  color: #33ff33;
              }
            '';
            environment.etc."wofi/style.css".text = ''
              window {
                  border: 2px solid #33ff33;
                  background-color: rgba(10,10,10,0.85);
                  border-radius: 12px;
                  font-family: "FiraCode Nerd Font", monospace;
                  font-size: 14px;
              }
              #input {
                  background-color: rgba(51,255,51,0.1);
              }
              #text {
                  color: #33ff33;
              }
              #entry:selected {
                  background-color: rgba(51,255,51,0.3);
                  border-radius: 8px;
              }
            '';
            environment.etc."qjackctl.conf".text = ''
              [Settings]
              Frames=32
              Periods=2
              SampleRate=96000
            '';
            environment.etc."keybindings_cheatsheet.sh" = {
              text = ''
                #!/usr/bin/env bash
                zenity --info --title="Hyprland Keybindings" --text="SUPER + Q: Terminal\nSUPER + D: Launcher\nSUPER + P: Toggle Blur (RT Audio)\nSUPER + 1-5: Workspaces\nXF86Audio*: Volume\nSUPER + SHIFT + H: Toggle HydraMesh\nSUPER + SHIFT + K: This Sheet"
              '';
              mode = "0755";
            };
            environment.etc."audio-setup.sh" = {
              text = ''
#!/usr/bin/env bash

set -euo pipefail  # Strict mode: exit on error, unset vars, pipe failures

# ArchibaldOS Post-Install Audio Setup Script by DeMoD LLC
# Version 1.2 - Enhanced with IRQ affinity pinning and RT priority setting
# Optimizes RT audio: checks, latency tweaks, hardware setup, kernel switching
# Run as non-root; uses sudo where needed. Based on NixOS manuals and Musnix docs.

LOG_FILE="$HOME/archibaldos-audio-setup.log"
exec > >(tee -a "$LOG_FILE") 2>&1  # Log all output

# Usage/help function
usage() {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  --help          Show this help message"
  echo "  --skip-ascii    Skip ASCII art display"
  echo "  --dry-run       Simulate actions without applying changes"
  echo "  --user <name>   Specify username (skips prompt)"
  echo "  --pci <id>      Specify PCI ID (skips prompt)"
  exit 0
}

# Parse options
DRY_RUN=false
SKIP_ASCII=false
USER=""
PCI_ID=""
while [[ $# -gt 0 ]]; do
  case $1 in
    --help) usage ;;
    --skip-ascii) SKIP_ASCII=true; shift ;;
    --dry-run) DRY_RUN=true; shift ;;
    --user) USER="$2"; shift 2 ;;
    --pci) PCI_ID="$2"; shift 2 ;;
    *) echo "Unknown option: $1"; usage ;;
  esac
done

# Display intro (optional ASCII)
echo "ArchibaldOS Audio Setup - DeMoD LLC (v1.2)"
if [ "$SKIP_ASCII" = false ]; then
  echo '                       *'
  echo '                      * *'
  echo '                     *   *'
  echo '                    * * * *'
  echo '                   *       *'
  echo '                  * *     * *'
  echo '                 *   *   *   *'
  echo '                * * * * * * * *'
  echo '               *               *'
  echo '              * *             * *'
  echo '             *   *           *   *'
  echo '            * * * *         * * * *'
  echo '           *       *       *       *'
  echo '          * *     * *     * *     * *'
  echo '         *   *   *   *   *   *   *   *'
  echo '        * * * * * * * * * * * * * * * *'
fi

# Detect live ISO
if mount | grep -q '/nix/.ro-store type squashfs'; then
  IS_LIVE=true
  echo "Live ISO detected. Some features (e.g., kernel switching) limited; install for full access."
else
  IS_LIVE=false
fi

# Check/install dependencies (transient via nix-shell if missing)
REQUIRED_PKGS=("realTimeConfigQuickScan" "qjackctl" "pciutils" "usbutils" "linuxPackages.stress-ng" "jack2")
for pkg in "${REQUIRED_PKGS[@]}"; do
  if ! command -v $(nix-locate --top-level bin/$pkg | awk '{print $1}' | head -1) &> /dev/null; then
    echo "Missing $pkg. Running in nix-shell..."
    nix-shell -p $pkg --command "$0 $@"  # Re-run script in shell with pkg
    exit $?
  fi
done
echo "All dependencies available."

# Prompt for user if not provided
if [ -z "$USER" ]; then
  read -p "Enter username: " USER
fi
if ! id "$USER" &>/dev/null; then
  echo "Error: User $USER does not exist."
  exit 1
fi

# Add user to audio/realtime groups
echo "Adding $USER to audio/realtime groups..."
if [ "$DRY_RUN" = false ]; then
  sudo usermod -aG audio,realtime "$USER" || echo "Failed to add groups; check sudo privileges."
else
  echo "[Dry-run] Would add $USER to audio/realtime."
fi
echo "Persist in configuration.nix: users.users.$USER.extraGroups = [ \"audio\" \"realtime\" ];"

# Latency tweaks (sysctl, RTC/HPET)
echo "Applying latency tweaks..."
if [ "$DRY_RUN" = false ]; then
  sudo sysctl vm.swappiness=10 || true
  sudo sysctl fs.inotify.max_user_watches=600000 || true
  echo 2048 | sudo tee /sys/class/rtc/rtc0/max_user_freq || true
  echo 2048 | sudo tee /proc/sys/dev/hpet/max-user-freq || true
else
  echo "[Dry-run] Would apply sysctl tweaks."
fi
echo "Persist in configuration.nix: boot.kernel.sysctl = { \"vm.swappiness\" = 10; \"fs.inotify.max_user_watches\" = 600000; };"

# Detect/optimize audio hardware
echo "Detecting audio devices..."
lspci | grep -i audio
lsusb | grep -i audio
AUTO_PCI_ID=$(lspci | grep -i audio | awk '{print $1}' | head -1)
if [ -n "$AUTO_PCI_ID" ] && [ -z "$PCI_ID" ]; then
  PCI_ID="$AUTO_PCI_ID"
  echo "Auto-detected audio PCI ID: $PCI_ID"
fi
if [ ! -z "$PCI_ID" ] && [[ "$PCI_ID" =~ ^[0-9a-f]{2}:[0-9a-f]{2}\.[0-9a-f]$ ]]; then
  echo "Optimizing latency timer for $PCI_ID..."
  if [ "$DRY_RUN" = false ]; then
    sudo setpci -v -s "$PCI_ID" latency_timer=ff || echo "Failed to set latency timer."
  else
    echo "[Dry-run] Would set latency timer for $PCI_ID."
  fi
else
  echo "Skipping PCI optimization (invalid or empty ID)."
fi

# IRQ Affinity Pinning
echo "Pinning audio IRQs to CPU1..."
if [ "$DRY_RUN" = false ]; then
  AUDIO_IRQ=$(cat /proc/interrupts | grep -i snd | awk '{print $1}' | sed 's/:$//' | head -1)
  [ -n "$AUDIO_IRQ" ] && echo 2 | sudo tee /proc/irq/$AUDIO_IRQ/smp_affinity || echo "No audio IRQ found."
else
  echo "[Dry-run] Would pin audio IRQ to CPU1."
fi

# RT Priorities for Audio Apps
echo "Setting RT priorities for audio apps..."
if [ "$DRY_RUN" = false ]; then
  for app in ardour qjackctl; do
    if pidof $app >/dev/null; then
      sudo chrt -f -p 80 $(pidof $app) || echo "Failed to set priority for $app."
    fi
  done
else
  echo "[Dry-run] Would set RT priorities for ardour, qjackctl."
fi

# Start qjackctl for config
echo "Starting qjackctl..."
if [ "$DRY_RUN" = false ]; then
  qjackctl & || echo "Failed to start qjackctl; ensure it's installed."
else
  echo "[Dry-run] Would start qjackctl."
fi

# Latency measurement
echo "Testing JACK latency..."
if command -v jack_iodelay &>/dev/null; then
  jack_iodelay | grep "total roundtrip latency" || echo "Start JACK server first (via qjackctl)."
else
  echo "Install jack_iodelay: nix-shell -p jack2 --run jack_iodelay"
fi
echo "Testing kernel latency..."
if command -v cyclictest &>/dev/null; then
  cyclictest -l 100000 -m -n -p99 -q | grep -E "Max|Avg" || echo "Failed; ensure RT kernel."
else
  echo "Install cyclictest: nix-shell -p linuxPackages.stress-ng --run cyclictest"
fi

# MIDI device check
if command -v amidi &>/dev/null; then
  echo "Listing MIDI devices..."
  amidi -l || echo "No MIDI devices detected."
else
  echo "Install amidi: nix-shell -p alsa-utils --run amidi"
fi

# Kernel Switching (using specialisations)
echo "Kernel Management: Default RT for low-latency; LTS backup for stability."
if [ "$IS_LIVE" = true ]; then
  echo "Live mode: Test RT with 'cyclictest -l 100000 -m -n -p99 -q'."
else
  read -p "Switch to LTS backup kernel? (y/n): " SWITCH
  if [ "$SWITCH" = "y" ]; then
    if [ "$DRY_RUN" = false ]; then
      sudo nixos-rebuild boot --specialisation lts-backup || echo "Failed; ensure specialisation in configuration.nix."
      echo "LTS set for next boot. Reboot to apply."
    else
      echo "[Dry-run] Would switch to LTS."
    fi
  else
    echo "Staying on RT. To switch: sudo nixos-rebuild boot --specialisation lts-backup"
  fi
fi

# Final verification
echo "Current kernel: $(uname -r)"
echo "Setup complete! Reboot if needed. Log saved to $LOG_FILE."
echo "Persist specialisations: specialisation.lts-backup.configuration = { boot.kernelPackages = pkgs.linuxPackages_lts; };"
              '';
              mode = "0755";
            };

            # Optional HydraMesh service: Minimal for RT audio networking
            systemd.services.hydramesh = {
              enable = false;  # Disabled by default; enable via config
              description = "HydraMesh for RT Audio P2P";
              wantedBy = [ "multi-user.target" ];
              serviceConfig = {
                ExecStart = "${pkgs.hydramesh-pkg}/bin/hydramesh --config /etc/hydramesh/config.toml";
                Restart = "always";
                User = "hydramesh";
                DynamicUser = true;
                PrivateDevices = true;
                ProtectSystem = "strict";
                ProtectHome = true;
                PrivateTmp = true;
                NoNewPrivileges = true;
                CapabilityBoundingSet = "";
                RestrictNamespaces = true;
                SystemCallFilter = "@system-service ~@privileged";
              };
            };
          })
        ];
      };

      installer = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-base.nix"
          "${nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
          disko.nixosModules.disko
          musnix.nixosModules.musnix
          hyprland.nixosModules.default
          ({ config, pkgs, lib, ... }: {
            isoImage.isoBaseName = "archibaldOS-rt-audio";
            isoImage.isoName = "${config.isoImage.isoBaseName}-${config.system.nixos.release}-${system}.iso";

            musnix.enable = true;
            musnix.kernel.realtime = true;
            musnix.kernel.packages = pkgs.linuxPackages_latest_rt;
            musnix.alsaSeq.enable = true;
            musnix.rtirq.enable = true;
            musnix.das_watchdog.enable = true;

            sound.enable = true;
            hardware.pulseaudio.enable = false;
            services.pipewire = {
              enable = true;
              alsa.enable = true;
              pulse.enable = true;
              jack.enable = true;
              extraConfig.pipewire."92-low-latency" = {
                "context.properties" = {
                  "default.clock.rate" = 96000;
                  "default.clock.quantum" = 32;
                  "default.clock.min-quantum" = 16;
                  "default.clock.max-quantum" = 64;
                };
              };
            };

            security.rtkit.enable = true;

            boot.kernelParams = [ "threadirqs" "isolcpus=1-3" "nohz_full=1-3" "intel_idle.max_cstate=1" "processor.max_cstate=1" ];
            boot.kernel.sysctl = {
              "vm.swappiness" = 0;
              "fs.inotify.max_user_watches" = 600000;
            };
            environment.etc."sysctl.d/99-audio.conf".text = ''
              dev.rtc.max-user-freq = 2048
              dev.hpet.max-user-freq = 2048
            '';
            powerManagement.cpuFreqGovernor = "performance";

            environment.etc."asound.conf".text = ''
              defaults.pcm.dmix.rate 96000
              defaults.pcm.dmix.format S32_LE
              defaults.pcm.dmix.buffer_size 32
            '';
            environment.etc."ardour6/ardour.rc".text = ''
              <JACK buffer-size="32" sample-rate="96000" periods="2"/>
            '';

            systemd.services."disable-non-essential" = {
              description = "Disable non-essential services for RT audio";
              wantedBy = [ "multi-user.target" ];
              serviceConfig = {
                Type = "oneshot";
                ExecStart = "${pkgs.systemd}/bin/systemctl stop NetworkManager bluetooth";
                RemainAfterExit = true;
              };
            };

            hardware.usb.enable = true;
            boot.kernelModules = [ "snd_usb_audio" "usbhid" "usbmidi" ];
            boot.extraModprobeConfig = ''
              options snd_usb_audio nrpacks=1 low_latency=1
            '';
            environment.systemPackages = basicPackages ++ [ pkgs.usbutils pkgs.libusb pkgs.alsa-firmware pkgs.alsa-tools ];

            programs.hyprland.enable = true;
            environment.systemPackages = basicPackages ++ [ pkgs.hyprpaper pkgs.waybar pkgs.wofi pkgs.kitty pkgs.grim pkgs.slurp pkgs.wpctl pkgs.guitarix pkgs.brightnessctl pkgs.playerctl pkgs.zenity pkgs.dialog pkgs.python3 pkgs.disko pkgs.ardour pkgs.fluidsynth pkgs.hydramesh-pkg pkgs.streamdb-pkg pkgs.qjackctl ];

            environment.etc."hypr/hyprland.conf".text = ''
              exec-once = kitty
              exec-once = waybar
              exec-once = hyprpaper
              monitor=,preferred,auto,1
              $terminal = kitty
              $menu = wofi --show drun
              general {
                  gaps_in = 5
                  gaps_out = 10
                  border_size = 2
                  col.active_border = rgba(33ff33aa) rgba(00ff00aa) 45deg
                  col.inactive_border = rgba(595959aa)
                  layout = dwindle
              }
              decoration {
                  rounding = 12
                  blur {
                      enabled = true
                      size = 8
                      passes = 2
                  }
              }
              animations {
                  enabled = true
              }
              dwindle {
                  pseudotile = true
                  preserve_split = true
              }
              input {
                  kb_layout = us
                  follow_mouse = 1
                  touchpad {
                      natural_scroll = true
                  }
              }
              gestures {
                  workspace_swipe = true
              }
              $mainMod = SUPER
              bind = $mainMod, Q, exec, $terminal
              bind = $mainMod, C, killactive,
              bind = $mainMod, M, exit,
              bind = $mainMod, D, exec, $menu
              bind = $mainMod, H, movefocus, l
              bind = $mainMod, L, movefocus, r
              bind = $mainMod, K, movefocus, u
              bind = $mainMod, J, movefocus, d
              bind = $mainMod, 1, workspace, 1
              bind = $mainMod, 2, workspace, 2
              bind = $mainMod, 3, workspace, 3
              bind = $mainMod, 4, workspace, 4
              bind = $mainMod, 5, workspace, 5
              bind = $mainMod, P, exec, hyprctl keyword decoration:blur:enabled 0
              bindel = ,XF86AudioRaiseVolume, exec, wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+
              bindel = ,XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-
              bindel = ,XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
              bind = $mainMod, PRINT, exec, grim ~/Pictures/Screenshots/screenshot-$(date +%F-%T).png
              bind = $mainMod SHIFT, PRINT, exec, grim -g "$(slurp)" ~/Pictures/Screenshots/screenshot-$(date +%F-%T).png
              bind = $mainMod SHIFT, H, exec, hydramesh-toggle
              bind = $mainMod SHIFT, K, exec, ~/.config/hypr/keybindings_cheatsheet.sh
            '';
            environment.etc."hypr/hyprpaper.conf".text = ''
              preload = ~/Pictures/wall.jpg
              wallpaper = ,~/Pictures/wall.jpg
            '';
            environment.etc."waybar/config".text = ''
              {
                  "layer": "top",
                  "position": "top",
                  "height": 32,
                  "modules-center": ["hyprland/workspaces", "cpu", "memory", "clock", "network", "custom/hydramesh"],
                  "hyprland/workspaces": {
                      "format": "{icon}",
                      "persistent-workspaces": {"*": 5}
                  },
                  "cpu": {
                      "format": "\uf2db {usage}%"
                  },
                  "memory": {
                      "format": "\uf538 {used:0.1f}GiB"
                  },
                  "clock": {
                      "format": "\uf017 {:%H:%M}"
                  },
                  "network": {
                      "format-wifi": "\uf1eb {essid}",
                      "format-ethernet": "\uf6ff {ipaddr}",
                      "format-disconnected": "\u26a0"
                  },
                  "custom/hydramesh": {
                      "format": "{}",
                      "exec": "${pkgs.hydramesh-pkg}/bin/hydramesh-status",
                      "interval": 5
                  }
              }
            '';
            environment.etc."waybar/style.css".text = ''
              * {
                  font-family: "FiraCode Nerd Font", monospace;
                  font-size: 14px;
              }
              window#waybar {
                  background: transparent;
                  color: #ffffff;
              }
              .modules-center {
                  background: rgba(10,10,10,0.85);
                  border-radius: 16px;
                  padding: 0 20px;
              }
              #workspaces button.active {
                  background: rgba(51,255,51,0.3);
                  border-radius: 8px;
              }
              #cpu, #memory, #clock, #network, #custom-hydramesh {
                  padding: 0 12px;
                  color: #33ff33;
              }
            '';
            environment.etc."wofi/style.css".text = ''
              window {
                  border: 2px solid #33ff33;
                  background-color: rgba(10,10,10,0.85);
                  border-radius: 12px;
                  font-family: "FiraCode Nerd Font", monospace;
                  font-size: 14px;
              }
              #input {
                  background-color: rgba(51,255,51,0.1);
              }
              #text {
                  color: #33ff33;
              }
              #entry:selected {
                  background-color: rgba(51,255,51,0.3);
                  border-radius: 8px;
              }
            '';
            environment.etc."qjackctl.conf".text = ''
              [Settings]
              Frames=32
              Periods=2
              SampleRate=96000
            '';
            environment.etc."keybindings_cheatsheet.sh" = {
              text = ''
                #!/usr/bin/env bash
                zenity --info --title="Hyprland Keybindings" --text="SUPER + Q: Terminal\nSUPER + D: Launcher\nSUPER + P: Toggle Blur (RT Audio)\nSUPER + 1-5: Workspaces\nXF86Audio*: Volume\nSUPER + SHIFT + H: Toggle HydraMesh\nSUPER + SHIFT + K: This Sheet"
              '';
              mode = "0755";
            };
            environment.etc."audio-setup.sh" = {
              text = ''
#!/usr/bin/env bash

set -euo pipefail  # Strict mode: exit on error, unset vars, pipe failures

# ArchibaldOS Post-Install Audio Setup Script by DeMoD LLC
# Version 1.2 - Enhanced with IRQ affinity pinning and RT priority setting
# Optimizes RT audio: checks, latency tweaks, hardware setup, kernel switching
# Run as non-root; uses sudo where needed. Based on NixOS manuals and Musnix docs.

LOG_FILE="$HOME/archibaldos-audio-setup.log"
exec > >(tee -a "$LOG_FILE") 2>&1  # Log all output

# Usage/help function
usage() {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  --help          Show this help message"
  echo "  --skip-ascii    Skip ASCII art display"
  echo "  --dry-run       Simulate actions without applying changes"
  echo "  --user <name>   Specify username (skips prompt)"
  echo "  --pci <id>      Specify PCI ID (skips prompt)"
  exit 0
}

# Parse options
DRY_RUN=false
SKIP_ASCII=false
USER=""
PCI_ID=""
while [[ $# -gt 0 ]]; do
  case $1 in
    --help) usage ;;
    --skip-ascii) SKIP_ASCII=true; shift ;;
    --dry-run) DRY_RUN=true; shift ;;
    --user) USER="$2"; shift 2 ;;
    --pci) PCI_ID="$2"; shift 2 ;;
    *) echo "Unknown option: $1"; usage ;;
  esac
done

# Display intro (optional ASCII)
echo "ArchibaldOS Audio Setup - DeMoD LLC (v1.2)"
if [ "$SKIP_ASCII" = false ]; then
  echo '                       *'
  echo '                      * *'
  echo '                     *   *'
  echo '                    * * * *'
  echo '                   *       *'
  echo '                  * *     * *'
  echo '                 *   *   *   *'
  echo '                * * * * * * * *'
  echo '               *               *'
  echo '              * *             * *'
  echo '             *   *           *   *'
  echo '            * * * *         * * * *'
  echo '           *       *       *       *'
  echo '          * *     * *     * *     * *'
  echo '         *   *   *   *   *   *   *   *'
  echo '        * * * * * * * * * * * * * * * *'
fi

# Detect live ISO
if mount | grep -q '/nix/.ro-store type squashfs'; then
  IS_LIVE=true
  echo "Live ISO detected. Some features (e.g., kernel switching) limited; install for full access."
else
  IS_LIVE=false
fi

# Check/install dependencies (transient via nix-shell if missing)
REQUIRED_PKGS=("realTimeConfigQuickScan" "qjackctl" "pciutils" "usbutils" "linuxPackages.stress-ng" "jack2")
for pkg in "${REQUIRED_PKGS[@]}"; do
  if ! command -v $(nix-locate --top-level bin/$pkg | awk '{print $1}' | head -1) &> /dev/null; then
    echo "Missing $pkg. Running in nix-shell..."
    nix-shell -p $pkg --command "$0 $@"  # Re-run script in shell with pkg
    exit $?
  fi
done
echo "All dependencies available."

# Prompt for user if not provided
if [ -z "$USER" ]; then
  read -p "Enter username: " USER
fi
if ! id "$USER" &>/dev/null; then
  echo "Error: User $USER does not exist."
  exit 1
fi

# Add user to audio/realtime groups
echo "Adding $USER to audio/realtime groups..."
if [ "$DRY_RUN" = false ]; then
  sudo usermod -aG audio,realtime "$USER" || echo "Failed to add groups; check sudo privileges."
else
  echo "[Dry-run] Would add $USER to audio/realtime."
fi
echo "Persist in configuration.nix: users.users.$USER.extraGroups = [ \"audio\" \"realtime\" ];"

# Latency tweaks (sysctl, RTC/HPET)
echo "Applying latency tweaks..."
if [ "$DRY_RUN" = false ]; then
  sudo sysctl vm.swappiness=10 || true
  sudo sysctl fs.inotify.max_user_watches=600000 || true
  echo 2048 | sudo tee /sys/class/rtc/rtc0/max_user_freq || true
  echo 2048 | sudo tee /proc/sys/dev/hpet/max-user-freq || true
else
  echo "[Dry-run] Would apply sysctl tweaks."
fi
echo "Persist in configuration.nix: boot.kernel.sysctl = { \"vm.swappiness\" = 10; \"fs.inotify.max_user_watches\" = 600000; };"

# Detect/optimize audio hardware
echo "Detecting audio devices..."
lspci | grep -i audio
lsusb | grep -i audio
AUTO_PCI_ID=$(lspci | grep -i audio | awk '{print $1}' | head -1)
if [ -n "$AUTO_PCI_ID" ] && [ -z "$PCI_ID" ]; then
  PCI_ID="$AUTO_PCI_ID"
  echo "Auto-detected audio PCI ID: $PCI_ID"
fi
if [ ! -z "$PCI_ID" ] && [[ "$PCI_ID" =~ ^[0-9a-f]{2}:[0-9a-f]{2}\.[0-9a-f]$ ]]; then
  echo "Optimizing latency timer for $PCI_ID..."
  if [ "$DRY_RUN" = false ]; then
    sudo setpci -v -s "$PCI_ID" latency_timer=ff || echo "Failed to set latency timer."
  else
    echo "[Dry-run] Would set latency timer for $PCI_ID."
  fi
else
  echo "Skipping PCI optimization (invalid or empty ID)."
fi

# IRQ Affinity Pinning
echo "Pinning audio IRQs to CPU1..."
if [ "$DRY_RUN" = false ]; then
  AUDIO_IRQ=$(cat /proc/interrupts | grep -i snd | awk '{print $1}' | sed 's/:$//' | head -1)
  [ -n "$AUDIO_IRQ" ] && echo 2 | sudo tee /proc/irq/$AUDIO_IRQ/smp_affinity || echo "No audio IRQ found."
else
  echo "[Dry-run] Would pin audio IRQ to CPU1."
fi

# RT Priorities for Audio Apps
echo "Setting RT priorities for audio apps..."
if [ "$DRY_RUN" = false ]; then
  for app in ardour qjackctl; do
    if pidof $app >/dev/null; then
      sudo chrt -f -p 80 $(pidof $app) || echo "Failed to set priority for $app."
    fi
  done
else
  echo "[Dry-run] Would set RT priorities for ardour, qjackctl."
fi

# Start qjackctl for config
echo "Starting qjackctl..."
if [ "$DRY_RUN" = false ]; then
  qjackctl & || echo "Failed to start qjackctl; ensure it's installed."
else
  echo "[Dry-run] Would start qjackctl."
fi

# Latency measurement
echo "Testing JACK latency..."
if command -v jack_iodelay &>/dev/null; then
  jack_iodelay | grep "total roundtrip latency" || echo "Start JACK server first (via qjackctl)."
else
  echo "Install jack_iodelay: nix-shell -p jack2 --run jack_iodelay"
fi
echo "Testing kernel latency..."
if command -v cyclictest &>/dev/null; then
  cyclictest -l 100000 -m -n -p99 -q | grep -E "Max|Avg" || echo "Failed; ensure RT kernel."
else
  echo "Install cyclictest: nix-shell -p linuxPackages.stress-ng --run cyclictest"
fi

# MIDI device check
if command -v amidi &>/dev/null; then
  echo "Listing MIDI devices..."
  amidi -l || echo "No MIDI devices detected."
else
  echo "Install amidi: nix-shell -p alsa-utils --run amidi"
fi

# Kernel Switching (using specialisations)
echo "Kernel Management: Default RT for low-latency; LTS backup for stability."
if [ "$IS_LIVE" = true ]; then
  echo "Live mode: Test RT with 'cyclictest -l 100000 -m -n -p99 -q'."
else
  read -p "Switch to LTS backup kernel? (y/n): " SWITCH
  if [ "$SWITCH" = "y" ]; then
    if [ "$DRY_RUN" = false ]; then
      sudo nixos-rebuild boot --specialisation lts-backup || echo "Failed; ensure specialisation in configuration.nix."
      echo "LTS set for next boot. Reboot to apply."
    else
      echo "[Dry-run] Would switch to LTS."
    fi
  else
    echo "Staying on RT. To switch: sudo nixos-rebuild boot --specialisation lts-backup"
  fi
fi

# Final verification
echo "Current kernel: $(uname -r)"
echo "Setup complete! Reboot if needed. Log saved to $LOG_FILE."
echo "Persist specialisations: specialisation.lts-backup.configuration = { boot.kernelPackages = pkgs.linuxPackages_lts; };"
              '';
              mode = "0755";
            };

            # Optional HydraMesh service: Minimal for RT audio networking
            systemd.services.hydramesh = {
              enable = false;  # Disabled by default; enable via config
              description = "HydraMesh for RT Audio P2P";
              wantedBy = [ "multi-user.target" ];
              serviceConfig = {
                ExecStart = "${pkgs.hydramesh-pkg}/bin/hydramesh --config /etc/hydramesh/config.toml";
                Restart = "always";
                User = "hydramesh";
                DynamicUser = true;
                PrivateDevices = true;
                ProtectSystem = "strict";
                ProtectHome = true;
                PrivateTmp = true;
                NoNewPrivileges = true;
                CapabilityBoundingSet = "";
                RestrictNamespaces = true;
                SystemCallFilter = "@system-service ~@privileged";
              };
            };

            # Installer script: Lean, audio-focused
            environment.etc."installer.sh" = {
              text = ''
#!/usr/bin/env bash

set -euo pipefail

echo "Welcome to Lean RT Audio ArchibaldOS Installer"
KB=$(dialog --menu "Select keyboard layout:" 20 60 12 us "US English" gb "UK English" de "German" fr "French" es "Spanish" it "Italian" ru "Russian" pl "Polish" pt "Portuguese (Portugal)" br "Portuguese (Brazil)" nl "Dutch" se "Swedish" no "Norwegian" dk "Danish" fi "Finnish" tr "Turkish" cz "Czech" hu "Hungarian" ro "Romanian" ua "Ukrainian" jp "Japanese" kr "Korean" cn "Chinese" ca "Canadian" au "Australian" latam "Latin American" in "Indian" af "Afghani" ara "Arabic" al "Albanian" am "Armenian" az "Azerbaijani" by "Belarusian" 3>&1 1>&2 2>&3)
setxkbmap -layout "$KB" || echo "Failed to set layout"
DISKS=$(lsblk -dno NAME,SIZE,TYPE | grep disk | awk '{print "/dev/"$1 " ("$2")"}')
DISK=$(dialog --menu "Select disk to wipe:" 15 50 5 $DISKS 3>&1 1>&2 2>&3)
dialog --yesno "WARNING: Erase $DISK?" 7 60 || exit
HYDRAMESH=$(dialog --yesno "Enable HydraMesh?" 7 60 && echo true || echo false)
LOCALE=$(dialog --inputbox "Locale (e.g., en_US.UTF-8):" 8 50 "en_US.UTF-8" 3>&1 1>&2 2>&3)
TZ=$(dialog --inputbox "Timezone (e.g., America/Los_Angeles):" 8 50 "America/Los_Angeles" 3>&1 1>&2 2>&3)
HOSTNAME=$(dialog --inputbox "Hostname:" 8 50 "archibaldos" 3>&1 1>&2 2>&3)
USERNAME=$(dialog --inputbox "Username:" 8 50 "audio-user" 3>&1 1>&2 2>&3)
USERPW=$(dialog --insecure --passwordbox "User password:" 8 50 3>&1 1>&2 2>&3)
USERPW_CONFIRM=$(dialog --insecure --passwordbox "Confirm:" 8 50 3>&1 1>&2 2>&3)
[ "$USERPW" = "$USERPW_CONFIRM" ] || { dialog --msgbox "Mismatch" 7 50; exit 1; }
cat <<EOF > /tmp/disko.nix
{ disks ? [ "$DISK" ], lib, ... }: {
  disko.devices = {
    disk.main = {
      type = "disk"; device = lib.head disks; format = "gpt";
      content = { type = "gpt"; partitions = {
        ESP = { size = "500M"; type = "EF00"; format = "vfat"; mountpoint = "/boot"; };
        root = { size = "100%"; format = "ext4"; mountpoint = "/"; };
      }; };
    };
  };
}
EOF
disko --mode format /tmp/disko.nix
disko --mode mount /tmp/disko.nix /mnt
nixos-generate-config --root /mnt
mkdir -p /mnt/etc/hypr /mnt/etc/waybar /mnt/etc/wofi
cp /etc/hypr/* /mnt/etc/hypr/
cp /etc/waybar/* /mnt/etc/waybar/
cp /etc/wofi/* /mnt/etc/wofi/
cd /mnt/etc/nixos
sed -i "s|time.timeZone = .*|time.timeZone = \"$TZ\";|" configuration.nix || true
sed -i "s|en_US.UTF-8|$LOCALE|" configuration.nix || true
sed -i "/services.xserver.enable = true;/a\    layout = \"$KB\";" configuration.nix || true
sed -i "s|networking.hostName = .*|networking.hostName = \"$HOSTNAME\";|" configuration.nix || true
if [ "$HYDRAMESH" = "true" ]; then
  echo "systemd.services.hydramesh.enable = true;" >> configuration.nix
fi
sed -i '/users.users.nixos = {/,/};/d' configuration.nix || true
echo "users.users.$USERNAME = { isNormalUser = true; extraGroups = [ \"audio\" \"jackaudio\" \"video\" \"wheel\" ]; initialHashedPassword = \"$(mkpasswd -m sha-512 "$USERPW")\"; };" >> configuration.nix
mkdir -p /mnt/home/"$USERNAME"/.config/hypr /mnt/home/"$USERNAME"/.config/waybar /mnt/home/"$USERNAME"/.config/wofi
cp /etc/hypr/* /mnt/home/"$USERNAME"/.config/hypr/
cp /etc/waybar/* /mnt/home/"$USERNAME"/.config/waybar/
cp /etc/wofi/* /mnt/home/"$USERNAME"/.config/wofi/
chown -R 1000:100 /mnt/home/"$USERNAME"
nixos-install --root /mnt --flake /mnt/etc/nixos#archibaldOS
dialog --msgbox "Installation complete. Reboot." 7 50
              '';
              mode = "0755";
            };

            # Activation for live: Minimal setup
            system.activationScripts.setupHyprland = lib.stringAfter [ "users" ] ''
              mkdir -p /home/nixos/.config/hypr /home/nixos/.config/waybar /home/nixos/.config/wofi
              cp /etc/hypr/* /home/nixos/.config/hypr/
              cp /etc/waybar/* /home/nixos/.config/waybar/
              cp /etc/wofi/* /home/nixos/.config/wofi/
              chown -R nixos:users /home/nixos
              chmod +x /home/nixos/.config/hypr/*.sh
            '';
            users.users.nixos = {
              isNormalUser = true;
              extraGroups = [ "wheel" "audio" "jackaudio" "video" ];
              initialPassword = "nixos";
              shell = pkgs.bash;
            };
            security.sudo.wheelNeedsPassword = false;

            hardware.opengl.enable = true;
            isoImage.squashfsCompression = "gzip -Xcompression-level 1";
            nix.settings.experimental-features = [ "nix-command" "flakes" ];
          })
        ];
      };
    };

    packages.${system}.installer = self.nixosConfigurations.installer.config.system.build.isoImage;

    devShells.${system}.default = pkgs.mkShell {
      packages = audioPackages ++ basicPackages;
    };
  };
}
