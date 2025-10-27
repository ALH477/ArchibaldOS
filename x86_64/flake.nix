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
      csound faust portaudio rtaudio supercollider qjackctl  # Added qjackctl for JACK GUI management

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

            # Optimized PipeWire for low-latency audio
            sound.enable = true;
            hardware.pulseaudio.enable = false;
            services.pipewire = {
              enable = true;
              alsa.enable = true;
              pulse.enable = true;
              jack.enable = true;
              extraConfig.pipewire."92-low-latency" = {
                "context.properties" = {
                  "default.clock.rate" = 48000;
                  "default.clock.quantum" = 64;
                  "default.clock.min-quantum" = 32;
                  "default.clock.max-quantum" = 128;
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
            boot.kernelParams = [ "threadirqs" "isolcpus=1-3" "nohz_full=1-3" ];  # Isolate CPUs for audio (adjust for hardware)
            boot.kernel.sysctl = {
              "vm.swappiness" = 10;
              "fs.inotify.max_user_watches" = 600000;
            };
            environment.etc."sysctl.d/99-audio.conf".text = ''
              dev.rtc.max-user-freq = 2048
              dev.hpet.max-user-freq = 2048
            '';
            powerManagement.cpuFreqGovernor = "performance";  # Consistent RT performance

            # USB libraries/drivers for basic connections (e.g., USB audio/MIDI interfaces)
            hardware.usb.enable = true;
            boot.kernelModules = [ "snd_usb_audio" "usbhid" "usbmidi" ];
            boot.extraModprobeConfig = ''
              options snd_usb_audio nrpacks=1
            '';
            environment.systemPackages = audioPackages ++ basicPackages ++ [ pkgs.usbutils pkgs.libusb ];  # Tools for USB detection/management

            # Lean packages: Audio + basic software + minimal UX
            environment.systemPackages = audioPackages ++ basicPackages ++ [ pkgs.hyprpaper pkgs.waybar pkgs.wofi pkgs.kitty pkgs.grim pkgs.slurp pkgs.wpctl pkgs.brightnessctl pkgs.playerctl pkgs.zenity pkgs.dialog pkgs.python3 pkgs.hydramesh-pkg pkgs.streamdb-pkg ];

            services.xserver.enable = true;
            services.displayManager.sddm.enable = true;
            services.displayManager.sddm.wayland.enable = true;
            services.displayManager.sddm.settings = {
              General.Background = "/etc/hypr/wallpaper.jpg";
            };
            environment.etc."hypr/wallpaper.jpg".source = wallpaperSrc;

            programs.hyprland.enable = true;

            # Minimal Hyprland config: Core UX only, adapted for RT audio
            environment.etc."hypr/hyprland.conf".text = ''
              exec-once = kitty
              exec-once = waybar
              exec-once = hyprpaper
              monitor=,preferred,auto,1
              $terminal = kitty
              $menu = wofi --show drun
              general { gaps_in = 5; gaps_out = 10; border_size = 2; col.active_border = rgba(33ff33aa) rgba(00ff00aa) 45deg; col.inactive_border = rgba(595959aa); layout = dwindle; }
              decoration { rounding = 12; blur { enabled = true; size = 8; passes = 2; } }
              animations { enabled = true; }
              dwindle { pseudotile = true; preserve_split = true; }
              input { kb_layout = us; follow_mouse = 1; touchpad { natural_scroll = true; } }
              gestures { workspace_swipe = true; }
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
                  "layer": "top", "position": "top", "height": 32,
                  "modules-center": ["hyprland/workspaces", "cpu", "memory", "clock", "network", "custom/hydramesh"],
                  "hyprland/workspaces": { "format": "{icon}", "persistent-workspaces": {"*": 5} },
                  "cpu": { "format": "\uf2db {usage}%" },
                  "memory": { "format": "\uf538 {used:0.1f}GiB" },
                  "clock": { "format": "\uf017 {:%H:%M}" },
                  "network": { "format-wifi": "\uf1eb {essid}", "format-ethernet": "\uf6ff {ipaddr}", "format-disconnected": "\u26a0" },
                  "custom/hydramesh": { "format": "{}", "exec": "${pkgs.hydramesh-pkg}/bin/hydramesh-status", "interval": 5 }
              }
            '';
            environment.etc."waybar/style.css".text = ''
              * { font-family: "FiraCode Nerd Font", monospace; font-size: 14px; }
              window#waybar { background: transparent; color: #ffffff; }
              .modules-center { background: rgba(10,10,10,0.85); border-radius: 16px; padding: 0 20px; }
              #workspaces button.active { background: rgba(51,255,51,0.3); border-radius: 8px; }
              #cpu, #memory, #clock, #network, #custom-hydramesh { padding: 0 12px; color: #33ff33; }
            '';
            environment.etc."wofi/style.css".text = ''
              window { border: 2px solid #33ff33; background-color: rgba(10,10,10,0.85); border-radius: 12px; font-family: "FiraCode Nerd Font", monospace; font-size: 14px; }
              #input { background-color: rgba(51,255,51,0.1); }
              #text { color: #33ff33; }
              #entry:selected { background-color: rgba(51,255,51,0.3); border-radius: 8px; }
            '';
            environment.etc."hypr/keybindings_cheatsheet.sh" = {
              text = ''
                #!/usr/bin/env bash
                zenity --info --title="Hyprland Keybindings" --text="SUPER + Q: Terminal\nSUPER + D: Launcher\nSUPER + 1-5: Workspaces\nXF86Audio*: Volume\nSUPER + SHIFT + H: Toggle HydraMesh\nSUPER + SHIFT + K: This Sheet"
              '';
              mode = "0755";
            };

            # Embed improved post-install audio setup script
            environment.etc."audio-setup.sh" = {
              text = ''
#!/usr/bin/env bash

set -euo pipefail  # Strict mode: exit on error, unset vars, pipe failures

# ArchibaldOS Post-Install Audio Setup Script by DeMoD LLC
# Version 1.1 - Improved with error handling, dependency checks, validation, and persistence.
# For RT checks, latency tweaks, hardware setup, and kernel switching.
# Run as non-root; uses sudo where needed. Based on NixOS manuals and Musnix docs.

LOG_FILE="$HOME/archibaldos-audio-setup.log"
exec > >(tee -a "$LOG_FILE") 2>&1  # Log all output

# Usage/help function
usage() {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  --help          Show this help message"
  echo "  --skip-ascii    Skip ASCII art display"
  echo "  --dry-run      Simulate actions without applying changes"
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
echo "ArchibaldOS Audio Setup - DeMoD LLC (v1.1)"
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
REQUIRED_PKGS=("realTimeConfigQuickScan" "qjackctl" "pciutils" "usbutils" "linuxPackages.stress-ng")  # Add stress-ng for cyclictest alternative
for pkg in "${REQUIRED_PKGS[@]}"; do
  if ! command -v $(nix-locate --top-level bin/$pkg | awk '{print $1}') &> /dev/null; then
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
echo "Suggest adding to configuration.nix for persistence: users.users.$USER.extraGroups = [ \"audio\" \"realtime\" ];"

# Latency tweaks (sysctl; suggest Nix persistence)
echo "Applying latency tweaks..."
if [ "$DRY_RUN" = false ]; then
  sudo sysctl vm.swappiness=10 || true
  sudo sysctl fs.inotify.max_user_watches=600000 || true
  echo 2048 | sudo tee /sys/class/rtc/rtc0/max_user_freq || true
  echo 2048 | sudo tee /proc/sys/dev/hpet/max-user-freq || true
else
  echo "[Dry-run] Would apply sysctl tweaks."
fi
echo "For persistence, add to configuration.nix: boot.kernel.sysctl = { \"vm.swappiness\" = 10; \"fs.inotify.max_user_watches\" = 600000; };"

# Detect/optimize audio hardware
echo "Detecting audio devices..."
lspci | grep -i audio
lsusb | grep -i audio
if [ -z "$PCI_ID" ]; then
  read -p "Enter sound card PCI ID (e.g., 00:1f.3, leave blank to skip): " PCI_ID
fi
if [ ! -z "$PCI_ID" ] && [[ "$PCI_ID" =~ ^[0-9a-f]{2}:[0-9a-f]{2}\.[0-9a-f]$ ]]; then  # Basic validation
  echo "Optimizing latency timer for $PCI_ID..."
  if [ "$DRY_RUN" = false ]; then
    sudo setpci -v -s "$PCI_ID" latency_timer=ff || echo "Failed to set latency timer."
  else
    echo "[Dry-run] Would set latency timer for $PCI_ID."
  fi
else
  echo "Skipping PCI optimization (invalid or empty ID)."
fi

# Start qjackctl for config
echo "Starting qjackctl..."
if [ "$DRY_RUN" = false ]; then
  qjackctl & || echo "Failed to start qjackctl; ensure it's installed."
else
  echo "[Dry-run] Would start qjackctl."
fi

# Latency measurement
echo "For latency test: Ensure JACK is running, then run 'jack_iodelay' or 'stress-ng --cyclic 1 --cyclic-policy rtt' for benchmarks."

# Kernel Switching (using specialisations)
echo "Kernel Management: Default RT for low-latency; LTS backup for stability."
if [ "$IS_LIVE" = true ]; then
  echo "Live mode: Test RT with 'cyclictest -l 100000 -m -n -p99 -q' (install via nix-shell -p linuxPackages.stress-ng)."
else
  read -p "Switch to LTS backup kernel? (y/n): " SWITCH
  if [ "$SWITCH" = "y" ]; then
    if [ "$DRY_RUN" = false ]; then
      sudo nixos-rebuild boot --specialisation lts-backup || echo "Failed; ensure specialisation is defined in configuration.nix."
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
echo "Setup complete! Reboot if needed. Log saved to $LOG_FILE. For issues, check NixOS audio wiki or /var/log/pipewire.log."
echo "Tip: Add to configuration.nix for specialisations: specialisation.lts-backup.configuration = { boot.kernelPackages = pkgs.linuxPackages_lts; };"
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
                  "default.clock.rate" = 48000;
                  "default.clock.quantum" = 64;
                  "default.clock.min-quantum" = 32;
                  "default.clock.max-quantum" = 128;
                };
              };
            };

            security.rtkit.enable = true;

            boot.kernelParams = [ "threadirqs" "isolcpus=1-3" "nohz_full=1-3" ];
            boot.kernel.sysctl = {
              "vm.swappiness" = 10;
              "fs.inotify.max_user_watches" = 600000;
            };
            environment.etc."sysctl.d/99-audio.conf".text = ''
              dev.rtc.max-user-freq = 2048
              dev.hpet.max-user-freq = 2048
            '';
            powerManagement.cpuFreqGovernor = "performance";

            hardware.usb.enable = true;
            boot.kernelModules = [ "snd_usb_audio" "usbhid" "usbmidi" ];
            boot.extraModprobeConfig = ''
              options snd_usb_audio nrpacks=1
            '';
            environment.systemPackages = basicPackages ++ [ pkgs.usbutils pkgs.libusb ];

            programs.hyprland.enable = true;
            environment.systemPackages = basicPackages ++ [ pkgs.hyprpaper pkgs.waybar pkgs.wofi pkgs.kitty pkgs.grim pkgs.slurp pkgs.wpctl pkgs.brightnessctl pkgs.playerctl pkgs.zenity pkgs.dialog pkgs.python3 pkgs.disko pkgs.ardour pkgs.fluidsynth pkgs.hydramesh-pkg pkgs.streamdb-pkg ];

            # Embed improved post-install audio setup script in live ISO
            environment.etc."audio-setup.sh" = {
              text = ''
#!/usr/bin/env bash

set -euo pipefail  # Strict mode: exit on error, unset vars, pipe failures

# ArchibaldOS Post-Install Audio Setup Script by DeMoD LLC
# Version 1.1 - Improved with error handling, dependency checks, validation, and persistence.
# For RT checks, latency tweaks, hardware setup, and kernel switching.
# Run as non-root; uses sudo where needed. Based on NixOS manuals and Musnix docs.

LOG_FILE="$HOME/archibaldos-audio-setup.log"
exec > >(tee -a "$LOG_FILE") 2>&1  # Log all output

# Usage/help function
usage() {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  --help          Show this help message"
  echo "  --skip-ascii    Skip ASCII art display"
  echo "  --dry-run      Simulate actions without applying changes"
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
echo "ArchibaldOS Audio Setup - DeMoD LLC (v1.1)"
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
REQUIRED_PKGS=("realTimeConfigQuickScan" "qjackctl" "pciutils" "usbutils" "linuxPackages.stress-ng")  # Add stress-ng for cyclictest alternative
for pkg in "${REQUIRED_PKGS[@]}"; do
  if ! command -v $(nix-locate --top-level bin/$pkg | awk '{print $1}') &> /dev/null; then
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
echo "Suggest adding to configuration.nix for persistence: users.users.$USER.extraGroups = [ \"audio\" \"realtime\" ];"

# Latency tweaks (sysctl; suggest Nix persistence)
echo "Applying latency tweaks..."
if [ "$DRY_RUN" = false ]; then
  sudo sysctl vm.swappiness=10 || true
  sudo sysctl fs.inotify.max_user_watches=600000 || true
  echo 2048 | sudo tee /sys/class/rtc/rtc0/max_user_freq || true
  echo 2048 | sudo tee /proc/sys/dev/hpet/max-user-freq || true
else
  echo "[Dry-run] Would apply sysctl tweaks."
fi
echo "For persistence, add to configuration.nix: boot.kernel.sysctl = { \"vm.swappiness\" = 10; \"fs.inotify.max_user_watches\" = 600000; };"

# Detect/optimize audio hardware
echo "Detecting audio devices..."
lspci | grep -i audio
lsusb | grep -i audio
if [ -z "$PCI_ID" ]; then
  read -p "Enter sound card PCI ID (e.g., 00:1f.3, leave blank to skip): " PCI_ID
fi
if [ ! -z "$PCI_ID" ] && [[ "$PCI_ID" =~ ^[0-9a-f]{2}:[0-9a-f]{2}\.[0-9a-f]$ ]]; then  # Basic validation
  echo "Optimizing latency timer for $PCI_ID..."
  if [ "$DRY_RUN" = false ]; then
    sudo setpci -v -s "$PCI_ID" latency_timer=ff || echo "Failed to set latency timer."
  else
    echo "[Dry-run] Would set latency timer for $PCI_ID."
  fi
else
  echo "Skipping PCI optimization (invalid or empty ID)."
fi

# Start qjackctl for config
echo "Starting qjackctl..."
if [ "$DRY_RUN" = false ]; then
  qjackctl & || echo "Failed to start qjackctl; ensure it's installed."
else
  echo "[Dry-run] Would start qjackctl."
fi

# Latency measurement
echo "For latency test: Ensure JACK is running, then run 'jack_iodelay' or 'stress-ng --cyclic 1 --cyclic-policy rtt' for benchmarks."

# Kernel Switching (using specialisations)
echo "Kernel Management: Default RT for low-latency; LTS backup for stability."
if [ "$IS_LIVE" = true ]; then
  echo "Live mode: Test RT with 'cyclictest -l 100000 -m -n -p99 -q' (install via nix-shell -p linuxPackages.stress-ng)."
else
  read -p "Switch to LTS backup kernel? (y/n): " SWITCH
  if [ "$SWITCH" = "y" ]; then
    if [ "$DRY_RUN" = false ]; then
      sudo nixos-rebuild boot --specialisation lts-backup || echo "Failed; ensure specialisation is defined in configuration.nix."
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
echo "Setup complete! Reboot if needed. Log saved to $LOG_FILE. For issues, check NixOS audio wiki or /var/log/pipewire.log."
echo "Tip: Add to configuration.nix for specialisations: specialisation.lts-backup.configuration = { boot.kernelPackages = pkgs.linuxPackages_lts; };"
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
