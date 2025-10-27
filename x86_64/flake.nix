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
      csound faust portaudio rtaudio supercollider

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
            # Lean Musnix RT setup: Essentials for low-latency audio
            musnix.enable = true;
            musnix.kernel.realtime = true;
            musnix.kernel.packages = pkgs.linuxPackages_latest_rt;
            musnix.alsaSeq.enable = true;
            musnix.rtirq.enable = true;

            # PipeWire for RT audio
            sound.enable = true;
            hardware.pulseaudio.enable = false;
            services.pipewire = {
              enable = true;
              alsa.enable = true;
              pulse.enable = true;
              jack.enable = true;
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

            boot.kernelParams = [ "threadirqs" ];  # RT IRQ threading

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

            services.xserver.enable = true;
            services.displayManager.sddm.enable = true;
            services.displayManager.sddm.wayland.enable = true;
            services.displayManager.autoLogin.enable = true;
            services.displayManager.autoLogin.user = "nixos";
            services.displayManager.defaultSession = "hyprland";

            services.displayManager.sddm.settings = {
              General.Background = "/etc/hypr/wallpaper.jpg";
            };
            environment.etc."hypr/wallpaper.jpg".source = wallpaperSrc;

            programs.hyprland.enable = true;
            environment.systemPackages = basicPackages ++ [ pkgs.hyprpaper pkgs.waybar pkgs.wofi pkgs.kitty pkgs.grim pkgs.slurp pkgs.wpctl pkgs.brightnessctl pkgs.playerctl pkgs.zenity pkgs.dialog pkgs.python3 pkgs.disko pkgs.ardour pkgs.fluidsynth pkgs.hydramesh-pkg pkgs.streamdb-pkg ];

            # Minimal live configs: Copy essentials
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

            # Lean installer script: Audio-focused, no extras
            environment.etc."installer.sh" = {
              text = ''
                #!/usr/bin/env bash
                set -e
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
