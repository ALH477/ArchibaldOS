{ config, pkgs, ... }: let
  basicPackages = with pkgs; [
    pcmanfm vim brave polybar feh waybar kitty wofi wireplumber grim slurp hyprpaper cava playerctl
    (nerdfonts.override { fonts = [ "Gohu" "Noto" "RobotoMono" ]; })  # For Unicode bars and Waybar fonts
  ];
  wallpaperSrc = ../wallpaper.jpg;

  customDwm = pkgs.writeShellScriptBin "dwm" ''
    ${pkgs.polybar}/bin/polybar --config=/etc/polybar/config.ini &
    ${pkgs.feh}/bin/feh --bg-fill /etc/hypr/wallpaper.jpg &
    exec ${pkgs.dwm}/bin/dwm
  '';
in {
  services.xserver.enable = true;
  services.xserver.windowManager.dwm.enable = true;
  services.xserver.windowManager.dwm.package = customDwm;
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.displayManager.sddm.settings = {
    General.Background = "/etc/hypr/wallpaper.jpg";
  };
  environment.etc."hypr/wallpaper.jpg".source = wallpaperSrc;

  programs.hyprland.enable = true;

  environment.etc."hypr/hyprland.conf".text = ''
    exec-once = kitty
    exec-once = waybar -c /etc/waybar/config -s /etc/waybar/style.css
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
    preload = /etc/hypr/wallpaper.jpg
    wallpaper = ,/etc/hypr/wallpaper.jpg
  '';

  environment.etc."polybar/config.ini".text = ''
    [bar/main]
    monitor =
    width = 100%
    height = 30
    radius = 0
    fixed-center = true

    background = #000000
    foreground = #ffffff

    line-size = 2
    line-color = #f00

    border-size = 0
    border-color = #00000000

    padding-left = 0
    padding-right = 2

    module-margin-left = 1
    module-margin-right = 2

    font-0 = fixed:pixelsize=10;1

    modules-left = cava
    modules-center = date
    modules-right = cpu memory

    tray-position = right
    tray-padding = 2

    wm-restack = generic

    [module/cava]
    type = custom/script
    tail = true
    exec = /etc/polybar/cava.sh
    format = <label>
    label = %output%

    [module/date]
    type = internal/date
    interval = 5
    date = %Y-%m-%d%
    time = %H:%M:%S
    label = %date% %time%

    [module/cpu]
    type = internal/cpu
    interval = 2
    format-prefix = "CPU "
    label = %percentage:2%%

    [module/memory]
    type = internal/memory
    interval = 2
    format-prefix = "RAM "
    label = %percentage_used%%
  '';

  environment.etc."polybar/cava.sh" = {
    text = ''
      #! /bin/bash

      bar="▁▂▃▄▅▆▇█"
      dict="s/;//g;"

      # creating "dictionary" to replace char with bar
      i=0
      while [ $i -lt ${#bar} ]
      do
          dict="${dict}s/$i/${bar:$i:1}/g;"
          i=$((i=i+1))
      done

      # write cava config
      config_file="/tmp/polybar_cava_config"
      echo "
      [general]
      bars = 10

      [output]
      method = raw
      raw_target = /dev/stdout
      data_format = ascii
      ascii_max_range = 7
      " > $config_file

      # read stdout from cava
      cava -p $config_file | while read -r line; do
          echo $line | sed $dict
      done
    '';
    mode = "0755";
  };

  environment.etc."waybar/config".text = ''
    [
        {
            // "layer": "top", // Waybar at top layer
            // "position": "bottom", // Waybar position (top|bottom|left|right)
            "height": 30, // Waybar height (to be removed for auto height)
            // "width": 1280, // Waybar width
            "spacing": 4, // Gaps between modules (4px)
            // Choose the order of the modules
            // "modules-left": ["idle_inhibitor", "hyprland/workspaces", "hyprland/submap", "hyprland/scratchpad", "custom/media"],
            "modules-left": ["idle_inhibitor", "hyprland/workspaces", "hyprland/submap", "hyprland/scratchpad"],
            // "modules-center": ["hyprland/window"],
            // "modules-center": ["custom/playerinfo","custom/cava"],
            "modules-center": ["custom/playerinfo"],
            // "modules-center": [],
            // "modules-right": ["mpd", "idle_inhibitor", "pulseaudio", "network", "cpu", "memory", "temperature", "backlight", "keyboard-state", "hyprland/language", "battery", "battery#bat2", "clock", "tray"],
            "modules-right": ["pulseaudio", "network", "cpu", "memory", "temperature", "backlight", "keyboard-state", "battery", "battery#bat2", "clock", "tray"],
            // "margin-top": 10,
            "margin-left": 20,
            "margin-right": 20,
            // Modules configuration
            // "hyprland/workspaces": {
            //     "disable-scroll": true,
            //     "all-outputs": true,
            //     "warp-on-scroll": false,
            //     "format": "{name}: {icon}",
            //     "format-icons": {
            //         "1": "",
            //         "2": "",
            //         "3": "",
            //         "4": "",
            //         "5": "",
            //         "urgent": "",
            //         "focused": "",
            //         "default": ""
            //     }
            // },
            "keyboard-state": {
                "numlock": true,
                "capslock": true,
                "format": "{name} {icon}",
                "format-icons": {
                    "locked": "",
                    "unlocked": ""
                }
            },
            "hyprland/submap": {
                "format": "<span style=\"italic\">{}</span>"
            },
            "hyprland/scratchpad": {
                "format": "{icon} {count}",
                "show-empty": false,
                "format-icons": ["", ""],
                "tooltip": true,
                "tooltip-format": "{app}: {title}"
            },
            "idle_inhibitor": {
                "format": "{icon}",
                "format-icons": {
                    "activated": "󰋘",
                    "deactivated": "󰋙"
                }
            },
            "tray": {
                // "icon-size": 21,
                "spacing": 10
            },
            "clock": {
                // "timezone": "America/New_York",
                "tooltip-format": "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>",
                "format-alt": "{:%Y-%m-%d}"
            },
            "cpu": {
                // "format": " {usage}%",
                "format": "󰒆 {usage}%",
                // "format": "󱇙 {usage}%",
                // "format": "󰋱 {usage}%",
                // "format": "  {usage}%",
                // "format": "󰍛 {usage}%",
                "tooltip": true
            },
            "memory": {
                // "format": " {}%"
                // "format": " {}%"
                // "format": " {}%"
                // "format": "  {}%"
                // "format": "󱊖 {}%"
                // "format": "󰉹  {}%"
                // "format": "󰒋  {}%"
                "format": "󰇖 {}%"
            },
            "temperature": {
                // "thermal-zone": 2,
                // "hwmon-path": "/sys/class/hwmon/hwmon2/temp1_input",
                "critical-threshold": 80,
                // "format-critical": "{temperatureC}°C {icon}",
                "format": "{icon} {temperatureC}󰔄",
                "format-icons": ["󰸄", "󱃃", "󰸁"]
            },
            "backlight": {
                // "device": "acpi_video1",
                "format": "{icon} {percent}%",
                "format-icons": ["", "", "", "", "", "", "", "", ""]
            },
            "battery": {
                "states": {
                    // "good": 95,
                    "warning": 30,
                    "critical": 15
                },
                "format": "{icon}  {capacity}%",
                "format-charging": "{icon}  {capacity}%",
                "format-plugged": "{icon}  {capacity}%",
                "format-alt": "{icon}  {time}",
                // "format-good": "", // An empty format will hide the module
                // "format-full": "",
                "format-icons": ["", "", "", "", ""]
            },
            "battery#bat2": {
                "bat": "BAT2"
            },
            "network": {
                // "interface": "wlp2*", // (Optional) To force the use of this interface
                "format-wifi": "  {essid} ({signalStrength}%)",
                "format-ethernet": "󰌗 {ipaddr}/{cidr}",
                "tooltip-format": "{ifname} via {gwaddr}",
                "format-linked": "{ifname} (No IP)",
                "format-disconnected": "󰏝",
                // "format-disconnected": "⚠ Disconnected",
                // "format-disconnected": "󰤮",
                // "format-disconnected": "󰤭",
                "format-alt": "{ifname}: {ipaddr}/{cidr}"
            },
            "pulseaudio": {
                // "scroll-step": 1, // %, can be a float
                // "format": "{volume}% {icon} {format_source}",
                "format": "{icon} {volume}%",
                // "format-bluetooth": "{volume}% {icon}󰂯 {format_source}",
                "format-bluetooth": "{volume}% {icon}",
                "format-bluetooth-muted": "󰸈 {icon}",
                "format-muted": "󰸈",
                "format-source": "{volume}% ",
                "format-source-muted": "",
                "format-icons": {
                    "headphone": "󰋋",
                    "hands-free": "󰋌",
                    "headset": "󰋎",
                    "phone": "",
                    "portable": "",
                    "car": "",
                    "default": ["", "", ""]
                },
                "on-click": "pavucontrol"
            },
            "custom/playerinfo": {
                "format": "&lt; {} &gt;",
                "exec": "/etc/waybar/playerinfo.sh",
                "escape": true,
                "return-type": "json",
                "interval": 1,
                "tooltip": true,
                "on-click-middle": "playerctl play-pause",
                "on-click-right": "playerctl next",
                "on-click-left": "playerctl previous"
            }
        },
        {
            "name": "overlay",
            "layer": "bottom", // Waybar at top layer
            "position": "top", // Waybar position (top|bottom|left|right)
            "height": 30, // Waybar height (to be removed for auto height)
            "modules-center": ["custom/cava"],
            "margin-top": -30,
            "exclusive": false,
            "passtrough": true,
            "width": 300,
            "custom/cava": {
                "format": "{}",
                "exec": "/etc/waybar/cava.sh"
            }
        }
    ]
  '';

  environment.etc."waybar/style.css".text = ''
    * {
        font-family: "GohuFont uni11 Nerd Font", "Noto Sans CJK KR", "Noto Sans CJK JP", Roboto, Helvetica, Arial, sans-serif;
        font-size: 13px;
    }

    window#waybar.overlay {
        background-color: rgba(0, 0, 0, 0);
        color: #ffffff;
    }

    window#waybar {
        background-color: rgba(43, 48, 59, 0.7);
        color: #ffffff;
        transition-property: background-color;
        transition-duration: .5s;
    }

    window#waybar.hidden {
        opacity: 0.2;
    }

    /*
    window#waybar.empty {
        background-color: transparent;
    }
    window#waybar.solo {
        background-color: #FFFFFF;
    }
    */

    button {
        /* Use box-shadow instead of border so the text isn't offset */
        box-shadow: inset 0 -3px transparent;
        /* Avoid rounded borders under each button name */
        border: none;
        border-radius: 0;
    }

    /* https://github.com/Alexays/Waybar/wiki/FAQ#the-workspace-buttons-have-a-strange-hover-effect */
    button:hover {
        background: inherit;
    }

    #workspaces button {
        padding: 0 10px;
        background-color: transparent;
        color: #ffffff;
    }

    #workspaces button:hover {
        background: rgba(0, 0, 0, 0.5);
    }

    #workspaces button.focused {
        background-color: #34424D;
        box-shadow: inset 0 -3px #2980b9;
    }

    #workspaces button.urgent {
        box-shadow: inset 0 -3px #eb4d4b;
    }

    #mode {
        background-color: #64727D;
        border-bottom: 3px solid #ffffff;
    }

    #clock,
    #battery,
    #cpu,
    #memory,
    #disk,
    #temperature,
    #backlight,
    #network,
    #pulseaudio,
    #wireplumber,
    #custom-media,
    #tray,
    #mode,
    #idle_inhibitor,
    #scratchpad,
    #custom-playerinfo,
    #mpd {
        padding: 0 10px;
        color: #ffffff;
    }

    #window,
    #workspaces {
        margin: 0 4px;
    }

    /* If workspaces is the leftmost module, omit left margin */
    .modules-left > widget:first-child > #workspaces {
        margin-left: 0;
    }

    /* If workspaces is the rightmost module, omit right margin */
    .modules-right > widget:last-child > #workspaces {
        margin-right: 0;
    }

    #clock {
    }

    #battery {
        box-shadow: inset 0 -3px #ffffff;
    }

    #battery.charging, #battery.plugged {
        box-shadow: inset 0 -3px #2980b9;
    }

    #custom-playerinfo {
        box-shadow: inset 0 -3px #ffffff;
        min-width: 300px;
    }

    #custom-cava {
        font-size: 24px;
        color: rgba(255, 255, 255, 0.7);
    }

    @keyframes blink {
        to {
            box-shadow: inset 0 -3px #ffffff;
        }
    }

    #battery.critical:not(.charging) {
        box-shadow: inset 0 -3px #eb4d4b;
        animation-name: blink;
        animation-duration: 0.5s;
        animation-timing-function: linear;
        animation-iteration-count: infinite;
        animation-direction: alternate;
    }

    label:focus {
        background-color: #000000;
    }

    #cpu {}
    #memory {}
    #disk {}
    #backlight {}

    #network {
        box-shadow: inset 0 -3px #2980b9;
    }

    #network.disconnected {
        box-shadow: inset 0 -3px #e0e0e0;
    }

    #pulseaudio {
    }

    #pulseaudio.muted {
        background-color: #90b1b1;
        color: #2a5c45;
    }

    #temperature {}
    #temperature.critical {
        box-shadow: inset 0 -3px #eb4d4b;
    }

    #tray {
        background-color: #2980b9;
    }

    #tray > .passive {
        -gtk-icon-effect: dim;
    }

    #tray > .needs-attention {
        -gtk-icon-effect: highlight;
        background-color: #eb4d4b;
    }

    #idle_inhibitor {}
    #idle_inhibitor.activated {
        background-color: #ecf0f1;
        color: #2d3436;
    }

    #keyboard-state {
        background: #97e1ad;
        color: #000000;
        padding: 0 0px;
        margin: 0 5px;
        min-width: 16px;
    }

    #keyboard-state > label {
        padding: 0 5px;
    }

    #keyboard-state > label.locked {
        background: rgba(0, 0, 0, 0.2);
    }

    #scratchpad {
        background: rgba(0, 0, 0, 0.2);
    }

    #scratchpad.empty {
    	background-color: transparent;
    }
  '';

  environment.etc."waybar/cava.sh" = {
    text = ''
      #! /bin/bash

      bar="▁▂▃▄▅▆▇█"
      dict="s/;//g;"

      # creating "dictionary" to replace char with bar
      i=0
      while [ $i -lt ${#bar} ]
      do
          dict="${dict}s/$i/${bar:$i:1}/g;"
          i=$((i=i+1))
      done

      # write cava config
      config_file="/tmp/waybar_cava_config"  # Renamed for clarity
      echo "
      [general]
      bars = 10  # Matched to Polybar for consistency

      [output]
      method = raw
      raw_target = /dev/stdout
      data_format = ascii
      ascii_max_range = 7
      " > $config_file

      # read stdout from cava
      cava -p $config_file | while read -r line; do
          echo $line | sed $dict
      done
    '';
    mode = "0755";
  };

  environment.etc."waybar/playerinfo.sh" = {
    text = ''
      #! /bin/bash

      text=$(playerctl metadata --format '{{artist}} - {{title}}')
      maxlength=35
      # if the text is longer than the max length, truncate it and add "..."
      if [ ${#text} -gt $maxlength ]; then
          text=${text:0:$maxlength-3}"..."
      fi

      playerctl metadata --format '{"text": "'"$text"'", "tooltip": "{{playerName}} : {{artist}} - {{title}}"}'
    '';
    mode = "0755";
  };

  environment.systemPackages = basicPackages;
}
