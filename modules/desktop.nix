{ config, pkgs, ... }: let
  basicPackages = with pkgs; [
    pcmanfm vim brave
  ];
  wallpaperSrc = ../wallpaper.jpg;
in {
  services.xserver.enable = true;
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.displayManager.sddm.settings = {
    General.Background = "/etc/hypr/wallpaper.jpg";
  };
  environment.etc."hypr/wallpaper.jpg".source = wallpaperSrc;

  programs.hyprland.enable = true;

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

  environment.systemPackages = basicPackages;
}
