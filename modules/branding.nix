{ config, pkgs, lib, ... }: {
  options.branding = {
    enable = lib.mkEnableOption "DeMoD LLC branding across installer, boot, and desktop";
    asciiArt = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable ASCII art in TUI installer (adds ~1.5s delay, optional)";
    };
    splash = lib.mkEnableOption "Plymouth boot splash with DeMoD logo";
    wallpaper = lib.mkEnableOption "DeMoD-branded Hyprland wallpaper";
    waybarIcons = lib.mkEnableOption "HydraMesh status icons with DeMoD styling";
  };

  config = lib.mkIf config.branding.enable {
    # === 1. ASCII Art ===
    environment.etc."installer-ascii.txt" = lib.mkIf config.branding.asciiArt {
      text = ''
                    _     _ _           _     _  ____   _____  
     /\            | |   (_) |         | |   | |/ __ \ / ____| 
    /  \   _ __ ___| |__  _| |__   __ _| | __| | |  | | (___   
   / /\ \ | '__/ __| '_ \| | '_ \ / _` | |/ _` | |  | |\___ \  
  / ____ \| | | (__| | | | | |_) | (_| | | (_| | |__| |____) | 
 /_/    \_\_|  \___|_| |_|_|_.__/ \__,_|_|\__,_|\____/|_____/  
                                                               
                                                               
DeMoD LLC
      '';
      mode = "0644";
    };

    # === 2. Plymouth Splash ===
    boot.plymouth = lib.mkIf config.branding.splash {
      enable = true;
      themePackages = [ (pkgs.runCommand "demod-plymouth" {} ''
        mkdir -p $out/share/plymouth/themes/demod
        cat > $out/share/plymouth/themes/demod/demod.plymouth <<'EOF'
[Plymouth Theme]
Name=DeMoD LLC
Description=ArchibaldOS branded boot splash
ModuleName=script

[script]
ImageDir=/run/current-system/sw/share/plymouth/themes/demod
ScriptFile=/run/current-system/sw/share/plymouth/themes/demod/demod.script
EOF
        cat > $out/share/plymouth/themes/demod/demod.script <<'EOF'
screen_width = Window.GetWidth()
screen_height = Window.GetHeight()
center_x = screen_width / 2
center_y = screen_height / 2

logo = Image.New("logo.png")
logo = logo.Scale(screen_width * 0.4, screen_height * 0.3)
sprite = Sprite.New(logo)
sprite.SetPosition(center_x - logo.GetWidth()/2, center_y - logo.GetHeight()/2, 0)

progress_bar = ProgressBar.New()
progress_bar.SetPosition(center_x - 200, center_y + 150, 0)
progress_bar.SetSize(400, 20)

fun ProgressBarCallback(progress) {
  progress_bar.SetPercent(progress * 100)
}
Window.SetProgressBarCallback(ProgressBarCallback)
EOF
        mkdir -p $out/share/plymouth/themes/demod
        cp ${./assets/demod-logo.png} $out/share/plymouth/themes/demod/logo.png
      '') ];
      theme = "demod";
    };

    # === 3. Wallpaper ===
    environment.etc."hypr/wallpaper.jpg" = lib.mkIf config.branding.wallpaper {
      source = ./assets/demod-wallpaper.jpg;
    };

    # === 4. Waybar Icons ===
    environment.etc."waybar/hydramesh-on.svg" = lib.mkIf config.branding.waybarIcons {
      text = builtins.readFile ./assets/hydramesh-on.svg;
    };
    environment.etc."waybar/hydramesh-off.svg" = lib.mkIf config.branding.waybarIcons {
      text = builtins.readFile ./assets/hydramesh-off.svg;
    };
  };
}
