{ config, pkgs, lib, ... }: 

{
  options.branding = {
    enable = lib.mkEnableOption "DeMoD LLC branding across installer, boot, and desktop";
    
    asciiArt = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable ASCII art in TUI installer";
    };
    
    splash = lib.mkOption {
      type = lib.types.bool;
      default = false;  # Disabled by default for ARM compatibility
      description = "Plymouth boot splash with DeMoD logo (x86 only)";
    };
    
    wallpapers = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable system-wide wallpaper placement for multiple DEs/WMs";
    };
    
    wallpaperPaths = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [ ./assets/demod-wallpaper.jpg ];
      description = "List of wallpaper files to deploy";
    };
  };

  config = lib.mkIf config.branding.enable {
    
    # === ASCII Art for Installer ===
    environment.etc."installer-ascii.txt" = lib.mkIf config.branding.asciiArt {
      text = ''
                  _     _ _           _     _  ____   _____  
   /\            | |   (_) |         | |   | |/ __ \ / ____| 
  /  \   _ __ ___| |__  _| |__   __ _| | __| | |  | | (___   
 / /\ \ | '__/ __| '_ \| | '_ \ / _` | |/ _` | |  | |\___ \  
/ ____ \| | | (__| | | | | |_) | (_| | | (_| | |__| |____) | 
/_/    \_\_|  \___|_| |_|_|_.__/ \__,_|_|\__,_|\____/|_____/  
                                                             
DeMoD LLC - ArchibaldOS
Real-Time Audio Workstation
      '';
      mode = "0644";
    };

    # === Plymouth Splash (x86 only) ===
    boot.plymouth = lib.mkIf (config.branding.splash && pkgs.stdenv.hostPlatform.isx86) {
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
        
        ${lib.optionalString (builtins.pathExists ./assets/demod-logo.png) ''
          cp ${./assets/demod-logo.png} $out/share/plymouth/themes/demod/logo.png
        ''}
      '') ];
      theme = "demod";
    };

    # === Wallpapers for Multiple DEs/WMs ===
    environment.systemPackages = lib.mkIf config.branding.wallpapers [
      (pkgs.runCommand "archibaldos-wallpapers" {} (let
        wallpaperDir = "$out/share/wallpapers/ArchibaldOS";
      in ''
        mkdir -p ${wallpaperDir}
        ${lib.concatStringsSep "\n" (map (wp: 
          "cp ${wp} ${wallpaperDir}/$(basename ${wp})"
        ) config.branding.wallpaperPaths)}
      ''))
    ];

    # DE-specific wallpaper integration
    environment.etc = lib.mkIf config.branding.wallpapers (lib.mkMerge [
      # Plasma 6
      (lib.mkIf config.services.desktopManager.plasma6.enable {
        "plasma-workspace/env/set_wallpaper.sh" = {
          text = ''
            #!/bin/sh
            mkdir -p $HOME/.local/share/wallpapers
            ln -sf /usr/share/wallpapers/ArchibaldOS/* $HOME/.local/share/wallpapers/
          '';
          mode = "0755";
        };
      })
      
      # GNOME
      (lib.mkIf (config.services.xserver.desktopManager.gnome.enable or false) {
        "gnome/backgrounds/archibaldos" = {
          source = "/usr/share/wallpapers/ArchibaldOS";
        };
      })
      
      # XFCE
      (lib.mkIf (config.services.xserver.desktopManager.xfce.enable or false) {
        "xfce4/backdrops/archibaldos" = {
          source = "/usr/share/wallpapers/ArchibaldOS";
        };
      })
    ]);

    # Fallback: Copy wallpapers to ~/Pictures/Wallpapers for all users
    system.activationScripts.copyWallpapers = lib.mkIf config.branding.wallpapers {
      text = ''
        for user_home in /home/*; do
          if [ -d "$user_home" ]; then
            mkdir -p "$user_home/Pictures/Wallpapers"
            cp -r /usr/share/wallpapers/ArchibaldOS/* "$user_home/Pictures/Wallpapers/" 2>/dev/null || true
            chown -R $(basename "$user_home"):users "$user_home/Pictures/Wallpapers" 2>/dev/null || true
          fi
        done
      '';
      deps = [ "users" ];
    };
  };
}
