{
  description = "ArchibaldOS by DeMoD LLC: NixOS for Real-Time Audio Production";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    musnix.url = "github:musnix/musnix";  # For integrated RT kernel and audio opts
    hyprland.url = "github:hyprwm/Hyprland";  # Optional
  };

  outputs = { self, nixpkgs, musnix, hyprland, ... }: {
    nixosConfigurations.archibald = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        musnix.nixosModules.musnix  # RT kernel integration
      ];
    };

    # ISO configuration for live booting with RT audio
    nixosConfigurations.archibald-iso = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"  # Minimal for lean live boot
        # Or use installation-cd-graphical-plasma.nix for KDE live

        ./configuration.nix
        musnix.nixosModules.musnix

        {
          # ISO overrides for live hardware testing
          users.users.youruser = {
            isNormalUser = true;
            extraGroups = [ "wheel" "audio" "realtime" "networkmanager" ];
            shell = pkgs.zsh;
            initialPassword = "archibald";  # Change post-boot
          };
          networking.firewall.enable = false;  # For live; re-enable on install
          services.openssh.enable = true;  # Remote access if needed
          boot.kernelParams = [ "preempt=full" "threadirqs" "copytoram" ];
          isoImage = {
            isoName = "archibaldos-${config.system.nixos.label}.iso";
            makeEfiBootable = true;
            makeUsbBootable = true;
          };
          # Include post-install script in live environment
          environment.systemPackages = config.environment.systemPackages ++ [ (pkgs.writeShellScriptBin "post-install-audio-setup.sh" (builtins.readFile ./post-install-audio-setup.sh)) ];
        }
      ];
    };
  };
}
