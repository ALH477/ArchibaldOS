{
  description = "ArchibaldOS: NixOS with CachyOS RT-BORE Kernel, KDE, and Audio Tools";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    chaotic = {
      url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Optional: For Hyprland extras if needed
    hyprland.url = "github:hyprwm/Hyprland";
  };

  outputs = { self, nixpkgs, chaotic, hyprland, ... }: {
    nixosConfigurations.archibald = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        # Chaotic-Nyx overlay for CachyOS kernels
        { imports = [ chaotic.nixosModules.default ]; }
      ];
    };
  };
}
