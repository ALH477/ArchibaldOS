# modules/ArchibaldVM/flake.nix
{
  description = "ArchibaldOS VM for RT Audio Development";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    musnix.url = "github:musnix/musnix";
    streamdb.url = "path:../../hydramesh/streamdb";
    streamdb.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, musnix, streamdb }: {
    nixosModules.default = { config, pkgs, lib, ... }: {
      imports = [ musnix.nixosModules.musnix ./modules/audio.nix ./modules/users.nix ];

      boot.kernelParams = [ "kvm-intel.nested=1" "kvm-amd.nested=1" ];

      archibaldOS.rtKernel.enable = true;

      services.pipewire.enable = true;
      services.pipewire.jack.enable = true;

      environment.systemPackages = with pkgs; [
        gcc python3 git cmake emacs neovim
        supercollider jack2 helvum
        realtimeconfigquickscan
        (streamdb.packages.${pkgs.system}.default or streamdb.packages.${pkgs.system}.streamdb)
      ];

      networking.firewall.allowedTCPPorts = [ 22 4713 8080 ] ++ (lib.range 8000 9000);

      services.openssh.enable = true;
      services.openssh.settings.PasswordAuthentication = false;

      security.apparmor.enable = true;

      services.fail2ban.enable = true;

      networking.firewall.interfaces.virbr0.allowedTCPPorts = [ 4713 ];

      systemd.services.libvirtd.serviceConfig.CPUSchedulingPolicy = "rr";
      systemd.services.libvirtd.serviceConfig.CPUSchedulingPriority = 99;

      # StreamDB service
      systemd.services.streamdb = {
        description = "StreamDB – Zero-config asset sharing";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          ExecStart = "${streamdb.packages.${pkgs.system}.default or streamdb.packages.${pkgs.system}.streamdb}/bin/streamdb";
          Restart = "always";
          User = "audio-user";
          WorkingDirectory = "/var/lib/streamdb";
        };
      };

      systemd.tmpfiles.rules = [
        "d /var/lib/streamdb 0750 audio-user audio-user -"
      ];

      system.stateVersion = "24.11";
    };

    nixosConfigurations.archibaldOS-vm = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ self.nixosModules.default ];
    };
  };
}
