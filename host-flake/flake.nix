# host-flake/flake.nix
{
  description = "NixOS Host for ArchibaldOS VM with Gaming Support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    archibaldos-vm.url = "path:./modules/ArchibaldVM";
    streamdb.url = "path:./hydramesh/streamdb";
    streamdb.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, archibaldos-vm, streamdb }: {
    nixosConfigurations.host = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ({ config, pkgs, lib, ... }: {
          boot.kernelParams = [ "iommu=pt" "amd_iommu=on" "intel_iommu=on" "vfio-pci.ids=10de:1f08,10de:10f9,10de:1ada,10de:1adb" ];

          services.openssh.enable = true;

          virtualisation.libvirtd.enable = true;
          virtualisation.libvirtd.qemu = {
            swtpm.enable = true;
            ovmf.enable = true;
            runAsRoot = false;
          };

          networking.firewall.allowedTCPPorts = [ 22 4713 8080 ] ++ (lib.range 8000 9000);

          environment.systemPackages = with pkgs; [
            steam godot4 unityhub
            virt-manager qemu ovmf
            streamdb.packages.${pkgs.system}.default or streamdb.packages.${pkgs.system}.streamdb
          ];

          hardware.opengl.enable = true;
          hardware.opengl.driSupport = true;

          nix.settings.experimental-features = [ "nix-command" "flakes" ];

          system.stateVersion = "24.11";

          # StreamDB service (auto-start)
          systemd.services.streamdb = {
            description = "StreamDB – Zero-config asset sharing for DeMoD";
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
        })
        archibaldos-vm.nixosModules.default
      ];
    };
  };
}
