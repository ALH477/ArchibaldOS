# host-flake/flake.nix

# Copyright 2025 DeMoD LLC

# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



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
