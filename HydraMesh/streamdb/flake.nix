{
  description = "HydraMesh: Lisp-based P2P audio networking for ArchibaldOS";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    streamdb.url = "path:./streamdb";  # StreamDB flake
  };

  outputs = { self, nixpkgs, streamdb }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    sbclWithPkgs = pkgs.sbcl.withPackages (ps: with ps; [
      cffi cl-ppcre cl-json cl-csv usocket bordeaux-threads log4cl trivial-backtrace cl-store hunchensocket fiveam cl-dot cserial-port
      cl-lorawan cl-lsquic cl-can cl-sctp cl-zigbee
    ]);
  in {
    packages.${system} = {
      hydramesh = pkgs.writeShellScriptBin "hydramesh" ''
        ${sbclWithPkgs}/bin/sbcl --load /root/quicklisp/setup.lisp \
          --load ${./src/hydramesh.lisp} \
          --eval '(dolist (plugin (directory "/etc/hydramesh/plugins/*.lisp")) (load plugin))' \
          --eval '(in-package :hydramesh)' \
          --eval '(hydramesh-init "/etc/hydramesh/config.json" :restore-state t)' \
          --eval '(hydramesh-start)' \
          --non-interactive
      '';
      streamdb = streamdb.packages.${system}.default;
      toggleScript = pkgs.writeShellScriptBin "hydramesh-toggle" ''
        #!/usr/bin/env bash
        if systemctl is-active --quiet hydramesh; then
          systemctl stop hydramesh
          hyprctl notify -1 4000 "rgb(ff3333)" "HydraMesh" "Service stopped"
          echo "OFF" > /var/lib/hydramesh/hydramesh-status
        else
          systemctl start hydramesh
          hyprctl notify -1 4000 "rgb(33ff33)" "HydraMesh" "Service started"
          echo "ON" > /var/lib/hydramesh/hydramesh-status
        fi
      '';
      statusScript = pkgs.writeShellScriptBin "hydramesh-status" ''
        #!/usr/bin/env bash
        STATUS=$(systemctl is-active hydramesh)
        if [ "$STATUS" = "active" ]; then
          echo "{\"text\": \"🕸️ ON\", \"class\": \"hydramesh-active\", \"tooltip\": \"HydraMesh running\", \"icon\": \"/etc/hydramesh/hydramesh.svg\"}"
        else
          echo "{\"text\": \"🕸️ OFF\", \"class\": \"hydramesh-inactive\", \"tooltip\": \"HydraMesh stopped\", \"icon\": \"/etc/hydramesh/hydramesh.svg\"}"
        fi
      '';
    };
  };
}
