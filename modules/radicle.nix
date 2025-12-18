# Copyright (c) 2025, DeMoD LLC
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.archibald.radicle;
in {
  options.services.archibald.radicle = {
    enable = mkEnableOption "Radicle P2P code collaboration with programming tools";

    package = mkOption {
      type = types.package;
      default = pkgs.radicle-node;
      description = "Radicle node package to use";
    };

    devTools = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Install lightweight programming/development tools";
      };
    };

    seedNode = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Configure as a public Radicle seed node (node + httpd + firewall)";
      };

      alias = mkOption {
        type = types.str;
        default = "archibald-seed";
        description = "Node alias (human-readable name)";
      };

      externalAddress = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "/ip4/203.0.113.42/tcp/8776";
        description = "External address for NAT/public discoverability";
      };

      domain = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "radicle.example.com";
        description = "Domain for nginx reverse proxy (enables HTTPS)";
      };

      pinnedRepositories = mkOption {
        type = types.listOf types.str;
        default = [];
        example = [ "rad:z3gqcJUoA1n9HaHKufZs5FCSGazv5" ];
        description = "Repositories to pin on the web interface";
      };
    };
  };

  config = mkIf cfg.enable {
    # Core Radicle service
    services.radicle = {
      enable = true;  # Always enable node for local use
      package = cfg.package;
      httpd.enable = cfg.seedNode.enable;

      settings = mkIf cfg.seedNode.enable {
        node.alias = cfg.seedNode.alias;
        node.externalAddresses = mkIf (cfg.seedNode.externalAddress != null) [ cfg.seedNode.externalAddress ];
        node.openFirewall = cfg.seedNode.enable;

        web.pinned.repositories = cfg.seedNode.pinnedRepositories;
      };

      # Optional nginx proxy for public HTTPS
      httpd.nginx = mkIf (cfg.seedNode.enable && cfg.seedNode.domain != null) {
        enable = true;
        serverName = cfg.seedNode.domain;
        enableACME = true;
        forceSSL = true;
      };
    };

    # Manual firewall fallback
    networking.firewall.allowedTCPPorts = mkIf cfg.seedNode.enable [ 8776 ];

    # Packages
    environment.systemPackages = with pkgs; [
      cfg.package  # radicle-node + CLI (rad)
    ] ++ optionals cfg.seedNode.enable [
      radicle-httpd
    ] ++ optionals cfg.devTools.enable [
      git git-lfs git-annex gh gitui lazygit
      rustup cargo clang gcc gnumake cmake
      nodejs python3 go zig
      helix neovim
      vscodium  # Lightweight FOSS alternative
    ];

    # Aliases for convenience
    programs.bash.shellAliases = {
      rad = "rad";
    };
  };
}
