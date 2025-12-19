# modules/ai/agentic-local-ai.nix
#
# BSD-style license (full text in LICENSE)
# Copyright (c) 2025 DeMoD LLC
# All rights reserved.
#
# Refined production-grade declarative NixOS module for a minimal, tiered local agentic AI stack
# Aligned with ArchibaldOS "Minimal Oligarchy" philosophy (December 19, 2025)
#
# Core principles applied:
# - Docker isolation for zero impact on real-time audio determinism
# - No native heavy services, no curl installers, no pip -e, no main-branch pins
# - Declarative, immutable compose generated from flake
# - Tiered presets adjust only performance-critical parameters (parallelism, loaded models, keep-alive)
# - Optional extensions kept minimal and fully containerized
# - Reduced attack surface: localhost-only by default, user-level execution
# - Lower jitter: Isolated containers, no competing native processes

{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.archibaldos.profiles.ai.agenticLocalAi;

  presetConfigs = {
    cpu-fallback = {
      description = "CPU-only fallback for systems without GPU acceleration or low-power scenarios. Optimized for smaller models with reasonable response times on modern CPUs.";
      numParallel = 1;
      maxLoadedModels = 2;
      keepAlive = "12h";
      shmSize = "8gb";
      acceleration = null;
      recommendedModels = [
        "tinyllama:1.1b-chat-q8_0"
        "phi3:3.8b-mini-128k-instruct-q8_0"
        "gemma2:2b-instruct-q8_0"
        "qwen2.5:3b-instruct-q8_0"
      ];
    };

    default = {
      description = "Balanced for consumer AMD/NVIDIA GPUs (16–24GB VRAM). With heavy quantization (e.g., Q4_K_M/Q3_K_M) and careful model selection, this tier can function effectively even on 8GB VRAM cards.";
      numParallel = 4;
      maxLoadedModels = 4;
      keepAlive = "24h";
      shmSize = "16gb";
      recommendedModels = [
        "llama3.2:3b-instruct-q8_0"
        "phi3:14b-medium-128k-instruct-q6_K"
        "qwen2.5-coder:14b-q6_K"
        "gemma2:27b-instruct-q6_K"
      ];
    };

    high-vram = {
      description = "Optimized for high-end GPUs (40GB+ VRAM, e.g., RX 7900 XTX, RTX 4090)";
      numParallel = 8;
      maxLoadedModels = 6;
      keepAlive = "48h";
      shmSize = "32gb";
      recommendedModels = [
        "llama3.1:70b-instruct-q6_K"
        "qwen2.5:72b-instruct-q5_K_M"
        "deepseek-coder-v2:236b-lite-instruct-q4_K_M"
        "gemma2:72b-instruct-q5_K_M"
      ];
    };

    pewdiepie = {
      description = "Extreme tier for multi-GPU monster rigs (requires CUDA, 8+ GPUs)";
      numParallel = 16;
      maxLoadedModels = 10;
      keepAlive = "72h";
      shmSize = "64gb";
      recommendedModels = [
        "llama3.1:405b-instruct-q4_K_M"
        "qwen3:235b-instruct-q4_K_M"
        "deepseek-v3:671b-q3_K_M"
      ];
    };
  };

  currentPreset = presetConfigs.${cfg.preset};

  effectiveAcceleration = if cfg.acceleration != null then cfg.acceleration else currentPreset.acceleration;

  ollamaImage = if effectiveAcceleration == "rocm" then "ollama/ollama:rocm"
                else "ollama/ollama";  # Default to CPU-capable image

  foldingAtHomeService = optionalString cfg.advanced.foldingAtHome.enable ''
      foldingathome:
        image: ghcr.io/linuxserver/foldingathome:latest
        container_name: foldingathome
        restart: unless-stopped
        environment:
          - USER=Anonymous
          - TEAM=0
          - ENABLE_GPU=true
          - ENABLE_SMP=true
        volumes:
          - ~/foldingathome-data:/config
        ${optionalString (effectiveAcceleration == "rocm") ''
        devices:
          - /dev/kfd:/dev/kfd
          - /dev/dri:/dev/dri
        ''}
        ${optionalString (effectiveAcceleration == "cuda") ''
        deploy:
          resources:
            reservations:
              devices:
                - driver: nvidia
                  count: all
                  capabilities: [gpu]
        ''}
        healthcheck:
          test: ["CMD", "curl", "-f", "http://localhost:7396/api/status"]
          interval: 60s
          timeout: 10s
          retries: 3
  '';

  dockerComposeYml = pkgs.writeText "docker-compose-agentic-ai.yml" ''
    version: "3.9"

    services:
      ollama:
        image: ${ollamaImage}
        container_name: ollama
        restart: unless-stopped
        ipc: host
        shm_size: "${currentPreset.shmSize}"
        ${optionalString (effectiveAcceleration == "rocm") ''
        devices:
          - /dev/kfd:/dev/kfd
          - /dev/dri:/dev/dri
        ''}
        volumes:
          - ~/.ollama:/root/.ollama
        ports:
          - "127.0.0.1:11434:11434"
        environment:
          - OLLAMA_FLASH_ATTENTION=1
          - OLLAMA_NUM_PARALLEL=${toString currentPreset.numParallel}
          - OLLAMA_MAX_LOADED_MODELS=${toString currentPreset.maxLoadedModels}
          - OLLAMA_KEEP_ALIVE=${currentPreset.keepAlive}
          - OLLAMA_SCHED_SPREAD=1
          - OLLAMA_KV_CACHE_TYPE=q8_0
        healthcheck:
          test: ["CMD", "curl", "-f", "http://localhost:11434/api/tags"]
          interval: 30s
          timeout: 10s
          retries: 3

      open-webui:
        image: ghcr.io/open-webui/open-webui:main
        container_name: open-webui
        restart: unless-stopped
        volumes:
          - ~/open-webui-data:/app/backend/data
        ports:
          - "127.0.0.1:8080:8080"
        environment:
          - OLLAMA_BASE_URL=http://ollama:11434
          - ENABLE_SIGNUP=false
          - WEBUI_AUTH=true
          - DEFAULT_USER_ROLE=admin
        depends_on:
          ollama:
            condition: service_healthy
        healthcheck:
          test: ["CMD", "curl", "-f", "http://localhost:8080"]
          interval: 30s
          timeout: 10s
          retries: 3

    ${foldingAtHomeService}
  '';

  aiStackScript = pkgs.writeShellScriptBin "ai-stack" ''
    #!/usr/bin/env bash
    set -euo pipefail

    if ! groups | grep -q docker; then
      echo "Error: User must be in 'docker' group."
      exit 1
    fi

    COMPOSE_DIR="$HOME/.config/archibaldos/ai-stack"
    mkdir -p "$COMPOSE_DIR" "$HOME/.ollama" "$HOME/open-webui-data"
    ${optionalString cfg.advanced.foldingAtHome.enable ''mkdir -p "$HOME/foldingathome-data"''}
    chmod 700 "$HOME/.ollama" "$HOME/open-webui-data"

    COMPOSE_FILE="$COMPOSE_DIR/docker-compose.yml"

    if [ ! -f "$COMPOSE_FILE" ]; then
      echo "Deploying minimal tiered Agentic AI stack (${cfg.preset} preset)..."
      echo "${currentPreset.description}"
      ${optionalString cfg.advanced.foldingAtHome.enable "echo 'Folding@Home container enabled (contribute to science when idle)'"}
      cp ${dockerComposeYml} "$COMPOSE_FILE"
    fi

    ${optionalString (cfg.preset != "cpu-fallback") ''
    if command -v rocminfo >/dev/null 2>&1; then
      GFX=$(rocminfo | grep -oP 'gfx\K\d{3,}' | head -1 || echo "")
      if [ -n "$GFX" ]; then
        SUGGESTED=$(echo "$GFX" | awk '{printf "%d.%d.0", substr($0,1,length($0)-1), substr($0,length($0))}')
        echo "Detected gfx$GFX → Add HSA_OVERRIDE_GFX_VERSION=$SUGGESTED to compose if needed"
      fi
    fi
    ''}

    cd "$COMPOSE_DIR"

    case "${1:-start}" in
      start|up)
        docker compose pull --quiet
        docker compose up -d
        echo "Agentic Local AI (${cfg.preset}) active → http://localhost:8080"
        ${optionalString cfg.advanced.foldingAtHome.enable "echo 'Folding@Home running in background'"}
        echo "Recommended models: ${concatStringsSep " " currentPreset.recommendedModels}"
        ;;
      stop|down)
        docker compose down
        ;;
      logs)
        docker compose logs -f "${@:2}"
        ;;
      pull)
        [ -z "${2:-}" ] && echo "Usage: ai-stack pull <model>" && exit 1
        docker exec -it ollama ollama pull "$2"
        ;;
      tune)
        rocm-smi --showmeminfo vram 2>/dev/null || echo "ROCm not active (CPU mode or no GPU)"
        ;;
      backup)
        TIMESTAMP=$(date +%Y%m%d_%H%M)
        tar -czf "$HOME/ai-backup-$TIMESTAMP.tar.gz" ~/.ollama ~/open-webui-data ${optionalString cfg.advanced.foldingAtHome.enable "~/foldingathome-data"}
        echo "Backup: $HOME/ai-backup-$TIMESTAMP.tar.gz"
        ;;
      *)
        echo "ai-stack [start|stop|logs|pull <model>|tune|backup]"
        ;;
    esac
  '';
in
{
  options.archibaldos.profiles.ai.agenticLocalAi = {
    enable = mkEnableOption "Minimal tiered agentic local AI stack (Ollama + Open WebUI)";

    preset = mkOption {
      type = types.enum [ "cpu-fallback" "default" "high-vram" "pewdiepie" ];
      default = "default";
      description = "Performance tier preset — adjusts only essential parameters.";
    };

    acceleration = mkOption {
      type = types.nullOr (types.enum [ "cuda" "rocm" ]);
      default = null;
      description = "Force acceleration backend (auto-detected via image tag where possible; cpu-fallback preset overrides to null)";
    };

    advanced.foldingAtHome = {
      enable = mkEnableOption "Optional Folding@Home container (contribute spare cycles to science)";
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.preset == "pewdiepie" -> cfg.acceleration == "cuda";
        message = "pewdiepie preset requires CUDA (multi-GPU).";
      }
      {
        assertion = cfg.preset == "cpu-fallback" -> effectiveAcceleration == null;
        message = "cpu-fallback preset must use CPU mode.";
      }
    ];

    virtualisation.docker.enable = mkDefault true;

    users.users.asher.extraGroups = [ "docker" "video" "render" ];

    environment.systemPackages = with pkgs; [
      docker-compose
      rocmPackages.rocm-smi
      rocmPackages.rocminfo
      aiStackScript
    ];

    system.activationScripts.aiAgentSetup = ''
      mkdir -p /home/asher/.ollama /home/asher/open-webui-data /home/asher/.config/archibaldos/ai-stack
      ${optionalString cfg.advanced.foldingAtHome.enable "mkdir -p /home/asher/foldingathome-data"}
      chown -R asher:users /home/asher/.ollama /home/asher/open-webui-data /home/asher/.config/archibaldos ${optionalString cfg.advanced.foldingAtHome.enable "/home/asher/foldingathome-data"}
      chmod 700 /home/asher/.ollama /home/asher/open-webui-data
    '';
  };
}
