# Copyright ©️ DeMoD LLC - see LICENSE 
#
#modules/agentic-local-ai.nix
# Declarative NixOS module for a complete local agentic AI stack on ArchibaldOS:
# - Ollama with optional CUDA acceleration
# - Preloaded text + vision models (with presets for different hardware tiers)
# - llama.cpp as high-performance alternative backend
# - Open WebUI as powerful agentic frontend (tools, pipelines, RAG, multi-model, image/audio upload)
# - Local STT (via Open WebUI's built-in Local Whisper / faster-whisper)
# - Local TTS:
#   - Piper (default: fast, low-latency, multiple voices)
#   - Optional Coqui XTTS-v2 (community-maintained high-quality multilingual + voice cloning)
# - Optional AutoGen + CrewAI for advanced multi-agent workflows (Ollama-native)
# - High-quality, low-latency PipeWire (96 kHz / 128-sample quantum default)

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.agentic-local-ai;
in
{
  options.services.agentic-local-ai = {
    enable = mkEnableOption "Agentic local AI profile with vision, voice, multi-agent, and agentic tools";

    preset = mkOption {
      type = types.enum [ "default" "high-vram" "pewdiepie" ];
      default = "default";
      description = ''
        Hardware-tier presets:
        - default: Balanced for typical single-GPU or CPU setups
        - high-vram: Larger models for systems with 48+ GB VRAM
        - pewdiepie: Extreme multi-GPU rigs (256+ GB total VRAM)
      '';
    };

    acceleration = mkOption {
      type = types.nullOr (types.enum [ "cuda" "rocm" ]);
      default = null;
      description = "Hardware acceleration (forced to cuda in pewdiepie preset)";
    };

    models = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Custom model list – overrides preset defaults if non-empty";
    };

    enableWebUI = mkEnableOption "Open WebUI frontend (strongly recommended for agentic use)" // { default = true; };

    webuiPort = mkOption { type = types.port; default = 8080; };
    ollamaPort = mkOption { type = types.port; default = 11434; };

    multiAgent = {
      enable = mkEnableOption "AutoGen and CrewAI for advanced multi-agent orchestration (native Ollama support)";
    };

    voice = {
      enable = mkEnableOption "Local voice support (STT via Local Whisper + multiple TTS engines)";

      mode = mkOption {
        type = types.enum [ "fast" "balanced" "high-quality" ];
        default = if cfg.preset == "pewdiepie" then "high-quality"
                  else if cfg.acceleration == "cuda" then "balanced"
                  else "fast";
        description = ''
          Voice optimization mode (applies to Piper; XTTS is always high-quality when enabled):
          - fast: Low-latency, CPU-friendly
          - balanced: Good quality and speed
          - high-quality: Maximum fidelity
        '';
      };

      recommendedWhisperModel = mkOption {
        type = types.str;
        readOnly = true;
        default = if cfg.voice.mode == "fast" then "base"
                  else if cfg.voice.mode == "balanced" then "small"
                  else "large-v3-turbo";
      };

      piperVoices = mkOption {
        type = types.listOf types.str;
        default = [
          "en_US-lessac-low"         # Fast mode
          "en_US-lessac-medium"      # Balanced
          "en_US-amy-high"           # High-quality English
          "en_GB-alan-medium"        # British English
          "en_US-hfc_male-medium"    # Additional natural male
          "de_DE-thorsten-high"      # Example multilingual (German)
        ];
        description = "Recommended Piper voices for different use cases (use with `piper --model <voice>`)";
      };

      xtts = {
        enable = mkEnableOption "Coqui XTTS-v2 (community-maintained) for SOTA multilingual TTS + voice cloning" // {
          description = "High-quality alternative/complement to Piper; best with CUDA";
        };
      };
    };
  };

  config = mkIf cfg.enable {
    services.ollama = {
      enable = true;
      port = cfg.ollamaPort;
      acceleration = if cfg.preset == "pewdiepie" then "cuda" else cfg.acceleration;
      host = "127.0.0.1";

      environment = mkIf (cfg.preset != "default") {
        OLLAMA_SCHED_SPREAD = "1";
        OLLAMA_MAX_LOADED_MODELS = "12";
        OLLAMA_NUM_PARALLEL = "8";
      };

      loadModels = if cfg.models != [] then cfg.models else
        if cfg.preset == "pewdiepie" then [
          "deepseek-r1:671b-q3_K_M"
          "qwen3-vl:235b-instruct-q4_K_M"
          "llama3.1:405b-instruct-q4_K_M"
          "gpt-oss:120b-q4_0"
        ] else if cfg.preset == "high-vram" then [
          "qwen3:72b-instruct-q6_K"
          "llama3.2:90b-vision-instruct-q5_K_M"
          "deepseek-r1:70b-q5_K_M"
          "qwen2.5vl:72b-instruct-q4_K_M"
        ] else [
          "llama3.2:latest"
          "llama3.2:11b-vision-instruct-q5_K_M"
          "qwen2.5:7b"
        ];
    };

    environment.systemPackages = with pkgs; [
      ollama
      open-webui
      (llama-cpp.override {
        cudaSupport = (cfg.acceleration == "cuda" || cfg.preset == "pewdiepie");
      })
      # Voice core
      (optional cfg.voice.enable piper-tts)
      (optional cfg.voice.enable whisper-cpp)
      # XTTS-v2 (community Coqui)
      (optional (cfg.voice.enable && cfg.voice.xtts.enable) python312Packages.tts)
      # Multi-agent frameworks
      (optional cfg.multiAgent.enable
        (python312.withPackages (ps: with ps; [
          pyautogen
          crewai
          crewai-tools
          langchain
          litellm  # For easy Ollama integration
        ])))
    ];

    services.pipewire = mkIf cfg.voice.enable {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;

      extraConfig.pipewire."91-high-quality-low-latency" = {
        "context.properties" = {
          "default.clock.rate" = 96000;
          "default.clock.quantum" = 128;
          "default.clock.min-quantum" = 128;
          "default.clock.max-quantum" = 128;
        };
      };
    };

    systemd.services.open-webui = mkIf cfg.enableWebUI {
      description = "Open WebUI – agentic frontend for Ollama";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" "ollama.service" ];
      requires = [ "ollama.service" ];
      environment = {
        OLLAMA_API_BASE = "http://127.0.0.1:${toString cfg.ollamaPort}";
        PORT = toString cfg.webuiPort;
        ENABLE_AUDIO = "true";
      };
      serviceConfig = {
        ExecStart = "${pkgs.open-webui}/bin/open-webui serve";
        DynamicUser = true;
        StateDirectory = "open-webui";
        WorkingDirectory = "/var/lib/open-webui";
        Restart = "on-failure";
        RestartSec = 5;
      };
    };

    networking.firewall.allowedTCPPorts = mkIf cfg.enableWebUI [ cfg.webuiPort ];
  };
}
