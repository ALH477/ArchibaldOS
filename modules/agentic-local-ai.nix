
# modules/agentic-local-ai.nix
# Declarative NixOS module for a complete local agentic AI stack on ArchibaldOS:
# - Ollama with optional CUDA acceleration
# - Preloaded text + vision models (with presets for different hardware tiers)
# - llama.cpp as high-performance alternative backend
# - Open WebUI as powerful agentic frontend (tools, pipelines, RAG, multi-model, image upload)
# - Local STT (via Open WebUI's built-in Local Whisper / faster-whisper)
# - Local TTS (Piper with hardware-optimized voice selection)
# - High-quality, low-latency PipeWire configuration (96 kHz / 128-sample quantum default)
#   → 24-bit depth is automatically negotiated where hardware supports it (standard for pro audio)

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.agentic-local-ai;
in
{
  options.services.agentic-local-ai = {
    enable = mkEnableOption "Agentic local AI profile with vision, voice, and agentic tools";

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

    voice = {
      enable = mkEnableOption "Local voice support (STT via Local Whisper + Piper TTS)";

      mode = mkOption {
        type = types.enum [ "fast" "balanced" "high-quality" ];
        default = if cfg.preset == "pewdiepie" then "high-quality"
                  else if cfg.acceleration == "cuda" then "balanced"
                  else "fast";
        description = ''
          Voice optimization mode:
          - fast: Low-latency, CPU-friendly (ideal for real-time on ARM or light x86)
          - balanced: Good quality and speed
          - high-quality: Maximum accuracy and voice fidelity (best on powerful GPUs)
        '';
      };

      recommendedWhisperModel = mkOption {
        type = types.str;
        readOnly = true;
        default = if cfg.voice.mode == "fast" then "base"
                  else if cfg.voice.mode == "balanced" then "small"
                  else "large-v3-turbo";
      };

      recommendedPiperVoice = mkOption {
        type = types.str;
        readOnly = true;
        default = if cfg.voice.mode == "fast" then "en_US-lessac-low"
                  else if cfg.voice.mode == "balanced" then "en_US-lessac-medium"
                  else "en_US-amy-high";
      };
    };
  };

  config = mkIf cfg.enable {
    services.ollama = {
      enable = true;
      port = cfg.ollamaPort;
      acceleration = if cfg.preset == "pewdiepie" then "cuda" else cfg.acceleration;
      host = "127.0.0.1";  # Secure local-only by default

      # Multi-GPU optimizations for high-end setups
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
    ] ++ (optional cfg.voice.enable piper-tts)
      ++ (optional cfg.voice.enable whisper-cpp);

    # High-quality, low-latency PipeWire (96 kHz, 128-sample quantum default)
    # → ~2.7 ms theoretical round-trip latency
    # → 24-bit depth automatically negotiated on supported hardware (standard for pro audio)
    # → Ties perfectly into JACK/Tone Assistant DSP workflows
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
        ENABLE_AUDIO = "true";  # Hint for multimodal/voice features
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
