# modules/agentic-local-ai.nix
#
# Copyright (c) 2025 DeMoD LLC
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS"
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
#
# Declarative NixOS module for a complete local agentic AI stack on ArchibaldOS:
# - Ollama with optional CUDA acceleration
# - Preloaded text + vision models (with presets for different hardware tiers)
# - llama.cpp as high-performance alternative backend
# - Open WebUI as powerful agentic frontend (tools, pipelines, RAG, multi-model, image/audio upload)
# - Local STT (via Open WebUI's built-in Local Whisper / faster-whisper)
# - Local TTS: Piper (fast, low-latency, multiple voices) + optional Coqui XTTS-v2 (multilingual + cloning)
# - Optional AutoGen + CrewAI for advanced multi-agent orchestration
# - Optional vLLM for high-performance multi-GPU inference (tensor parallelism + PagedAttention)
# - Optional LLM fine-tuning environment (Axolotl-compatible)
# - Optional Folding@Home client for GPU protein folding simulations
# - High-quality, low-latency PipeWire (96 kHz / 128-sample quantum default)

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.agentic-local-ai;
in
{
  options.services.agentic-local-ai = {
    enable = mkEnableOption "Agentic local AI profile with vision, voice, multi-agent, and advanced tools";

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

    enableWebUI = mkEnableOption "Open WebUI frontend" // { default = true; };

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
          Voice optimization mode:
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
          "en_US-lessac-low"
          "en_US-lessac-medium"
          "en_US-amy-high"
          "en_GB-alan-medium"
          "en_US-hfc_male-medium"
          "de_DE-thorsten-high"
        ];
        description = "Recommended Piper voices for different use cases";
      };

      xtts = {
        enable = mkEnableOption "Coqui XTTS-v2 for SOTA multilingual TTS + voice cloning" // {
          description = "High-quality alternative/complement to Piper; best with CUDA";
        };
      };
    };

    advanced = {
      vllm = mkEnableOption "vLLM for high-performance multi-GPU inference (tensor parallelism + PagedAttention)";

      fineTuning = mkEnableOption "LLM fine-tuning environment (Axolotl-compatible with QLoRA, flash-attn, etc.)";

      foldingAtHome = mkEnableOption "Folding@Home client for GPU protein folding simulations (charity compute)";
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
      (llama-cpp.override { cudaSupport = (cfg.acceleration == "cuda" || cfg.preset == "pewdiepie"); })
      (optional cfg.voice.enable piper-tts)
      (optional cfg.voice.enable whisper-cpp)
      (optional (cfg.voice.enable && cfg.voice.xtts.enable) python312Packages.tts)
      (optional cfg.multiAgent.enable (python312.withPackages (ps: with ps; [
        pyautogen crewai crewai-tools langchain litellm
      ])))
      (optional cfg.advanced.vllm python312Packages.vllm)
      (optional cfg.advanced.fineTuning (python312.withPackages (ps: with ps; [
        torch transformers accelerate peft bitsandbytes trl datasets flash-attn
      ])))
      (optional cfg.advanced.foldingAtHome foldingathome)
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

    systemd.services.foldingathome = mkIf cfg.advanced.foldingAtHome {
      description = "Folding@Home Protein Folding Client";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.foldingathome}/bin/FAHClient";
        DynamicUser = true;
        StateDirectory = "foldingathome";
        WorkingDirectory = "/var/lib/foldingathome";
        Restart = "always";
      };
    };

    networking.firewall.allowedTCPPorts = mkIf cfg.enableWebUI [ cfg.webuiPort ];
  };
}
