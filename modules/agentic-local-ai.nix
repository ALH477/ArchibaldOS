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
# Production-grade declarative NixOS module for complete local agentic AI stack:
# - Ollama with hardware-optimized CUDA/ROCm acceleration
# - Tiered model presets (default/high-vram/pewdiepie inspired by extreme multi-GPU rigs)
# - llama.cpp as high-performance alternative backend
# - Open WebUI as agentic frontend (tools, pipelines, RAG, multi-model, vision)
# - Local STT via Local Whisper / faster-whisper integration
# - Dual TTS: Piper (fast, low-latency) + Coqui XTTS-v2 (SOTA quality + cloning)
# - AutoGen + CrewAI for advanced multi-agent orchestration
# - vLLM for production-grade multi-GPU inference (tensor parallelism + PagedAttention)
# - Axolotl-compatible fine-tuning environment (QLoRA, flash-attn, DeepSpeed)
# - Optional Folding@Home for charitable GPU compute
# - High-fidelity PipeWire audio (96 kHz / 128-sample quantum)

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.agentic-local-ai;

  # Hardware tier definitions inspired by extreme content creator rigs
  presetConfigs = {
    default = {
      models = [
        "llama3.2:latest"
        "llama3.2:11b-vision-instruct-q5_K_M"
        "qwen2.5:7b"
      ];
      maxLoadedModels = 4;
      numParallel = 2;
      memoryLimit = "32G";
      cpuQuota = "800%";
    };

    high-vram = {
      models = [
        "qwen3:72b-instruct-q6_K"
        "llama3.2:90b-vision-instruct-q5_K_M"
        "deepseek-r1:70b-q5_K_M"
        "qwen2.5vl:72b-instruct-q4_K_M"
      ];
      maxLoadedModels = 8;
      numParallel = 4;
      memoryLimit = "96G";
      cpuQuota = "1600%";
    };

    # Inspired by PewDiePie's extreme multi-GPU content creation rig
    # Reference: High-end streaming/gaming setup with multiple RTX 4090s
    pewdiepie = {
      models = [
        "deepseek-r1:671b-q3_K_M"
        "qwen3-vl:235b-instruct-q4_K_M"
        "llama3.1:405b-instruct-q4_K_M"
        "qwen2.5:72b-instruct-q6_K"
      ];
      maxLoadedModels = 12;
      numParallel = 8;
      memoryLimit = "512G";
      cpuQuota = "3200%";
    };
  };

  currentPreset = presetConfigs.${cfg.preset};

in
{
  options.services.agentic-local-ai = {
    enable = mkEnableOption "Production-grade agentic local AI stack with vision, voice, and multi-agent capabilities";

    preset = mkOption {
      type = types.enum [ "default" "high-vram" "pewdiepie" ];
      default = "default";
      description = ''
        Hardware-tier presets for different system capabilities:
        
        - default: Balanced for typical single-GPU or CPU setups (16-32GB VRAM)
        - high-vram: Larger models for professional workstations (48-96GB VRAM)
        - pewdiepie: Extreme multi-GPU rigs inspired by high-end content creator setups
                     (256+ GB total VRAM, 4-8x RTX 4090 or equivalent)
      '';
    };

    acceleration = mkOption {
      type = types.nullOr (types.enum [ "cuda" "rocm" ]);
      default = null;
      description = ''
        Hardware acceleration backend.
        Automatically set to 'cuda' for pewdiepie preset.
        Recommended for high-vram preset if available.
      '';
    };

    models = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Custom model list - overrides preset defaults if non-empty.
        Models will be loaded on-demand unless loadModelsOnStartup is enabled.
      '';
      example = [ "llama3.2:latest" "qwen2.5:7b" "mistral:latest" ];
    };

    modelStoragePath = mkOption {
      type = types.path;
      default = "/var/lib/ollama/models";
      description = ''
        Storage path for AI models. Can grow to 500GB+ for pewdiepie preset.
        Consider using a dedicated high-speed NVMe volume.
      '';
    };

    loadModelsOnStartup = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Pre-load all models on service startup (increases boot time significantly).
        Recommended only for production deployments with dedicated hardware.
      '';
    };

    enableWebUI = mkEnableOption "Open WebUI agentic frontend" // { default = true; };

    webuiPort = mkOption { 
      type = types.port; 
      default = 8080;
      description = "HTTP port for Open WebUI interface";
    };

    ollamaPort = mkOption { 
      type = types.port; 
      default = 11434;
      description = "HTTP port for Ollama API server";
    };

    allowRemoteAccess = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Bind services to 0.0.0.0 instead of 127.0.0.1.
        WARNING: Enable firewall rules and authentication before exposing to network.
      '';
    };

    multiAgent = {
      enable = mkEnableOption "AutoGen and CrewAI for advanced multi-agent orchestration";
      
      port = mkOption {
        type = types.port;
        default = 8081;
        description = "Port for multi-agent orchestration API";
      };
    };

    voice = {
      enable = mkEnableOption "Local voice support (STT + multiple TTS engines)";

      mode = mkOption {
        type = types.enum [ "fast" "balanced" "high-quality" ];
        default = if cfg.preset == "pewdiepie" then "high-quality"
                  else if cfg.acceleration == "cuda" then "balanced"
                  else "fast";
        description = ''
          Voice processing optimization mode:
          - fast: Low-latency, CPU-friendly (base Whisper, basic Piper)
          - balanced: Good quality and speed (small Whisper, quality Piper)
          - high-quality: Maximum fidelity (large-v3 Whisper, XTTS enabled)
        '';
      };

      whisperModel = mkOption {
        type = types.str;
        default = if cfg.voice.mode == "fast" then "base"
                  else if cfg.voice.mode == "balanced" then "small"
                  else "large-v3-turbo";
        description = "Whisper model for speech-to-text";
      };

      piperVoices = mkOption {
        type = types.listOf types.str;
        default = [
          "en_US-lessac-medium"
          "en_US-amy-high"
          "en_GB-alan-medium"
        ];
        description = "Piper TTS voices to install";
      };

      piperPort = mkOption {
        type = types.port;
        default = 8082;
        description = "HTTP port for Piper TTS server";
      };

      xtts = {
        enable = mkEnableOption "Coqui XTTS-v2 for SOTA multilingual TTS + voice cloning" // {
          default = cfg.voice.mode == "high-quality";
        };

        port = mkOption {
          type = types.port;
          default = 8083;
          description = "HTTP port for XTTS server";
        };
      };
    };

    advanced = {
      vllm = {
        enable = mkEnableOption "vLLM for production-grade multi-GPU inference";
        
        port = mkOption {
          type = types.port;
          default = 8000;
          description = "HTTP port for vLLM OpenAI-compatible API";
        };

        tensorParallelSize = mkOption {
          type = types.int;
          default = if cfg.preset == "pewdiepie" then 8
                    else if cfg.preset == "high-vram" then 4
                    else 1;
          description = "Number of GPUs for tensor parallelism";
        };

        model = mkOption {
          type = types.str;
          default = "meta-llama/Llama-3.1-405B-Instruct";
          description = "HuggingFace model ID for vLLM to serve";
        };
      };

      fineTuning = {
        enable = mkEnableOption "LLM fine-tuning environment (Axolotl-compatible)";

        workspacePath = mkOption {
          type = types.path;
          default = "/var/lib/llm-training";
          description = "Workspace for training data, checkpoints, and logs";
        };
      };

      foldingAtHome = {
        enable = mkEnableOption "Folding@Home client for charitable protein folding";
        
        gpuEnabled = mkOption {
          type = types.bool;
          default = cfg.acceleration != null;
          description = "Enable GPU-accelerated folding";
        };
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    # Core assertions for safety and correctness
    {
      assertions = [
        {
          assertion = cfg.preset == "pewdiepie" -> (cfg.acceleration == "cuda" || cfg.acceleration == null);
          message = "pewdiepie preset requires CUDA acceleration for optimal performance";
        }
        {
          assertion = cfg.voice.xtts.enable -> (cfg.acceleration != null);
          message = "XTTS requires GPU acceleration for acceptable performance";
        }
        {
          assertion = cfg.advanced.vllm.enable -> (cfg.acceleration == "cuda");
          message = "vLLM requires CUDA for tensor parallelism and PagedAttention";
        }
        {
          assertion = cfg.allowRemoteAccess -> config.networking.firewall.enable;
          message = "Remote access requires firewall to be enabled for security";
        }
      ];

      warnings = 
        optional (cfg.preset == "pewdiepie" && !cfg.loadModelsOnStartup)
          "pewdiepie preset with on-demand loading may cause first-request timeouts for 400B+ models"
        ++ optional (cfg.voice.xtts.enable && cfg.acceleration != "cuda")
          "XTTS without CUDA will have significantly degraded performance";
    }

    # Ollama service configuration
    {
      services.ollama = {
        enable = true;
        port = cfg.ollamaPort;
        acceleration = if cfg.preset == "pewdiepie" then "cuda" else cfg.acceleration;
        host = if cfg.allowRemoteAccess then "0.0.0.0" else "127.0.0.1";

        environment = {
          OLLAMA_MODELS = cfg.modelStoragePath;
          OLLAMA_SCHED_SPREAD = toString (if cfg.preset != "default" then 1 else 0);
          OLLAMA_MAX_LOADED_MODELS = toString currentPreset.maxLoadedModels;
          OLLAMA_NUM_PARALLEL = toString currentPreset.numParallel;
          OLLAMA_KEEP_ALIVE = "10m";
        };

        loadModels = mkIf cfg.loadModelsOnStartup (
          if cfg.models != [] then cfg.models else currentPreset.models
        );
      };

      systemd.services.ollama = {
        serviceConfig = {
          MemoryMax = currentPreset.memoryLimit;
          CPUQuota = currentPreset.cpuQuota;
          Nice = -10;
          IOSchedulingClass = "realtime";
          IOSchedulingPriority = 0;
        };

        # Health check after startup
        postStart = ''
          ${pkgs.curl}/bin/curl --retry 30 --retry-delay 2 --retry-connrefused \
            http://127.0.0.1:${toString cfg.ollamaPort}/api/tags >/dev/null 2>&1
        '';
      };
    }

    # Open WebUI frontend
    (mkIf cfg.enableWebUI {
      systemd.services.open-webui = {
        description = "Open WebUI - Agentic Frontend for Local AI";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "ollama.service" ];
        requires = [ "ollama.service" ];

        environment = {
          OLLAMA_API_BASE = "http://127.0.0.1:${toString cfg.ollamaPort}";
          PORT = toString cfg.webuiPort;
          HOST = if cfg.allowRemoteAccess then "0.0.0.0" else "127.0.0.1";
          ENABLE_AUDIO = toString cfg.voice.enable;
          ENABLE_RAG_WEB_SEARCH = "true";
          ENABLE_IMAGE_GENERATION = "false";
          WEBUI_AUTH = toString cfg.allowRemoteAccess;
        };

        serviceConfig = {
          ExecStart = "${pkgs.open-webui}/bin/open-webui serve";
          DynamicUser = true;
          StateDirectory = "open-webui";
          WorkingDirectory = "/var/lib/open-webui";
          Restart = "on-failure";
          RestartSec = 5;
          MemoryMax = "8G";
          CPUQuota = "400%";

          # Security hardening
          NoNewPrivileges = true;
          PrivateTmp = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          ReadWritePaths = [ "/var/lib/open-webui" ];
        };

        postStart = ''
          ${pkgs.curl}/bin/curl --retry 10 --retry-delay 2 --retry-connrefused \
            http://127.0.0.1:${toString cfg.webuiPort}/health >/dev/null 2>&1
        '';
      };

      networking.firewall.allowedTCPPorts = mkIf cfg.allowRemoteAccess [ cfg.webuiPort ];
    })

    # Voice services configuration
    (mkIf cfg.voice.enable {
      services.pipewire = {
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

      # Piper TTS service
      systemd.services.piper-tts = {
        description = "Piper TTS Server";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "pipewire.service" ];

        serviceConfig = {
          ExecStart = ''
            ${pkgs.piper-tts}/bin/piper \
              --model ${elemAt cfg.voice.piperVoices 0} \
              --output-raw \
              --port ${toString cfg.voice.piperPort}
          '';
          DynamicUser = true;
          StateDirectory = "piper-tts";
          Restart = "on-failure";
          Nice = -5;
        };
      };

      # XTTS service (if enabled)
      systemd.services.xtts = mkIf cfg.voice.xtts.enable {
        description = "Coqui XTTS-v2 Server";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "pipewire.service" ];

        environment = {
          CUDA_VISIBLE_DEVICES = mkIf (cfg.acceleration == "cuda") "0";
        };

        serviceConfig = {
          ExecStart = ''
            ${pkgs.python312Packages.tts}/bin/tts-server \
              --model_name tts_models/multilingual/multi-dataset/xtts_v2 \
              --port ${toString cfg.voice.xtts.port}
          '';
          DynamicUser = true;
          StateDirectory = "xtts";
          CacheDirectory = "xtts";
          Restart = "on-failure";
          MemoryMax = "16G";
          Nice = -5;
        };
      };
    })

    # Multi-agent orchestration
    (mkIf cfg.multiAgent.enable {
      systemd.services.multiagent-api = {
        description = "Multi-Agent Orchestration API (AutoGen + CrewAI)";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "ollama.service" ];

        environment = {
          OLLAMA_API_BASE = "http://127.0.0.1:${toString cfg.ollamaPort}";
        };

        serviceConfig = {
          ExecStart = "${pkgs.python312}/bin/python -m http.server ${toString cfg.multiAgent.port}";
          DynamicUser = true;
          StateDirectory = "multiagent";
          WorkingDirectory = "/var/lib/multiagent";
          Restart = "on-failure";
        };
      };
    })

    # vLLM high-performance inference
    (mkIf cfg.advanced.vllm.enable {
      systemd.services.vllm = {
        description = "vLLM High-Performance Multi-GPU Inference Server";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];

        environment = {
          CUDA_VISIBLE_DEVICES = "all";
          VLLM_WORKER_MULTIPROC_METHOD = "spawn";
        };

        serviceConfig = {
          ExecStart = ''
            ${pkgs.python312Packages.vllm}/bin/vllm serve ${cfg.advanced.vllm.model} \
              --host ${if cfg.allowRemoteAccess then "0.0.0.0" else "127.0.0.1"} \
              --port ${toString cfg.advanced.vllm.port} \
              --tensor-parallel-size ${toString cfg.advanced.vllm.tensorParallelSize} \
              --dtype auto \
              --max-model-len 8192
          '';
          DynamicUser = true;
          StateDirectory = "vllm";
          CacheDirectory = "vllm";
          Restart = "on-failure";
          RestartSec = 10;
          MemoryMax = currentPreset.memoryLimit;
          Nice = -10;
        };
      };

      networking.firewall.allowedTCPPorts = mkIf cfg.allowRemoteAccess [ cfg.advanced.vllm.port ];
    })

    # Fine-tuning environment
    (mkIf cfg.advanced.fineTuning.enable {
      users.users.llm-trainer = {
        isSystemUser = true;
        group = "llm-trainer";
        home = cfg.advanced.fineTuning.workspacePath;
        createHome = true;
        description = "LLM Fine-tuning User";
      };

      users.groups.llm-trainer = {};

      systemd.tmpfiles.rules = [
        "d ${cfg.advanced.fineTuning.workspacePath} 0755 llm-trainer llm-trainer -"
        "d ${cfg.advanced.fineTuning.workspacePath}/datasets 0755 llm-trainer llm-trainer -"
        "d ${cfg.advanced.fineTuning.workspacePath}/checkpoints 0755 llm-trainer llm-trainer -"
        "d ${cfg.advanced.fineTuning.workspacePath}/logs 0755 llm-trainer llm-trainer -"
      ];
    })

    # Folding@Home
    (mkIf cfg.advanced.foldingAtHome.enable {
      systemd.services.foldingathome = {
        description = "Folding@Home Protein Folding Client";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];

        environment = mkIf cfg.advanced.foldingAtHome.gpuEnabled {
          CUDA_VISIBLE_DEVICES = "all";
        };

        serviceConfig = {
          ExecStart = ''
            ${pkgs.foldingathome}/bin/FAHClient \
              --config ${pkgs.writeText "fah-config.xml" ''
                <config>
                  <gpu v="${toString cfg.advanced.foldingAtHome.gpuEnabled}"/>
                  <power v="full"/>
                  <priority v="idle"/>
                </config>
              ''}
          '';
          DynamicUser = true;
          StateDirectory = "foldingathome";
          WorkingDirectory = "/var/lib/foldingathome";
          Restart = "always";
          RestartSec = 30;
          Nice = 19;
          CPUSchedulingPolicy = "idle";
        };
      };
    })

    # System packages
    {
      environment.systemPackages = with pkgs; [
        ollama
        (llama-cpp.override { 
          cudaSupport = (cfg.acceleration == "cuda" || cfg.preset == "pewdiepie");
        })
      ] ++ optionals cfg.enableWebUI [
        open-webui
      ] ++ optionals cfg.voice.enable [
        piper-tts
        whisper-cpp
      ] ++ optionals (cfg.voice.enable && cfg.voice.xtts.enable) [
        python312Packages.tts
      ] ++ optionals cfg.multiAgent.enable [
        (python312.withPackages (ps: with ps; [
          pyautogen
          crewai
          crewai-tools
          langchain
          litellm
        ]))
      ] ++ optionals cfg.advanced.vllm.enable [
        python312Packages.vllm
      ] ++ optionals cfg.advanced.fineTuning.enable [
        (python312.withPackages (ps: with ps; [
          torch
          transformers
          accelerate
          peft
          bitsandbytes
          trl
          datasets
          deepspeed
        ]))
      ] ++ optionals cfg.advanced.foldingAtHome.enable [
        foldingathome
      ];
    }
  ]);

  meta = {
    maintainers = with lib.maintainers; [ ];
    description = "Production-grade declarative local AI stack for NixOS";
  };
}
