# modules/agentic-local-ai.nix
#
# Copyright (c) 2025 DeMoD LLC
# All rights reserved.
#
# BSD-style license (full text in original file)
#
# Production-grade declarative NixOS module for a complete local agentic AI stack (Dec 2025):
# - Ollama with CUDA/ROCm acceleration
# - Tiered presets for different hardware capabilities
# - Open WebUI as powerful agentic frontend (tools, RAG, pipelines, vision)
# - Local voice support via Open WebUI settings (Whisper STT) + optional XTTS-v2 server
# - Packages for AutoGen/CrewAI multi-agent orchestration (manual scripting)
# - vLLM for high-performance multi-GPU inference
# - Axolotl-compatible fine-tuning environment
# - Optional Folding@Home
# - High-quality low-latency PipeWire audio

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.agentic-local-ai;

  presetConfigs = {
    default = {
      models = [
        "llama3.2:latest"
        "llama3.2:11b-vision-instruct-q5_K_M"
        "qwen2.5:32b-instruct-q6_K"
      ];
      maxLoadedModels = 4;
      numParallel = 2;
      memoryLimit = "32G";
      cpuQuota = "800%";
    };

    high-vram = {
      models = [
        "qwen3:72b-instruct-q6_K"
        "llama3.1:70b-instruct-q5_K_M"
        "deepseek-r1:67b-q5_K_M"
        "qwen3-vl:72b-instruct-q4_K_M"
      ];
      maxLoadedModels = 8;
      numParallel = 4;
      memoryLimit = "96G";
      cpuQuota = "1600%";
    };

    pewdiepie = {
      models = [
        "deepseek-r1:671b-q3_K_M"
        "qwen3-vl:235b-instruct-q4_K_M"
        "llama3.1:405b-instruct-q4_K_M"
        "deepseek-v3:671b-q3_K_M"
      ];
      maxLoadedModels = 12;
      numParallel = 8;
      memoryLimit = "512G";
      cpuQuota = "3200%";
    };
  };

  currentPreset = presetConfigs.${cfg.preset};

  effectiveModels = if cfg.models != [] then cfg.models else currentPreset.models;

in
{
  options.services.agentic-local-ai = {
    enable = mkEnableOption "Production-grade local agentic AI stack";

    preset = mkOption {
      type = types.enum [ "default" "high-vram" "pewdiepie" ];
      default = "default";
      description = ''
        Hardware tier presets (updated for December 2025 models).
      '';
    };

    acceleration = mkOption {
      type = types.nullOr (types.enum [ "cuda" "rocm" ]);
      default = if cfg.preset == "pewdiepie" then "cuda" else null;
      description = "Hardware acceleration backend (auto-set to CUDA for pewdiepie preset).";
    };

    models = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Custom model list — overrides preset if non-empty.";
    };

    modelStoragePath = mkOption {
      type = types.path;
      default = "/var/lib/ollama/models";
      description = "Path for Ollama models (can grow very large).";
    };

    loadModelsOnStartup = mkOption {
      type = types.bool;
      default = false;
      description = "Pre-load models at boot (significantly increases startup time).";
    };

    enableWebUI = mkEnableOption "Open WebUI agentic frontend" // { default = true; };

    webuiPort = mkOption {
      type = types.port;
      default = 8080;
      description = "Port for Open WebUI.";
    };

    ollamaPort = mkOption {
      type = types.port;
      default = 11434;
      description = "Port for Ollama API.";
    };

    allowRemoteAccess = mkOption {
      type = types.bool;
      default = false;
      description = "Bind services to 0.0.0.0 (requires firewall rules and authentication).";
    };

    multiAgent.enable = mkEnableOption "Install AutoGen and CrewAI packages for multi-agent orchestration";

    voice = {
      enable = mkEnableOption "Enable voice-related components (STT/TTS configured in Open WebUI)";

      mode = mkOption {
        type = types.enum [ "fast" "balanced" "high-quality" ];
        default = if cfg.preset == "pewdiepie" then "high-quality"
                  else if cfg.acceleration == "cuda" then "balanced"
                  else "fast";
        description = "Voice processing mode hint (used for XTTS default).";
      };

      piperVoices = mkOption {
        type = types.listOf types.str;
        default = [ "en_US-lessac-medium" "en_US-amy-high" ];
        description = "Piper voices to make available (for manual use or future extensions).";
      };

      xtts = {
        enable = mkEnableOption "Coqui XTTS-v2 server for high-quality multilingual TTS + voice cloning" // {
          default = cfg.voice.mode == "high-quality";
        };

        port = mkOption {
          type = types.port;
          default = 8083;
        };
      };
    };

    advanced = {
      vllm = {
        enable = mkEnableOption "vLLM for production multi-GPU inference";

        port = mkOption {
          type = types.port;
          default = 8000;
        };

        tensorParallelSize = mkOption {
          type = types.int;
          default = if cfg.preset == "pewdiepie" then 8
                    else if cfg.preset == "high-vram" then 4
                    else 1;
        };

        model = mkOption {
          type = types.str;
          default = "meta-llama/Meta-Llama-3.1-405B-Instruct";
          description = "Hugging Face model ID served by vLLM.";
        };
      };

      fineTuning = {
        enable = mkEnableOption "Axolotl-compatible fine-tuning environment";

        workspacePath = mkOption {
          type = types.path;
          default = "/var/lib/llm-training";
        };
      };

      foldingAtHome = {
        enable = mkEnableOption "Folding@Home client";

        gpuEnabled = mkOption {
          type = types.bool;
          default = cfg.acceleration != null;
        };
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      assertions = [
        {
          assertion = cfg.preset == "pewdiepie" -> cfg.acceleration == "cuda";
          message = "pewdiepie preset requires CUDA acceleration.";
        }
        {
          assertion = cfg.voice.xtts.enable -> cfg.acceleration != null;
          message = "XTTS-v2 requires GPU acceleration for reasonable performance.";
        }
        {
          assertion = cfg.advanced.vllm.enable -> cfg.acceleration == "cuda";
          message = "vLLM currently requires CUDA (ROCm support experimental).";
        }
        {
          assertion = cfg.allowRemoteAccess -> config.networking.firewall.enable;
          message = "Remote access requires firewall to be enabled.";
        }
      ];

      warnings = [
        "Local STT (Whisper) and basic TTS are configured in Open WebUI → Admin → Settings → Audio."
      ] ++ optional cfg.multiAgent.enable
        "AutoGen/CrewAI packages installed — run multi-agent scripts manually."
      ++ optional cfg.allowRemoteAccess
        "Remote access enabled: use HTTPS reverse proxy and strong authentication.";
    }

    # Ollama core service
    {
      services.ollama = {
        enable = true;
        port = cfg.ollamaPort;
        acceleration = cfg.acceleration;
        host = if cfg.allowRemoteAccess then "0.0.0.0" else "127.0.0.1";

        environment = {
          OLLAMA_MODELS = cfg.modelStoragePath;
          OLLAMA_MAX_LOADED_MODELS = toString currentPreset.maxLoadedModels;
          OLLAMA_NUM_PARALLEL = toString currentPreset.numParallel;
          OLLAMA_KEEP_ALIVE = "10m";
        };

        loadModels = mkIf cfg.loadModelsOnStartup effectiveModels;
      };

      systemd.services.ollama.serviceConfig = {
        MemoryMax = currentPreset.memoryLimit;
        CPUQuota = currentPreset.cpuQuota;
        Nice = -10;
        IOSchedulingClass = "realtime";
        IOSchedulingPriority = 0;
      };
    }

    # Open WebUI
    (mkIf cfg.enableWebUI {
      systemd.services.open-webui = {
        description = "Open WebUI - Agentic Frontend";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "ollama.service" ];
        requires = [ "ollama.service" ];

        environment = {
          OLLAMA_API_BASE = "http://127.0.0.1:${toString cfg.ollamaPort}";
          PORT = toString cfg.webuiPort;
          HOST = if cfg.allowRemoteAccess then "0.0.0.0" else "127.0.0.1";
          WEBUI_AUTH = toString cfg.allowRemoteAccess;  # Enable built-in auth when remote
        };

        serviceConfig = {
          ExecStart = "${pkgs.open-webui}/bin/open-webui serve";
          DynamicUser = true;
          StateDirectory = "open-webui";
          WorkingDirectory = "/var/lib/open-webui";
          Restart = "on-failure";
          MemoryMax = "8G";
          CPUQuota = "400%";
          NoNewPrivileges = true;
          PrivateTmp = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          ReadWritePaths = [ "/var/lib/open-webui" ];
        };
      };

      networking.firewall.allowedTCPPorts = mkIf cfg.allowRemoteAccess [ cfg.webuiPort cfg.ollamaPort ];
    })

    # Voice components
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

      # XTTS-v2 server (high-quality TTS)
      systemd.services.xtts = mkIf cfg.voice.xtts.enable {
        description = "Coqui XTTS-v2 TTS Server";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "pipewire.service" ];

        environment = {
          CUDA_VISIBLE_DEVICES = mkIf (cfg.acceleration == "cuda") "0,1,2,3,4,5,6,7";
        };

        serviceConfig = {
          ExecStart = ''
            ${pkgs.python312Packages.tts}/bin/tts-server \
              --model_name tts_models/multilingual/multi-dataset/xtts_v2 \
              --port ${toString cfg.voice.xtts.port} \
              --host ${if cfg.allowRemoteAccess then "0.0.0.0" else "127.0.0.1"}
          '';
          DynamicUser = true;
          StateDirectory = "xtts";
          CacheDirectory = "xtts";
          Restart = "on-failure";
          MemoryMax = "16G";
          Nice = -5;
        };
      };

      networking.firewall.allowedTCPPorts = mkIf (cfg.allowRemoteAccess && cfg.voice.xtts.enable) [ cfg.voice.xtts.port ];
    })

    # vLLM high-performance inference
    (mkIf cfg.advanced.vllm.enable {
      systemd.services.vllm = {
        description = "vLLM Multi-GPU Inference Server";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];

        environment = {
          CUDA_VISIBLE_DEVICES = "all";
        };

        serviceConfig = {
          ExecStart = ''
            ${pkgs.python312Packages.vllm}/bin/vllm serve ${cfg.advanced.vllm.model} \
              --host ${if cfg.allowRemoteAccess then "0.0.0.0" else "127.0.0.1"} \
              --port ${toString cfg.advanced.vllm.port} \
              --tensor-parallel-size ${toString cfg.advanced.vllm.tensorParallelSize} \
              --dtype auto \
              --max-model-len 131072
          '';
          DynamicUser = true;
          StateDirectory = "vllm";
          CacheDirectory = "vllm";
          Restart = "on-failure";
          MemoryMax = currentPreset.memoryLimit;
          Nice = -10;
        };
      };

      networking.firewall.allowedTCPPorts = mkIf cfg.allowRemoteAccess [ cfg.advanced.vllm.port ];
    })

    # Fine-tuning workspace
    (mkIf cfg.advanced.fineTuning.enable {
      users.users.llm-trainer = {
        isSystemUser = true;
        group = "llm-trainer";
        home = cfg.advanced.fineTuning.workspacePath;
        createHome = true;
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
        description = "Folding@Home Client";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];

        serviceConfig = {
          ExecStart = "${pkgs.foldingathome}/bin/FAHClient";
          DynamicUser = true;
          StateDirectory = "foldingathome";
          WorkingDirectory = "/var/lib/foldingathome";
          Restart = "always";
          Nice = 19;
        };
      };
    })

    # System packages
    {
      environment.systemPackages = with pkgs; [
        ollama
        (llama-cpp.override {
          cudaSupport = (cfg.acceleration == "cuda" || cfg.preset == "pewdiepie");
          rocmSupport = (cfg.acceleration == "rocm");
        })
        whisper-cpp
        piper-tts
      ] ++ optionals cfg.enableWebUI [ open-webui ]
        ++ optionals cfg.voice.xtts.enable [ python312Packages.tts ]
        ++ optionals cfg.multiAgent.enable [
          (python312.withPackages (ps: with ps: [
            pyautogen crewai crewai-tools langchain litellm
          ]))
        ]
        ++ optionals cfg.advanced.vllm.enable [ python312Packages.vllm ]
        ++ optionals cfg.advanced.fineTuning.enable [
          python312Packages.deepspeed
          python312Packages.flash-attn
        ];
    }
  ]);
}
