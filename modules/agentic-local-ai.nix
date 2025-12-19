# modules/agentic-local-ai.nix
#
# BSD-style license (full text in LICENSE)
# Copyright (c) 2025 DeMoD LLC
# All rights reserved.
#
# Production-grade declarative NixOS module for a complete local agentic AI stack (December 18, 2025)
#
# Features:
# - Ollama with CUDA/ROCm acceleration
# - Tiered hardware presets (default / high-vram / pewdiepie)
# - Open WebUI as powerful agentic frontend (RAG, tools, pipelines, vision)
# - High-quality voice via Chatterbox Turbo TTS (installed from source per official guide)
# - Declarative import of manually downloaded GGUF models (localGgufModels)
# - vLLM for production multi-GPU inference
# - Axolotl-compatible fine-tuning environment
# - Optional Folding@Home
# - Optional Nexa SDK for multimodal audio models (e.g., OmniAudio-2.6B) installed via official script
# - High-fidelity low-latency PipeWire audio

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

  # Python 3.11 environment for Chatterbox Turbo (official recommended version)
  chatterboxPython = pkgs.python311;

  chatterboxEnv = chatterboxPython.withPackages (ps: with ps; [
    torchWithCuda
    fastapi
    uvicorn
    pydantic
  ]);

  # Fetch Chatterbox source from GitHub
  chatterboxSrc = pkgs.fetchFromGitHub {
    owner = "resemble-ai";
    repo = "chatterbox";
    rev = "main"; # For production, replace with a specific tag/commit
    sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="; # Replace with actual hash
  };

  # Package the source for editable install
  chatterboxPackage = pkgs.stdenv.mkDerivation {
    name = "chatterbox-tts-src";
    src = chatterboxSrc;
    buildInputs = [ chatterboxPython ];
    installPhase = ''
      mkdir -p $out
      cp -r . $out
    '';
  };

in
{
  options.services.agentic-local-ai = {
    enable = mkEnableOption "Production-grade local agentic AI stack";

    preset = mkOption {
      type = types.enum [ "default" "high-vram" "pewdiepie" ];
      default = "default";
      description = "Hardware tier presets (updated for December 2025 models).";
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
      description = "Pre-load official models at boot via ollama pull.";
    };

    enableWebUI = mkEnableOption "Open WebUI agentic frontend" // { default = true; };

    webuiPort = mkOption {
      type = types.port;
      default = 8080;
      description = "HTTP port for Open WebUI.";
    };

    ollamaPort = mkOption {
      type = types.port;
      default = 11434;
      description = "HTTP port for Ollama API.";
    };

    allowRemoteAccess = mkOption {
      type = types.bool;
      default = false;
      description = "Bind services to 0.0.0.0 (requires firewall rules and authentication).";
    };

    multiAgent.enable = mkEnableOption "Install AutoGen and CrewAI packages for multi-agent orchestration";

    voice = {
      enable = mkEnableOption "High-quality local voice support via Chatterbox Turbo TTS" // { default = true; };

      chatterbox = {
        port = mkOption {
          type = types.port;
          default = 8083;
          description = "Port for Chatterbox Turbo OpenAI-compatible TTS server";
        };

        host = mkOption {
          type = types.str;
          default = "127.0.0.1";
          description = "Bind host for Chatterbox Turbo server";
        };
      };
    };

    localGgufModels = mkOption {
      type = types.listOf (types.submodule {
        options = {
          name = mkOption {
            type = types.str;
            description = "Ollama model name/tag (e.g., deepseek-r1:67b-local)";
          };
          ggufPath = mkOption {
            type = types.path;
            description = ''
              Path to a manually downloaded .gguf file.
              Place it next to your configuration (e.g., ./models/my-model.gguf).
              Nix will copy it into the store and import it into Ollama automatically.
            '';
          };
          extraModelfile = mkOption {
            type = types.lines;
            default = "";
            description = ''
              Additional Modelfile content (TEMPLATE, PARAMETER, SYSTEM, etc.).
              Recommended: copy from a similar official model via
              `ollama show --modelfile similar-model`.
            '';
          };
        };
      });
      default = [];
      description = "Declaratively import manually downloaded GGUF models into Ollama.";
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

      nexaSdk = {
        enable = mkEnableOption "Nexa SDK for multimodal audio models (e.g., OmniAudio-2.6B)" // { default = false; };

        model = mkOption {
          type = types.str;
          default = "NexaAI/OmniAudio-2.6B";
          description = "Nexa model to serve (auto-downloads GGUF on first use)";
        };

        port = mkOption {
          type = types.port;
          default = 8084;
          description = "Port for Nexa SDK OpenAI-compatible server";
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
          assertion = cfg.voice.enable -> cfg.acceleration != null;
          message = "Chatterbox Turbo performs best with GPU acceleration.";
        }
        {
          assertion = cfg.advanced.vllm.enable -> cfg.acceleration == "cuda";
          message = "vLLM currently requires CUDA.";
        }
        {
          assertion = cfg.advanced.nexaSdk.enable -> cfg.acceleration != null;
          message = "Nexa SDK multimodal audio models recommend GPU acceleration.";
        }
        {
          assertion = cfg.allowRemoteAccess -> config.networking.firewall.enable;
          message = "Remote access requires firewall to be enabled.";
        }
      ];

      warnings = [
        "Local STT and basic TTS are configured in Open WebUI → Admin → Settings → Audio."
      ] ++ optional cfg.voice.enable
        "Chatterbox Turbo TTS server running at http://localhost:${toString cfg.voice.chatterbox.port}/v1 (OpenAI-compatible)."
      ++ optional cfg.advanced.nexaSdk.enable
        "Nexa SDK server running at http://localhost:${toString cfg.advanced.nexaSdk.port} for native audio understanding.";
    }

    # Core Ollama service
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

    # Open WebUI frontend
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
          WEBUI_AUTH = toString cfg.allowRemoteAccess;
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

    # Voice support - Chatterbox Turbo TTS (source install)
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

      systemd.services.chatterbox-tts = {
        description = "Chatterbox Turbo TTS Server (OpenAI-compatible)";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "pipewire.service" ];

        environment = {
          CUDA_VISIBLE_DEVICES = mkIf (cfg.acceleration == "cuda") "all";
          PYTHONUNBUFFERED = "1";
          PYTHONPATH = "${chatterboxPackage}:${chatterboxEnv}/${chatterboxPython.sitePackages}";
        };

        serviceConfig = {
          ExecStart = ''
            ${chatterboxEnv}/bin/uvicorn chatterbox.server:app \
              --host ${cfg.voice.chatterbox.host} \
              --port ${toString cfg.voice.chatterbox.port}
          '';
          DynamicUser = true;
          StateDirectory = "chatterbox-tts";
          CacheDirectory = "chatterbox-tts";
          Restart = "on-failure";
          MemoryMax = "16G";
          Nice = -5;
        };

        preStart = ''
          if [ ! -f /var/lib/chatterbox-tts/installed ]; then
            ${chatterboxEnv}/bin/pip install -e ${chatterboxPackage} --no-deps
            mkdir -p /var/lib/chatterbox-tts
            touch /var/lib/chatterbox-tts/installed
          fi
        '';
      };

      networking.firewall.allowedTCPPorts = mkIf cfg.allowRemoteAccess [ cfg.voice.chatterbox.port ];
    })

    # Declarative import of manually downloaded GGUF models
    (mkIf (cfg.localGgufModels != []) {
      systemd.tmpfiles.rules = [
        "d ${cfg.modelStoragePath}/local-ggufs 0755 root root -"
      ];

      system.activationScripts.importLocalGgufModels = stringAfter [ "var" ] ''
        ${concatMapStringsSep "\n" (model: let
          storeGguf = model.ggufPath;
          destGguf = "${cfg.modelStoragePath}/local-ggufs/${baseNameOf model.ggufPath}";
          modelfilePath = "${cfg.modelStoragePath}/local-ggufs/${model.name}-Modelfile";
        in ''
          echo "Importing local GGUF model: ${model.name}"

          if [ ! -f "${destGguf}" ] || ! ${pkgs.diffutils}/bin/cmp -s ${storeGguf} ${destGguf}; then
            cp -f ${storeGguf} ${destGguf}
            chmod 644 ${destGguf}
          fi

          cat > ${modelfilePath} <<'EOF'
          FROM ${destGguf}
          ${model.extraModelfile}
          EOF

          ${pkgs.ollama}/bin/ollama create ${model.name} -f ${modelfilePath} || true
        '') cfg.localGgufModels}
      '';
    })

    # Nexa SDK multimodal audio support (OmniAudio-2.6B)
    (mkIf cfg.advanced.nexaSdk.enable {
      system.activationScripts.installNexaSdk = stringAfter [ "users" "groups" ] ''
        NEXA_BIN="/usr/local/bin/nexa"
        if [ ! -f "$NEXA_BIN" ]; then
          echo "Installing Nexa SDK via official installer script..."
          ${pkgs.curl}/bin/curl -fsSL https://nexa.ai/install.sh -o /tmp/nexa-install.sh
          chmod +x /tmp/nexa-install.sh
          /tmp/nexa-install.sh
          rm /tmp/nexa-install.sh
        else
          echo "Nexa SDK already installed"
        fi
      '';

      systemd.services.nexa-server = {
        description = "Nexa SDK OpenAI-compatible Server (OmniAudio-2.6B)";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];

        environment = {
          CUDA_VISIBLE_DEVICES = mkIf (cfg.acceleration == "cuda") "all";
        };

        serviceConfig = {
          ExecStart = ''
            /usr/local/bin/nexa serve \
              --model ${cfg.advanced.nexaSdk.model} \
              --port ${toString cfg.advanced.nexaSdk.port}
          '';
          Restart = "on-failure";
          RestartSec = 10;
          WorkingDirectory = "/var/lib/nexa";
          StateDirectory = "nexa";
          CacheDirectory = "nexa";
          MemoryMax = "16G";
          Nice = -5;
        };
      };

      networking.firewall.allowedTCPPorts = mkIf cfg.allowRemoteAccess [ cfg.advanced.nexaSdk.port ];

      environment.systemPackages = [
        (pkgs.writeScriptBin "nexa" ''
          #!${pkgs.stdenv.shell}
          exec /usr/local/bin/nexa "$@"
        '')
      ];
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

    # Fine-tuning environment
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
        ++ optionals cfg.voice.enable [ chatterboxEnv ]
        ++ optionals cfg.multiAgent.enable [
          (python312.withPackages (ps: with ps: [ pyautogen crewai crewai-tools langchain litellm ]))
        ]
        ++ optionals cfg.advanced.vllm.enable [ python312Packages.vllm ]
        ++ optionals cfg.advanced.fineTuning.enable [
          python312Packages.deepspeed
          python312Packages.flash-attn
        ];
    }

    # Firewall for Ollama
    {
      networking.firewall.allowedTCPPorts = mkIf cfg.allowRemoteAccess [ cfg.ollamaPort ];
    }
  ]);
}
