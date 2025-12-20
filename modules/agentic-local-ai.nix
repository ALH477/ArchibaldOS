{ config, lib, pkgs, ... }:

let
  cfg = config.services.agentic-local-ai;
  
  preload_models = ''
    echo "Starting model preload process..."
    
    # Health check before pulling models
    max_retries=10
    retry_count=0
    
    while [ $retry_count -lt $max_retries ]; do
      if curl -s http://localhost:8000/health > /dev/null 2>&1; then
        echo "Service is healthy, proceeding with model preload..."
        break
      fi
      
      retry_count=$((retry_count + 1))
      echo "Health check attempt $retry_count/$max_retries failed, retrying in 2 seconds..."
      sleep 2
    done
    
    if [ $retry_count -eq $max_retries ]; then
      echo "ERROR: Service failed to become healthy after $max_retries attempts"
      exit 1
    fi
    
    # Pull models
    ${lib.concatStringsSep "\n" (map (model: "ollama pull ${model}") cfg.models)}
    
    echo "Model preload process completed."
  '';
in
{
  options = {
    services.agentic-local-ai = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Enable the agentic local AI service";
      };

      autoStart = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Automatically start the agentic-local-ai service on system boot";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.ollama;
        description = "The package to use for the local AI service";
      };

      models = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        example = [ "neural-chat" "mistral" ];
        description = "List of models to preload on startup";
      };

      port = lib.mkOption {
        type = lib.types.int;
        default = 8000;
        description = "Port for the local AI service to listen on";
      };

      dataDir = lib.mkOption {
        type = lib.types.path;
        default = "/var/lib/ollama";
        description = "Directory for storing model data";
      };

      extraConfig = lib.mkOption {
        type = lib.types.attrs;
        default = { };
        description = "Extra configuration options for the service";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.agentic-local-ai = {
      description = "Agentic Local AI Service";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = lib.mkIf cfg.autoStart [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${cfg.package}/bin/ollama serve --addr 127.0.0.1:${toString cfg.port}";
        Restart = "always";
        RestartSec = 5;
        User = "ollama";
        Group = "ollama";
        StateDirectory = "ollama";
        StateDirectoryMode = "0700";
      };
    };

    systemd.services.agentic-local-ai-preload = {
      description = "Agentic Local AI Model Preload";
      after = [ "agentic-local-ai.service" ];
      requires = [ "agentic-local-ai.service" ];
      wantedBy = lib.mkIf cfg.autoStart [ "multi-user.target" ];

      script = preload_models;

      serviceConfig = {
        Type = "oneshot";
        User = "ollama";
        Group = "ollama";
        RemainAfterExit = true;
      };
    };

    users.users.ollama = lib.mkIf (!(builtins.pathExists "/etc/passwd") || !(lib.hasAttrByPath [ "users" "users" "ollama" ] config)) {
      isSystemUser = true;
      group = "ollama";
      home = cfg.dataDir;
      createHome = true;
    };

    users.groups.ollama = { };

    environment.variables = {
      OLLAMA_HOST = "127.0.0.1:${toString cfg.port}";
      OLLAMA_MODELS = cfg.dataDir;
    };
  };
}
