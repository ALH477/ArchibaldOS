# tone-assistant-flake.nix
# Copyright 2025 DeMoD LLC
# SPDX-License-Identifier: BSD-3-Clause
#
# Tone Assistant: Neural amp modeling module for ArchibaldOS
# Provides Lisp/Incudine real-time DSP + PyTorch training pipeline

{
  description = "Tone Assistant: Neural Amp Modeling for ArchibaldOS";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        # =================================================================
        # LISP/INCUDINE ENVIRONMENT
        # =================================================================
        
        lispLibraries = with pkgs; [
          jack2
          libsndfile
          fftw
          fftwFloat
          gsl
          portmidi
          libsamplerate
        ];

        lispBuildTools = with pkgs; [
          sbcl
          faust
          gcc
          gnumake
          pkg-config
          git
        ];

        lispEnv = {
          buildInputs = lispBuildTools ++ lispLibraries;
          
          shellHook = ''
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath lispLibraries}:$LD_LIBRARY_PATH
            export C_INCLUDE_PATH=${pkgs.lib.makeSearchPathOutput "dev" "include" lispLibraries}:$C_INCLUDE_PATH
            export CPLUS_INCLUDE_PATH=$C_INCLUDE_PATH
            export CL_SOURCE_REGISTRY=$(pwd):$(pwd)/lisp:$CL_SOURCE_REGISTRY
            export QUICKLISP_HOME=''${QUICKLISP_HOME:-$HOME/quicklisp}
          '';
        };

        # =================================================================
        # PYTHON/PYTORCH TRAINING ENVIRONMENT
        # =================================================================

        pythonEnv = pkgs.python311.withPackages (ps: with ps; [
          torch
          torchvision
          torchaudio
          numpy
          scipy
          matplotlib
          tensorboard
          tqdm
          h5py
          pyyaml
          librosa
          soundfile
          ipython
          black
          pylint
        ]);

        # Training script wrapper
        trainAmpScript = pkgs.writeScriptBin "train-amp" ''
          #!${pkgs.bash}/bin/bash
          exec ${pythonEnv}/bin/python "$(pwd)/neural_amp_trainer.py" "$@"
        '';

        # Audio validation utility
        validateAudioScript = pkgs.writeScriptBin "validate-audio" ''
          #!${pythonEnv}/bin/python
          import sys
          import torchaudio
          from pathlib import Path
          
          def validate_audio(path):
              try:
                  info = torchaudio.info(path)
                  print(f"✓ {path}")
                  print(f"  Sample rate: {info.sample_rate}Hz")
                  print(f"  Channels: {info.num_channels}")
                  print(f"  Duration: {info.num_frames / info.sample_rate:.2f}s")
                  return True
              except Exception as e:
                  print(f"✗ {path}: {e}")
                  return False
          
          if len(sys.argv) < 2:
              print("Usage: validate-audio file.wav [file2.wav ...]")
              sys.exit(1)
          
          all_valid = all(validate_audio(f) for f in sys.argv[1:])
          sys.exit(0 if all_valid else 1)
        '';

        # Export to C++ header (for direct embedding)
        exportCppHeaderScript = pkgs.writeScriptBin "export-cpp-header" ''
          #!${pythonEnv}/bin/python
          import json
          import sys
          from pathlib import Path
          
          def json_to_header(json_path, header_path):
              with open(json_path) as f:
                  data = json.load(f)
              
              with open(header_path, 'w') as f:
                  f.write(f"// Auto-generated from {json_path}\n")
                  f.write("// Copyright 2025 DeMoD LLC\n")
                  f.write("#pragma once\n\n")
                  f.write("#include <cstddef>\n\n")
                  f.write("namespace ToneAssistant {\n\n")
                  
                  arch = data['architecture']
                  f.write(f"constexpr size_t INPUT_SIZE = {arch['input_size']};\n")
                  f.write(f"constexpr size_t HIDDEN_SIZE = {arch['hidden_size']};\n")
                  f.write(f"constexpr size_t NUM_LAYERS = {arch['num_layers']};\n")
                  f.write(f"constexpr size_t OUTPUT_SIZE = {arch['output_size']};\n\n")
                  
                  for key, values in data['weights'].items():
                      safe_name = key.replace('.', '_').replace('-', '_')
                      f.write(f"constexpr float {safe_name}[{len(values)}] = {{\n")
                      for i, v in enumerate(values):
                          f.write(f"  {v}f")
                          if i < len(values) - 1:
                              f.write(",")
                          if (i + 1) % 6 == 0:
                              f.write("\n")
                      f.write("\n};\n\n")
                  
                  f.write("} // namespace ToneAssistant\n")
              
              print(f"✓ C++ header exported to {header_path}")
          
          if len(sys.argv) != 3:
              print("Usage: export-cpp-header weights.json output.hpp")
              sys.exit(1)
          
          json_to_header(sys.argv[1], sys.argv[2])
        '';

        # Quick start guide
        quickstartScript = pkgs.writeScriptBin "tone-quickstart" ''
          #!${pkgs.bash}/bin/bash
          cat << 'EOF'
          ╔════════════════════════════════════════════════════════════════╗
          ║          🎸 TONE ASSISTANT - Neural Amp Modeling               ║
          ║                   ArchibaldOS Edition                          ║
          ╚════════════════════════════════════════════════════════════════╝
          
          WORKFLOW:
          ─────────
          1. RECORD TRAINING DATA
             • DI (direct input) guitar signal
             • Reamped signal through target amplifier
             • Export both as 48kHz mono/stereo WAV files
          
          2. VALIDATE AUDIO FILES
             $ validate-audio di_guitar.wav reamped_mesa.wav
          
          3. TRAIN NEURAL MODEL
             $ train-amp \
                 --input di_guitar.wav \
                 --target reamped_mesa.wav \
                 --out mesa_boogie_weights.json \
                 --epochs 200 \
                 --hidden-size 32 \
                 --device auto
          
          4. EXPORT FOR REAL-TIME USE
             a) JSON (for Lisp/Incudine with RTNeural CFFI):
                → Use mesa_boogie_weights.json directly
             
             b) C++ Header (for embedded/native code):
                $ export-cpp-header mesa_boogie_weights.json amp_model.hpp
          
          5. LOAD IN EMACS/INCUDINE
             M-x tone-assistant-start
             (in-package :scratch)
             (load-neural-amp-weights "mesa_boogie_weights.json")
             (neural-amp-process input-buffer output-buffer)
          
          ════════════════════════════════════════════════════════════════
          
          DOCUMENTATION:
            • Training:  python neural_amp_trainer.py --help
            • Emacs:     emacs --eval '(describe-function 'tone-assistant-start)'
            • Incudine:  https://github.com/titola/incudine
          
          EOF
        '';

        # =================================================================
        # EMACS CONFIGURATION PACKAGE
        # =================================================================

        toneAssistantEmacs = pkgs.writeTextFile {
          name = "tone-assistant.el";
          destination = "/share/emacs/site-lisp/tone-assistant.el";
          text = builtins.readFile ./emacs/tone-assistant.el;
        };

        emacsWithToneAssistant = pkgs.emacsWithPackages (epkgs: with epkgs; [
          slime
          slime-company
          paredit
          rainbow-delimiters
          faust-mode
          company
          magit
          which-key
        ]);

        # =================================================================
        # UTILITY SCRIPTS
        # =================================================================

        # Check JACK status
        jackStatusScript = pkgs.writeScriptBin "jack-status" ''
          #!${pkgs.bash}/bin/bash
          if command -v jack_lsp &>/dev/null; then
            if jack_lsp &>/dev/null; then
              echo "✓ JACK is running"
              echo ""
              echo "Active ports:"
              jack_lsp
            else
              echo "✗ JACK is not running"
              echo ""
              echo "Start JACK with:"
              echo "  jackd -R -d alsa -r 48000 -p 256"
            fi
          else
            echo "✗ JACK tools not found"
            echo "Install: nix-shell -p jack2"
          fi
        '';

        # Launch tone assistant environment
        launchToneAssistantScript = pkgs.writeScriptBin "tone-assistant-start" ''
          #!${pkgs.bash}/bin/bash
          
          # Check JACK first
          if ! command -v jack_lsp &>/dev/null || ! jack_lsp &>/dev/null; then
            echo "⚠ JACK is not running. Starting JACK server..."
            jackd -R -d alsa -r 48000 -p 256 &
            sleep 2
          fi
          
          # Launch Emacs with Tone Assistant
          exec ${emacsWithToneAssistant}/bin/emacs \
            --eval "(progn
                      (add-to-list 'load-path \"${toneAssistantEmacs}/share/emacs/site-lisp\")
                      (require 'tone-assistant)
                      (tone-assistant-start))"
        '';

        # =================================================================
        # COMPLETE PACKAGE
        # =================================================================

        toneAssistantPackage = pkgs.buildEnv {
          name = "tone-assistant";
          paths = [
            trainAmpScript
            validateAudioScript
            exportCppHeaderScript
            quickstartScript
            jackStatusScript
            launchToneAssistantScript
            emacsWithToneAssistant
            toneAssistantEmacs
            pythonEnv
          ] ++ lispBuildTools ++ lispLibraries;
        };

      in {
        # =================================================================
        # DEVELOPMENT SHELLS
        # =================================================================

        devShells = {
          # Full environment (Lisp + Python + Emacs)
          default = pkgs.mkShell {
            name = "tone-assistant-full";
            
            buildInputs = [
              pythonEnv
              emacsWithToneAssistant
              trainAmpScript
              validateAudioScript
              exportCppHeaderScript
              quickstartScript
              jackStatusScript
              launchToneAssistantScript
            ] ++ lispEnv.buildInputs;
            
            shellHook = ''
              ${lispEnv.shellHook}
              
              export PYTHONPATH="$(pwd):$(pwd)/python:$PYTHONPATH"
              export PYTHONDONTWRITEBYTECODE=1
              export TORCH_HOME="./.cache/torch"
              
              cat << 'EOF'
              ╔════════════════════════════════════════════════════════════════╗
              ║             🎸 TONE ASSISTANT - Development Shell              ║
              ║                   Neural Amp Modeling Suite                    ║
              ╚════════════════════════════════════════════════════════════════╝
              
              ENVIRONMENT:
                • SBCL:      $(sbcl --version 2>&1 | head -n1)
                • Python:    $(python --version)
                • PyTorch:   $(python -c 'import torch; print(torch.__version__)' 2>/dev/null || echo "N/A")
                • Emacs:     $(emacs --version | head -n1)
                • Faust:     $(faust --version 2>&1 | head -n1)
              
              COMMANDS:
                • tone-quickstart          - Show workflow guide
                • tone-assistant-start     - Launch Emacs environment
                • train-amp               - Train neural amp model
                • validate-audio          - Check audio files
                • export-cpp-header       - Export to C++ header
                • jack-status             - Check JACK server
              
              QUICKSTART:
                $ tone-quickstart
              
              ════════════════════════════════════════════════════════════════
              EOF
            '';
          };
          
          # Lisp-only environment
          lisp = pkgs.mkShell {
            name = "tone-assistant-lisp";
            buildInputs = lispEnv.buildInputs ++ [ jackStatusScript ];
            shellHook = ''
              ${lispEnv.shellHook}
              echo "🎼 Lisp/Incudine Environment Ready"
              echo "   SBCL: $(sbcl --version 2>&1 | head -n1)"
            '';
          };
          
          # Python-only environment
          python = pkgs.mkShell {
            name = "tone-assistant-python";
            buildInputs = [
              pythonEnv
              trainAmpScript
              validateAudioScript
              exportCppHeaderScript
              quickstartScript
            ];
            shellHook = ''
              export PYTHONPATH="$(pwd):$PYTHONPATH"
              export PYTHONDONTWRITEBYTECODE=1
              export TORCH_HOME="./.cache/torch"
              
              echo "🧠 Python Training Environment Ready"
              echo "   Python:  $(python --version)"
              echo "   PyTorch: $(python -c 'import torch; print(torch.__version__)' 2>/dev/null || echo 'N/A')"
              echo "   CUDA:    $(python -c 'import torch; print("Available" if torch.cuda.is_available() else "Not available")' 2>/dev/null || echo 'N/A')"
            '';
          };
        };

        # =================================================================
        # PACKAGES
        # =================================================================

        packages = {
          default = toneAssistantPackage;
          tone-assistant = toneAssistantPackage;
          train-amp = trainAmpScript;
          emacs-config = toneAssistantEmacs;
        };

        # =================================================================
        # APPS
        # =================================================================

        apps = {
          default = {
            type = "app";
            program = "${launchToneAssistantScript}/bin/tone-assistant-start";
          };
          
          train = {
            type = "app";
            program = "${trainAmpScript}/bin/train-amp";
          };
          
          validate = {
            type = "app";
            program = "${validateAudioScript}/bin/validate-audio";
          };
          
          quickstart = {
            type = "app";
            program = "${quickstartScript}/bin/tone-quickstart";
          };
          
          test = {
            type = "app";
            program = toString (pkgs.writeScript "test-tone-assistant" ''
              #!${pkgs.bash}/bin/bash
              set -e
              
              echo "═══════════════════════════════════════════════════════════"
              echo "Running Tone Assistant Tests"
              echo "═══════════════════════════════════════════════════════════"
              
              # Test SBCL
              echo ""
              echo "[1/5] Testing SBCL..."
              ${pkgs.sbcl}/bin/sbcl --version
              echo "✓ SBCL OK"
              
              # Test Python imports
              echo ""
              echo "[2/5] Testing Python environment..."
              ${pythonEnv}/bin/python -c "import torch, torchaudio, numpy; print('✓ Python imports OK')"
              
              # Test Faust
              echo ""
              echo "[3/5] Testing Faust compiler..."
              ${pkgs.faust}/bin/faust --version
              echo "✓ Faust OK"
              
              # Test Emacs
              echo ""
              echo "[4/5] Testing Emacs..."
              ${emacsWithToneAssistant}/bin/emacs --version | head -n1
              echo "✓ Emacs OK"
              
              # Test JACK tools
              echo ""
              echo "[5/5] Testing JACK tools..."
              if command -v jack_lsp &>/dev/null; then
                echo "✓ JACK tools found"
              else
                echo "⚠ JACK tools not in PATH (optional)"
              fi
              
              echo ""
              echo "═══════════════════════════════════════════════════════════"
              echo "✓ All tests passed"
              echo "═══════════════════════════════════════════════════════════"
            '');
          };
        };
      }
    ) // {
      # =================================================================
      # NIXOS MODULE (for integration with ArchibaldOS)
      # =================================================================

      nixosModules.tone-assistant = { config, lib, pkgs, ... }: {
        options.services.tone-assistant = {
          enable = lib.mkEnableOption "Tone Assistant Neural Amp Modeling";
          
          user = lib.mkOption {
            type = lib.types.str;
            default = "archibald";
            description = "User to run Tone Assistant as";
          };
          
          autoStartJack = lib.mkOption {
            type = lib.types.bool;
            default = true;
            description = "Automatically start JACK audio server";
          };
          
          quicklispDir = lib.mkOption {
            type = lib.types.str;
            default = "/home/${config.services.tone-assistant.user}/quicklisp";
            description = "Quicklisp installation directory";
          };
        };
        
        config = lib.mkIf config.services.tone-assistant.enable {
          # Install system packages
          environment.systemPackages = with pkgs; [
            (self.packages.${pkgs.system}.tone-assistant or self.packages.x86_64-linux.tone-assistant)
            jack2
            qjackctl
          ];
          
          # Audio group permissions
          users.users.${config.services.tone-assistant.user} = {
            extraGroups = [ "audio" "jackaudio" "realtime" ];
          };
          
          # Realtime permissions
          security.pam.loginLimits = [
            { domain = "@audio"; type = "-"; item = "rtprio"; value = "99"; }
            { domain = "@audio"; type = "-"; item = "memlock"; value = "unlimited"; }
            { domain = "@audio"; type = "-"; item = "nice"; value = "-19"; }
          ];
          
          # JACK service (optional)
          systemd.user.services.jackd = lib.mkIf config.services.tone-assistant.autoStartJack {
            description = "JACK Audio Connection Kit for Tone Assistant";
            after = [ "sound.target" ];
            serviceConfig = {
              Type = "simple";
              ExecStart = "${pkgs.jack2}/bin/jackd -R -d alsa -r 48000 -p 256";
              Restart = "on-failure";
              LimitRTPRIO = 95;
              LimitMEMLOCK = "infinity";
            };
            wantedBy = [ "default.target" ];
          };
          
          # Environment variables
          environment.variables = {
            QUICKLISP_HOME = config.services.tone-assistant.quicklispDir;
          };
        };
      };
    };
}
