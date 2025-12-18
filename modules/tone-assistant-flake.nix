# tone-assistant.nix
# Copyright 2025 DeMoD LLC
# SPDX-License-Identifier: BSD-3-Clause
#
# Tone Assistant: Neural amp modeling module for ArchibaldOS
# Production-grade, fully self-contained module with embedded Emacs config

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
        # EMBEDDED EMACS CONFIGURATION (Production Grade)
        # =================================================================
        
        emacsConfigText = ''
;;; tone-assistant.el --- Tone Assistant Development Environment
;; Copyright 2025 DeMoD LLC
;; Version: 2.0.0
;; Package-Requires: ((emacs "26.1") (slime "2.26") (company "0.9"))
;; Keywords: audio, lisp, dsp, incudine, faust
;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:
;;
;; Production-grade Emacs environment for neural amplifier modeling.
;; Integrates Common Lisp (Incudine), Faust DSP, and PyTorch training.
;;
;; Key Features:
;;   - Automatic Quicklisp bootstrap and management
;;   - JACK audio integration with status monitoring
;;   - Faust DSP compilation and visualization
;;   - Structural Lisp editing with Paredit
;;   - SLIME integration for real-time REPL interaction

;;; Code:

(require 'url)

;; ============================================================================
;; CUSTOMIZATION
;; ============================================================================

(defgroup tone-assistant nil
  "Configuration for Tone Assistant audio development environment."
  :group 'applications
  :prefix "tone-assistant-")

(defcustom tone-assistant-quicklisp-dir
  (expand-file-name "~/quicklisp/")
  "Directory where Quicklisp is installed."
  :type 'directory
  :group 'tone-assistant)

(defcustom tone-assistant-quicklisp-url
  "https://beta.quicklisp.org/quicklisp.lisp"
  "URL for Quicklisp installer download."
  :type 'string
  :group 'tone-assistant)

(defcustom tone-assistant-sbcl-program "sbcl"
  "Path to SBCL executable."
  :type 'string
  :group 'tone-assistant)

(defcustom tone-assistant-faust-program "faust"
  "Path to Faust compiler executable."
  :type 'string
  :group 'tone-assistant)

(defcustom tone-assistant-check-jack-on-start t
  "Check if JACK is running when starting the environment."
  :type 'boolean
  :group 'tone-assistant)

(defcustom tone-assistant-auto-load-libraries t
  "Automatically load Incudine and CL-JACK on SLIME connection."
  :type 'boolean
  :group 'tone-assistant)

(defcustom tone-assistant-jack-sample-rate 48000
  "Default JACK sample rate in Hz."
  :type 'integer
  :group 'tone-assistant)

(defcustom tone-assistant-jack-buffer-size 256
  "Default JACK buffer size in frames."
  :type 'integer
  :group 'tone-assistant)

;; ============================================================================
;; SLIME CONFIGURATION
;; ============================================================================

(setq inferior-lisp-program tone-assistant-sbcl-program)

;; Configure SLIME contribs (packages managed by Nix, not package.el)
(setq slime-contribs '(slime-fancy slime-asdf slime-quicklisp slime-company))

;; Initialize SLIME with contribs
(with-eval-after-load 'slime
  (slime-setup slime-contribs))

;; Better SLIME defaults
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
(setq slime-startup-animation nil)
(setq slime-enable-evaluate-in-emacs t)
(setq slime-load-failed-fasl 'always)

;; Prevent SLIME from trying to load contribs that don't exist
(setq slime-setup-contribs nil)

;; ============================================================================
;; QUICKLISP MANAGEMENT
;; ============================================================================

(defun tone-assistant--quicklisp-installed-p ()
  "Check if Quicklisp is installed."
  (file-exists-p (expand-file-name "setup.lisp" tone-assistant-quicklisp-dir)))

(defun tone-assistant--install-quicklisp ()
  "Install Quicklisp if not already present."
  (unless (tone-assistant--quicklisp-installed-p)
    (message "Tone Assistant: Quicklisp not found, installing...")
    (let ((ql-installer (make-temp-file "quicklisp" nil ".lisp")))
      (condition-case err
          (progn
            ;; Download installer
            (url-copy-file tone-assistant-quicklisp-url ql-installer t)
            
            ;; Run installation
            (let* ((install-dir (directory-file-name tone-assistant-quicklisp-dir))
                   (cmd (format "%s --no-userinit --no-sysinit --non-interactive --load %s --eval %s --quit"
                                tone-assistant-sbcl-program
                                (shell-quote-argument ql-installer)
                                (shell-quote-argument 
                                 (format "(quicklisp-quickstart:install :path \"%s\")" install-dir))))
                   (exit-code (call-process-shell-command cmd nil "*Quicklisp Install*")))
              
              (if (and (= exit-code 0) (tone-assistant--quicklisp-installed-p))
                  (message "Tone Assistant: Quicklisp installed successfully âœ“")
                (progn
                  (pop-to-buffer "*Quicklisp Install*")
                  (error "Quicklisp installation failed. Check *Quicklisp Install* buffer"))))
            
            ;; Clean up
            (when (file-exists-p ql-installer)
              (delete-file ql-installer)))
        
        (error
         (message "Tone Assistant: Error installing Quicklisp: %s" (error-message-string err))
         (when (file-exists-p ql-installer)
           (delete-file ql-installer))
         nil)))))

(defun tone-assistant--load-quicklisp ()
  "Load Quicklisp setup.lisp if available."
  (let ((ql-setup (expand-file-name "setup.lisp" tone-assistant-quicklisp-dir)))
    (when (file-exists-p ql-setup)
      (condition-case err
          (progn
            (load ql-setup)
            (message "Tone Assistant: Quicklisp loaded âœ“"))
        (error
         (message "Tone Assistant: Error loading Quicklisp: %s" (error-message-string err))
         nil)))))

;; Bootstrap Quicklisp on load
(tone-assistant--install-quicklisp)
(tone-assistant--load-quicklisp)

;; ============================================================================
;; JACK AUDIO UTILITIES
;; ============================================================================

(defun tone-assistant-check-jack ()
  "Check if JACK Audio Connection Kit is running."
  (interactive)
  (if (executable-find "jack_lsp")
      (if (= 0 (call-process "jack_lsp" nil nil nil))
          (message "JACK is running âœ“")
        (message "JACK is not running. Start with: jackd -R -d alsa -r %d -p %d"
                 tone-assistant-jack-sample-rate
                 tone-assistant-jack-buffer-size))
    (message "JACK tools not found. Ensure JACK is installed in your system")))

(defun tone-assistant-start-jack ()
  "Start JACK audio server with configured parameters."
  (interactive)
  (if (executable-find "jackd")
      (let ((cmd (format "jackd -R -d alsa -r %d -p %d &"
                         tone-assistant-jack-sample-rate
                         tone-assistant-jack-buffer-size)))
        (async-shell-command cmd "*JACK*")
        (message "Starting JACK... (check *JACK* buffer for output)"))
    (error "jackd executable not found")))

(defun tone-assistant-stop-jack ()
  "Stop JACK audio server."
  (interactive)
  (if (= 0 (call-process "pkill" nil nil nil "jackd"))
      (message "JACK stopped")
    (message "JACK was not running or could not be stopped")))

(defun tone-assistant-list-jack-ports ()
  "List all JACK audio ports."
  (interactive)
  (if (executable-find "jack_lsp")
      (let ((ports (shell-command-to-string "jack_lsp")))
        (with-output-to-temp-buffer "*JACK Ports*"
          (princ "=== JACK Audio Ports ===\n\n")
          (princ ports)))
    (message "JACK tools not installed")))

(defun tone-assistant-jack-connections ()
  "Show JACK audio connections."
  (interactive)
  (if (executable-find "jack_lsp")
      (let ((connections (shell-command-to-string "jack_lsp -c")))
        (with-output-to-temp-buffer "*JACK Connections*"
          (princ "=== JACK Audio Connections ===\n\n")
          (princ connections)))
    (message "JACK tools not installed")))

;; ============================================================================
;; FAUST DSP UTILITIES
;; ============================================================================

(defun tone-assistant-compile-faust ()
  "Compile current Faust (.dsp) file to C++."
  (interactive)
  (unless (executable-find tone-assistant-faust-program)
    (error "Faust compiler not found at: %s" tone-assistant-faust-program))
  
  (let* ((input-file (buffer-file-name))
         (output-file (concat (file-name-sans-extension input-file) ".cpp")))
    
    (unless input-file
      (error "Buffer is not visiting a file"))
    
    (unless (string-suffix-p ".dsp" input-file)
      (error "Not a Faust file (.dsp)"))
    
    (save-buffer)
    
    ;; Use compile for proper error handling
    (compile (format "%s %s -o %s" 
                     tone-assistant-faust-program
                     (shell-quote-argument input-file)
                     (shell-quote-argument output-file)))))

(defun tone-assistant-faust-to-svg ()
  "Generate SVG diagram of current Faust DSP."
  (interactive)
  (unless (executable-find tone-assistant-faust-program)
    (error "Faust compiler not found at: %s" tone-assistant-faust-program))
  
  (let* ((input-file (buffer-file-name))
         (output-dir (file-name-directory input-file)))
    
    (unless input-file
      (error "Buffer is not visiting a file"))
    
    (unless (string-suffix-p ".dsp" input-file)
      (error "Not a Faust file (.dsp)"))
    
    (save-buffer)
    
    (let ((exit-code (call-process tone-assistant-faust-program nil "*Faust SVG*" nil
                                   input-file "-svg" "-o" "/dev/null")))
      (if (= exit-code 0)
          (message "SVG diagrams generated in: %s" output-dir)
        (progn
          (pop-to-buffer "*Faust SVG*")
          (error "Faust SVG generation failed. Check *Faust SVG* buffer"))))))

(defun tone-assistant-faust-diagram-view ()
  "Generate and view SVG diagram of current Faust DSP."
  (interactive)
  (tone-assistant-faust-to-svg)
  (let* ((input-file (buffer-file-name))
         (svg-file (concat (file-name-sans-extension input-file) "-svg/process.svg")))
    (when (file-exists-p svg-file)
      (find-file-other-window svg-file))))

;; ============================================================================
;; MODE HOOKS
;; ============================================================================

(defun tone-assistant--setup-lisp-mode ()
  "Configure Lisp editing environment."
  ;; Structural editing
  (when (fboundp 'paredit-mode)
    (paredit-mode 1))
  
  ;; Visual aids
  (when (fboundp 'rainbow-delimiters-mode)
    (rainbow-delimiters-mode 1))
  
  ;; Completion
  (when (fboundp 'company-mode)
    (company-mode 1))
  
  ;; Key discovery
  (when (fboundp 'which-key-mode)
    (which-key-mode 1))
  
  ;; Keybindings
  (local-set-key (kbd "C-c C-d") 'slime-describe-symbol)
  (local-set-key (kbd "C-c C-t") 'slime-toggle-trace-fdefinition)
  (local-set-key (kbd "C-c M-d") 'slime-disassemble-symbol)
  (local-set-key (kbd "C-c C-k") 'slime-compile-and-load-file))

(defun tone-assistant--setup-faust-mode ()
  "Configure Faust DSP editing environment."
  ;; Completion
  (when (fboundp 'company-mode)
    (company-mode 1))
  
  ;; Indentation
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  
  ;; Keybindings
  (local-set-key (kbd "C-c C-c") 'tone-assistant-compile-faust)
  (local-set-key (kbd "C-c C-g") 'tone-assistant-faust-to-svg)
  (local-set-key (kbd "C-c C-v") 'tone-assistant-faust-diagram-view))

;; Apply hooks
(add-hook 'lisp-mode-hook #'tone-assistant--setup-lisp-mode)
(add-hook 'slime-repl-mode-hook #'tone-assistant--setup-lisp-mode)

;; Faust mode - check if it's available first
(with-eval-after-load 'faust-mode
  (add-hook 'faust-mode-hook #'tone-assistant--setup-faust-mode))

;; ============================================================================
;; MAIN ENTRY POINTS
;; ============================================================================

;;;###autoload
(defun tone-assistant-start ()
  "Initialize the Tone Assistant audio development environment."
  (interactive)
  
  ;; Check JACK if configured
  (when tone-assistant-check-jack-on-start
    (tone-assistant-check-jack))
  
  ;; Start SLIME
  (message "Tone Assistant: Starting SLIME...")
  (slime)
  
  ;; Load audio libraries when SLIME connects (if configured)
  (when tone-assistant-auto-load-libraries
    (add-hook 'slime-connected-hook
              #'tone-assistant--load-audio-libraries
              nil t)))

(defun tone-assistant--load-audio-libraries ()
  "Load Incudine and related audio libraries via Quicklisp."
  (message "Tone Assistant: Loading audio libraries...")
  
  ;; Load Incudine
  (slime-eval-async
   '(cl:handler-case
        (cl:progn
          (ql:quickload :incudine :silent t)
          "âœ“ Incudine loaded")
      (cl:error (e)
        (cl:format nil "âœ— Error loading Incudine: ~A" e)))
   (lambda (result)
     (message "Incudine: %s" result)))
  
  ;; Load CL-JACK
  (slime-eval-async
   '(cl:handler-case
        (cl:progn
          (ql:quickload :cl-jack :silent t)
          "âœ“ CL-JACK loaded")
      (cl:error (e)
        (cl:format nil "âœ— Error loading CL-JACK: ~A" e)))
   (lambda (result)
     (message "CL-JACK: %s" result)))
  
  (message "Tone Assistant: Environment ready âœ“"))

;;;###autoload
(defun tone-assistant-restart ()
  "Restart the Tone Assistant environment."
  (interactive)
  (when (and (fboundp 'slime-connected-p) (slime-connected-p))
    (slime-quit-lisp)
    (sleep-for 1))
  (tone-assistant-start))

;;;###autoload
(defun tone-assistant-status ()
  "Display the status of the Tone Assistant environment."
  (interactive)
  (with-output-to-temp-buffer "*Tone Assistant Status*"
    (princ "â•”â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•—\n")
    (princ "â•‘              TONE ASSISTANT - System Status                   â•‘\n")
    (princ "â•šâ•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•\n\n")
    
    ;; SLIME status
    (princ (format "SLIME:      %s\n" 
                   (if (and (fboundp 'slime-connected-p) (slime-connected-p))
                       "Connected âœ“"
                     "Not connected")))
    
    ;; SBCL status
    (princ (format "SBCL:       %s\n"
                   (if (executable-find tone-assistant-sbcl-program)
                       (format "Found at %s âœ“" tone-assistant-sbcl-program)
                     "Not found âœ—")))
    
    ;; Quicklisp status
    (princ (format "Quicklisp:  %s\n"
                   (if (tone-assistant--quicklisp-installed-p)
                       (format "Installed at %s âœ“" tone-assistant-quicklisp-dir)
                     "Not installed âœ—")))
    
    ;; JACK status
    (princ (format "JACK:       %s\n"
                   (if (and (executable-find "jack_lsp")
                            (= 0 (call-process "jack_lsp" nil nil nil)))
                       (format "Running âœ“ (SR: %dHz, Buffer: %d)"
                               tone-assistant-jack-sample-rate
                               tone-assistant-jack-buffer-size)
                     "Not running")))
    
    ;; Faust status
    (princ (format "Faust:      %s\n"
                   (if (executable-find tone-assistant-faust-program)
                       (format "Found at %s âœ“" tone-assistant-faust-program)
                     "Not found (optional)")))
    
    (princ "\n")
    (princ "â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€\n")
    (princ "Commands: M-x tone-assistant- TAB (for completion)\n")))

;;;###autoload
(defun tone-assistant-new-dsp-file (filename)
  "Create a new Faust DSP file with a basic template."
  (interactive "FNew DSP file: ")
  (unless (string-suffix-p ".dsp" filename)
    (setq filename (concat filename ".dsp")))
  
  (find-file filename)
  (insert "// Faust DSP - " (file-name-nondirectory filename) "
// Copyright 2025 DeMoD LLC

import(\"stdfaust.lib\");

// Parameters
freq = hslider(\"Frequency[unit:Hz]\", 440, 20, 20000, 1);
gain = hslider(\"Gain[unit:dB]\", -10, -60, 0, 0.1) : ba.db2linear;

// DSP Definition
process = os.osc(freq) * gain;
")
  (goto-char (point-min))
  (search-forward "process = ")
  (goto-char (match-end 0)))

;;;###autoload
(defun tone-assistant-new-incudine-file (filename)
  "Create a new Incudine Lisp file with a basic template."
  (interactive "FNew Incudine file: ")
  (unless (string-suffix-p ".lisp" filename)
    (setq filename (concat filename ".lisp")))
  
  (find-file filename)
  (insert ";;;; Incudine Real-Time Audio - " (file-name-nondirectory filename) "
;;;; Copyright 2025 DeMoD LLC

(in-package :scratch)

;;; DSP Definition
(dsp! simple-sine ((freq float) (amp float))
  \"Simple sine wave oscillator.\"
  (stereo (* amp (sine freq 0.5))))

;;; Usage:
;;;   (simple-sine 440 0.3)
;;;   (free 0)  ; Stop all DSPs
")
  (goto-char (point-min))
  (search-forward "(dsp! ")
  (end-of-line))

;; ============================================================================
;; MENU INTEGRATION
;; ============================================================================

(when (fboundp 'easy-menu-define)
  (easy-menu-define tone-assistant-menu nil
    "Menu for Tone Assistant"
    '("Tone Assistant"
      ["Start Environment" tone-assistant-start
       :help "Initialize the audio development environment"
       :visible (not (and (fboundp 'slime-connected-p) (slime-connected-p)))]
      ["Restart Environment" tone-assistant-restart
       :help "Restart SLIME and reload libraries"
       :visible (and (fboundp 'slime-connected-p) (slime-connected-p))]
      ["Show Status" tone-assistant-status
       :help "Display environment status"]
      "---"
      ("JACK Audio"
       ["Check Status" tone-assistant-check-jack
        :help "Check if JACK is running"]
       ["Start JACK" tone-assistant-start-jack
        :help "Start JACK audio server"]
       ["Stop JACK" tone-assistant-stop-jack
        :help "Stop JACK audio server"]
       ["List Ports" tone-assistant-list-jack-ports
        :help "List all JACK audio ports"]
       ["Show Connections" tone-assistant-jack-connections
        :help "Show JACK audio connections"])
      "---"
      ("File Templates"
       ["New DSP File" tone-assistant-new-dsp-file
        :help "Create a new Faust DSP file"]
       ["New Incudine File" tone-assistant-new-incudine-file
        :help "Create a new Incudine Lisp file"])
      "---"
      ["Customize" (customize-group 'tone-assistant)
       :help "Customize Tone Assistant settings"]))
  
  (easy-menu-add-item nil '("Tools") tone-assistant-menu))

;; ============================================================================
;; PROVIDE
;; ============================================================================

(provide 'tone-assistant)

;;; tone-assistant.el ends here
        '';

  
