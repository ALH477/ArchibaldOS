#!/usr/bin/env bash

set -euo pipefail  # Strict mode: exit on error, unset vars, pipe failures

# ArchibaldOS Post-Install Audio Setup Script by DeMoD LLC
# Version 1.1 - Improved with error handling, dependency checks, validation, and persistence.
# For RT checks, latency tweaks, hardware setup, and kernel switching.
# Run as non-root; uses sudo where needed. Based on NixOS manuals and Musnix docs.

LOG_FILE="$HOME/archibaldos-audio-setup.log"
exec > >(tee -a "$LOG_FILE") 2>&1  # Log all output

# Usage/help function
usage() {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  --help          Show this help message"
  echo "  --skip-ascii    Skip ASCII art display"
  echo "  --dry-run      Simulate actions without applying changes"
  echo "  --user <name>   Specify username (skips prompt)"
  echo "  --pci <id>      Specify PCI ID (skips prompt)"
  exit 0
}

# Parse options
DRY_RUN=false
SKIP_ASCII=false
USER=""
PCI_ID=""
while [[ $# -gt 0 ]]; do
  case $1 in
    --help) usage ;;
    --skip-ascii) SKIP_ASCII=true; shift ;;
    --dry-run) DRY_RUN=true; shift ;;
    --user) USER="$2"; shift 2 ;;
    --pci) PCI_ID="$2"; shift 2 ;;
    *) echo "Unknown option: $1"; usage ;;
  esac
done

# Display intro (optional ASCII)
echo "ArchibaldOS Audio Setup - DeMoD LLC (v1.1)"
if [ "$SKIP_ASCII" = false ]; then
  echo '                       *'
  echo '                      * *'
  echo '                     *   *'
  echo '                    * * * *'
  echo '                   *       *'
  echo '                  * *     * *'
  echo '                 *   *   *   *'
  echo '                * * * * * * * *'
  echo '               *               *'
  echo '              * *             * *'
  echo '             *   *           *   *'
  echo '            * * * *         * * * *'
  echo '           *       *       *       *'
  echo '          * *     * *     * *     * *'
  echo '         *   *   *   *   *   *   *   *'
  echo '        * * * * * * * * * * * * * * * *'
fi

# Detect live ISO
if mount | grep -q '/nix/.ro-store type squashfs'; then
  IS_LIVE=true
  echo "Live ISO detected. Some features (e.g., kernel switching) limited; install for full access."
else
  IS_LIVE=false
fi

# Check/install dependencies (transient via nix-shell if missing)
REQUIRED_PKGS=("realTimeConfigQuickScan" "qjackctl" "pciutils" "usbutils" "linuxPackages.stress-ng")  # Add stress-ng for cyclictest alternative
for pkg in "${REQUIRED_PKGS[@]}"; do
  if ! command -v $(nix-locate --top-level bin/$pkg | awk '{print $1}') &> /dev/null; then
    echo "Missing $pkg. Running in nix-shell..."
    nix-shell -p $pkg --command "$0 $@"  # Re-run script in shell with pkg
    exit $?
  fi
done
echo "All dependencies available."

# Prompt for user if not provided
if [ -z "$USER" ]; then
  read -p "Enter username: " USER
fi
if ! id "$USER" &>/dev/null; then
  echo "Error: User $USER does not exist."
  exit 1
fi

# Add user to audio/realtime groups
echo "Adding $USER to audio/realtime groups..."
if [ "$DRY_RUN" = false ]; then
  sudo usermod -aG audio,realtime "$USER" || echo "Failed to add groups; check sudo privileges."
else
  echo "[Dry-run] Would add $USER to audio/realtime."
fi
echo "Suggest adding to configuration.nix for persistence: users.users.$USER.extraGroups = [ \"audio\" \"realtime\" ];"

# Latency tweaks (sysctl; suggest Nix persistence)
echo "Applying latency tweaks..."
if [ "$DRY_RUN" = false ]; then
  sudo sysctl vm.swappiness=10 || true
  sudo sysctl fs.inotify.max_user_watches=600000 || true
  echo 2048 | sudo tee /sys/class/rtc/rtc0/max_user_freq || true
  echo 2048 | sudo tee /proc/sys/dev/hpet/max-user-freq || true
else
  echo "[Dry-run] Would apply sysctl tweaks."
fi
echo "For persistence, add to configuration.nix: boot.kernel.sysctl = { \"vm.swappiness\" = 10; \"fs.inotify.max_user_watches\" = 600000; };"

# Detect/optimize audio hardware
echo "Detecting audio devices..."
lspci | grep -i audio
lsusb | grep -i audio
if [ -z "$PCI_ID" ]; then
  read -p "Enter sound card PCI ID (e.g., 00:1f.3, leave blank to skip): " PCI_ID
fi
if [ ! -z "$PCI_ID" ] && [[ "$PCI_ID" =~ ^[0-9a-f]{2}:[0-9a-f]{2}\.[0-9a-f]$ ]]; then  # Basic validation
  echo "Optimizing latency timer for $PCI_ID..."
  if [ "$DRY_RUN" = false ]; then
    sudo setpci -v -s "$PCI_ID" latency_timer=ff || echo "Failed to set latency timer."
  else
    echo "[Dry-run] Would set latency timer for $PCI_ID."
  fi
else
  echo "Skipping PCI optimization (invalid or empty ID)."
fi

# Start qjackctl for config
echo "Starting qjackctl..."
if [ "$DRY_RUN" = false ]; then
  qjackctl & || echo "Failed to start qjackctl; ensure it's installed."
else
  echo "[Dry-run] Would start qjackctl."
fi

# Latency measurement
echo "For latency test: Ensure JACK is running, then run 'jack_iodelay' or 'stress-ng --cyclic 1 --cyclic-policy rtt' for benchmarks."

# Kernel Switching (using specialisations)
echo "Kernel Management: Default RT for low-latency; LTS backup for stability."
if [ "$IS_LIVE" = true ]; then
  echo "Live mode: Test RT with 'cyclictest -l 100000 -m -n -p99 -q' (install via nix-shell -p linuxPackages.stress-ng)."
else
  read -p "Switch to LTS backup kernel? (y/n): " SWITCH
  if [ "$SWITCH" = "y" ]; then
    if [ "$DRY_RUN" = false ]; then
      sudo nixos-rebuild boot --specialisation lts-backup || echo "Failed; ensure specialisation is defined in configuration.nix."
      echo "LTS set for next boot. Reboot to apply."
    else
      echo "[Dry-run] Would switch to LTS."
    fi
  else
    echo "Staying on RT. To switch: sudo nixos-rebuild boot --specialisation lts-backup"
  fi
fi

# Final verification
echo "Current kernel: $(uname -r)"
echo "Setup complete! Reboot if needed. Log saved to $LOG_FILE. For issues, check NixOS audio wiki or /var/log/pipewire.log."
echo "Tip: Add to configuration.nix for specialisations: specialisation.lts-backup.configuration = { boot.kernelPackages = pkgs.linuxPackages_lts; };"
