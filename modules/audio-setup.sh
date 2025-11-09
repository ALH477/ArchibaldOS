#!/usr/bin/env bash
# modules/audio-setup.sh

set -euo pipefail

LOG_FILE="/var/log/audio-setup.log"
exec > >(tee -a "$LOG_FILE") 2>&1
echo "Starting audio-setup.sh at $(date)"

DRY_RUN=false
if [[ "$1" == "--dry-run" ]]; then
  DRY_RUN=true
  echo "Running in dry-run mode"
fi

USER=""
PCI_ID=""

echo "Checking dependencies..."
for cmd in lspci lsusb setpci chrt cyclictest jack_iodelay amidi; do
  if ! command -v "$cmd" &> /dev/null; then
    echo "Warning: $cmd not found. Installing via nix-shell if needed."
  fi
done

if [ -z "$USER" ]; then
  read -p "Enter username: " USER
fi
if ! id "$USER" &>/dev/null; then
  echo "Error: User $USER does not exist."
  exit 1
fi

echo "Adding $USER to audio/realtime groups..."
if [ "$DRY_RUN" = false ]; then
  sudo usermod -aG audio,realtime "$USER" || echo "Failed to add groups; check sudo privileges."
else
  echo "[Dry-run] Would add $USER to audio/realtime."
fi
echo "Persist in configuration.nix: users.users.$USER.extraGroups = [ \"audio\" \"realtime\" ];"

echo "Applying latency tweaks..."
if [ "$DRY_RUN" = false ]; then
  sudo sysctl vm.swappiness=10 || true
  sudo sysctl fs.inotify.max_user_watches=600000 || true
  echo 2048 | sudo tee /sys/class/rtc/rtc0/max_user_freq || true
  echo 2048 | sudo tee /proc/sys/dev/hpet/max-user-freq || true
else
  echo "[Dry-run] Would apply sysctl tweaks."
fi
echo "Persist in configuration.nix: boot.kernel.sysctl = { \"vm.swappiness\" = 10; \"fs.inotify.max_user_watches\" = 600000; };"

echo "Detecting audio devices..."
lspci | grep -i audio
lsusb | grep -i audio
AUTO_PCI_ID=$(lspci | grep -i audio | awk '{print $1}' | head -1)
if [ -n "$AUTO_PCI_ID" ] && [ -z "$PCI_ID" ]; then
  PCI_ID="$AUTO_PCI_ID"
  echo "Auto-detected audio PCI ID: $PCI_ID"
fi
if [ ! -z "$PCI_ID" ] && [[ "$PCI_ID" =~ ^[0-9a-f]{2}:[0-9a-f]{2}\.[0-9a-f]$ ]]; then
  echo "Optimizing latency timer for $PCI_ID..."
  if [ "$DRY_RUN" = false ]; then
    sudo setpci -v -s "$PCI_ID" latency_timer=ff || echo "Failed to set latency timer."
  else
    echo "[Dry-run] Would set latency timer for $PCI_ID."
  fi
else
  echo "Skipping PCI optimization (invalid or empty ID)."
fi

echo "Pinning audio IRQs to CPU1..."
if [ "$DRY_RUN" = false ]; then
  AUDIO_IRQ=$(cat /proc/interrupts | grep -i snd | awk '{print $1}' | sed 's/:$//' | head -1)
  [ -n "$AUDIO_IRQ" ] && echo 2 | sudo tee /proc/irq/$AUDIO_IRQ/smp_affinity || echo "No audio IRQ found."
else
  echo "[Dry-run] Would pin audio IRQ to CPU1."
fi

echo "Setting RT priorities for audio apps..."
if [ "$DRY_RUN" = false ]; then
  for app in ardour qjackctl; do
    if pidof $app >/dev/null; then
      sudo chrt -f -p 80 $(pidof $app) || echo "Failed to set priority for $app."
    fi
  done
else
  echo "[Dry-run] Would set RT priorities for ardour, qjackctl."
fi

echo "Starting qjackctl..."
if [ "$DRY_RUN" = false ]; then
  qjackctl & || echo "Failed to start qjackctl; ensure it's installed."
else
  echo "[Dry-run] Would start qjackctl."
fi

echo "Testing JACK latency..."
if command -v jack_iodelay &>/dev/null; then
  jack_iodelay | grep "total roundtrip latency" || echo "Start JACK server first (via qjackctl)."
else
  echo "Install jack_iodelay: nix-shell -p jack2 --run jack_iodelay"
fi

echo "Testing kernel latency..."
if command -v cyclictest &>/dev/null; then
  cyclictest -l 100000 -m -n -p99 -q | grep -E "Max|Avg" || echo "Failed; ensure RT kernel."
else
  echo "Install cyclictest: nix-shell -p linuxPackages.stress-ng --run cyclictest"
fi

if command -v amidi &>/dev/null; then
  echo "Listing MIDI devices..."
  amidi -l || echo "No MIDI devices detected."
else
  echo "Install amidi: nix-shell -p alsa-utils --run amidi"
fi

if [ -f "/etc/hydramesh/config.json" ]; then
  echo "HydraMesh config found at /etc/hydramesh/config.json. Ensure 'peers' and 'port' are set for P2P."
else
  echo "Warning: /etc/hydramesh/config.json not found. Create with 'transport', 'host', 'port', 'mode'."
fi

echo "Current kernel: $(uname -r)"
echo "Setup complete! Reboot if needed. Log saved to $LOG_FILE."
echo "Persist specialisations: specialisation.lts-backup.configuration = { boot.kernelPackages = pkgs.linuxPackages_lts; };"
