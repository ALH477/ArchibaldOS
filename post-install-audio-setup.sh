#!/usr/bin/env bash

# ArchibaldOS Post-Install Audio Setup Script by DeMoD LLC
# For precision configuration: RT checks, latency tweaks, hardware setup, and kernel switching.
# Run on live boot or post-install for direct hardware optimization. Based on NixOS manuals.

echo "ArchibaldOS Audio Setup - DeMoD LLC"
echo "Sierpinski Triangle Trademark:"
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

# Detect if in live ISO (check for squashfs root)
if mount | grep -q '/nix/.ro-store type squashfs'; then
  IS_LIVE=true
  echo "Detected live ISO mode. Kernel switching requires installation; using default RT kernel."
else
  IS_LIVE=false
fi

# Check RT setup (default kernel)
echo "Running realtimeconfigquickscan..."
realtimeconfigquickscan --all

# Add user to audio/realtime groups (if not already)
read -p "Enter username: " USER
sudo usermod -aG audio,realtime $USER

# Latency tweaks (apply sysctl if needed)
sudo sysctl vm.swappiness=10
sudo sysctl fs.inotify.max_user_watches=600000
echo 2048 | sudo tee /sys/class/rtc/rtc0/max_user_freq
echo 2048 | sudo tee /proc/sys/dev/hpet/max-user-freq

# Detect and optimize audio hardware
echo "Detecting audio devices..."
lspci | grep -i audio
lsusb | grep -i audio
read -p "Enter sound card PCI ID (e.g., 00:1f.3) for optimization: " PCI_ID
if [ ! -z "$PCI_ID" ]; then
  sudo setpci -v -s $PCI_ID latency_timer=ff
fi

# Test JACK/PipeWire
echo "Starting qjackctl for manual config..."
qjackctl &

# Measure latency (requires JACK running)
echo "Run 'jack_iodelay' manually for RTD measurement."

# Kernel Switching (using NixOS specialisations; skip rebuild in live)
echo "Kernel Management:"
echo "Default: Real-Time (RT) kernel for low-latency audio."
echo "Backup: LTS kernel for stability (switch if RT issues occur)."
if [ "$IS_LIVE" = true ]; then
  echo "In live mode: Install first to enable switching. Test RT on hardware now."
else
  read -p "Switch to LTS backup kernel? (y/n): " SWITCH
  if [ "$SWITCH" = "y" ]; then
    sudo nixos-rebuild boot --specialisation lts-backup
    echo "LTS kernel set for next boot. Reboot to apply."
    echo "Diagnostics: Check module compatibility with 'lsmod' post-reboot."
  else
    echo "Staying on RT kernel. To switch later: sudo nixos-rebuild boot --specialisation lts-backup"
    echo "To switch back: sudo nixos-rebuild boot"
  fi
fi

# Safety Check: Verify kernel version post-switch (after reboot)
echo "Current kernel: $(uname -r)"
echo "If issues, boot into LTS from systemd-boot menu (sub-entry) after install."

echo "Setup complete! Reboot for changes. For issues, check /var/log/pipewire.log or NixOS manual on specialisations."
