#!/bin/bash

# Improved Bash script for post-install setup on fresh Armbian Minimal Ubuntu (Jammy/Noble) for Orange Pi 5B
# Focus: Headless Guitarix with real-time audio
# Run as root on a CLI-only (minimal) image
# Improvements: User prompt for username, add threadirqs, file backups, package checks, IRQ affinity for low-latency audio,
#               better error handling, modular structure, specific RT kernel guidance with links, reboot prompt.
# RT kernel: Manual install from prebuilts (e.g., 6.1.75-rt from Armbian forums). Prefer kernels >=5.15 for USB audio.

set -e  # Exit on error

if [ "$EUID" -ne 0 ]; then
  echo "Run as root: sudo $0"
  exit 1
fi

# Function to check if package is available
check_package() {
  if ! apt-cache show "$1" >/dev/null 2>&1; then
    echo "Warning: Package $1 not found in repos. Skipping or install manually."
    return 1
  fi
  return 0
}

# Function to backup and append to file
backup_and_append() {
  local file="$1"
  local content="$2"
  if [ -f "$file" ]; then
    cp "$file" "$file.bak" || { echo "Error: Failed to backup $file"; exit 1; }
  fi
  if ! grep -q "$content" "$file"; then
    echo "$content" >> "$file" || { echo "Error: Failed to append to $file"; exit 1; }
  fi
}

# Update and upgrade system
echo "Updating system..."
apt update && apt upgrade -y

# Install audio and RT tools (with checks)
echo "Installing JACK, Guitarix, and RT tools..."
PACKAGES="jackd2 libjack-jackd2-dev guitarix alsa-utils rt-tests rtirq-init"
for pkg in $PACKAGES; do
  if check_package "$pkg"; then
    apt install -y "$pkg"
  fi
done

# If guitarix isn't available, provide build instructions (uncomment to enable auto-build)
# if ! check_package "guitarix"; then
#   echo "Building Guitarix from source..."
#   apt install -y git build-essential libgtk2.0-dev libsndfile1-dev libcurl4-openssl-dev
#   git clone https://github.com/brummer10/guitarix.git
#   cd guitarix
#   ./waf configure --prefix=/usr --enable-lv2 --ladspa
#   ./waf build
#   sudo ./waf install
#   cd ..
# fi

# Prompt for username and add to audio group
read -p "Enter your username (default: armbian): " USERNAME
USERNAME=${USERNAME:-armbian}
if id "$USERNAME" >/dev/null 2>&1; then
  usermod -aG audio "$USERNAME" || { echo "Error: Failed to add $USERNAME to audio group"; exit 1; }
  echo "Added $USERNAME to audio group."
else
  echo "Error: User $USERNAME does not exist. Skipping."
fi

# Configure real-time priorities in /etc/security/limits.conf
echo "Setting RT priorities..."
backup_and_append "/etc/security/limits.conf" "@audio   -  rtprio     95"
backup_and_append "/etc/security/limits.conf" "@audio   -  memlock    unlimited"
backup_and_append "/etc/security/limits.conf" "@audio   -  nice       -19"

# Optimize kernel params for audio (isolcpus and threadirqs for RK3588S)
echo "Configuring boot parameters..."
backup_and_append "/boot/armbianEnv.txt" "isolcpus=5,7"
backup_and_append "/boot/armbianEnv.txt" "extraargs=threadirqs"

# Set Ethernet IRQ affinity for low-latency (adjust IRQs based on /proc/interrupts)
echo "Setting Ethernet IRQ affinity in /etc/rc.local..."
if [ ! -f /etc/rc.local ]; then
  echo "#!/bin/sh -e" > /etc/rc.local
  chmod +x /etc/rc.local
fi
backup_and_append "/etc/rc.local" "sleep 10"
backup_and_append "/etc/rc.local" "echo 20 > /proc/irq/81/smp_affinity"  # Example for IRQ 81 (Ethernet); check /proc/interrupts
backup_and_append "/etc/rc.local" "echo 20 > /proc/irq/82/smp_affinity"  # Example for IRQ 82
backup_and_append "/etc/rc.local" "exit 0"

# Enable and start rtirq for audio IRQ prioritization
echo "Setting up rtirq..."
if check_package "rtirq-init"; then
  systemctl enable rtirq
  systemctl start rtirq
fi

# Manual RT kernel installation guidance
echo "Manual step: Download RT kernel .deb for better USB audio support (kernels >=5.15 recommended)."
echo "Sources: https://mega.nz/folder/znBVHJ4Z#69huCvInrf3tqc8I4QR-lQ or https://drive.google.com/drive/folders/1JglCIPKvyFwviAEmfyAsL1SmroZcjOvr"
echo "Example: wget [link to deb] && sudo dpkg -i linux-image-*-rt-arm64.deb && sudo update-initramfs -u"
echo "Warning: 5.10 kernels may have USB audio issues. After install, reboot and verify with uname -r."

# Test instructions
echo "Setup complete. Reboot to apply changes? (y/n)"
read -p "" REBOOT
if [ "$REBOOT" = "y" ]; then
  reboot
fi

echo "After reboot, test with:"
echo "1. Verify RT: cat /sys/kernel/realtime (should output 1)"
echo "2. Check audio devices: aplay -l"
echo "3. Start JACK: jackd -d alsa -d hw:0 -r 48000 -p 128 -n 2 &  # Adjust hw:0 as needed"
echo "4. Run Guitarix headless: guitarix --nogui -i alsa -o jack"
echo "5. Check latency: sudo cyclictest -m -n -p 99 -t 4 (aim for <50µs max)"
echo "For issues, check Armbian forums: https://forum.armbian.com/topic/28559-realtime-kernel-for-orange-pi-5/"
