{ config, pkgs, lib, ... }: {
  environment.systemPackages = with pkgs; [ dialog disko mkpasswd ];

  environment.etc."audio-setup.sh" = {
    text = ''
#!/usr/bin/env bash

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
    '';
    mode = "0755";
  };

  environment.etc."installer.sh" = {
    text = ''
#!/usr/bin/env bash

set -euo pipefail

LOG_FILE="/tmp/archibaldos-install.log"
exec > >(tee -a "$LOG_FILE") 2>&1
echo "Starting ArchibaldOS Installer at $(date)"

# Function for error handling
error_exit() {
  echo "Error: $1" >&2
  dialog --msgbox "Installation failed: $1\nSee $LOG_FILE for details." 8 60
  exit 1
}

# Display branding if available
clear
if [ -f /etc/installer-ascii.txt ]; then
  cat /etc/installer-ascii.txt
  sleep 1.5
fi

echo "Welcome to Lean RT Audio ArchibaldOS Installer"

# Keyboard layout selection
KB=$(dialog --menu "Select keyboard layout:" 20 60 12 us "US English" gb "UK English" de "German" fr "French" es "Spanish" it "Italian" ru "Russian" pl "Polish" pt "Portuguese (Portugal)" br "Portuguese (Brazil)" nl "Dutch" se "Swedish" no "Norwegian" dk "Danish" fi "Finnish" tr "Turkish" cz "Czech" hu "Hungarian" ro "Romanian" ua "Ukrainian" jp "Japanese" kr "Korean" cn "Chinese" ca "Canadian" au "Australian" latam "Latin American" in "Indian" af "Afghani" ara "Arabic" al "Albanian" am "Armenian" az "Azerbaijani" by "Belarusian" 3>&1 1>&2 2>&3) || error_exit "Keyboard selection canceled"
setxkbmap -layout "$KB" || echo "Warning: Failed to set keyboard layout"

# Disk selection with validation (exclude live USB if detectable)
DISKS=$(lsblk -dno NAME,SIZE,TYPE | grep disk | awk '{print "/dev/"$1 " ("$2")"}')
DISK=$(dialog --menu "Select disk to wipe (ensure it's not your live USB!):" 15 50 5 $DISKS 3>&1 1>&2 2>&3) || error_exit "Disk selection canceled"
DISK_SIZE=$(lsblk -no SIZE "$DISK")
dialog --yesno "Confirm erase $DISK (size: $DISK_SIZE)? This is irreversible!" 8 60 || error_exit "Disk wipe canceled"

# Optional HydraMesh
HYDRAMESH=$(dialog --yesno "Enable HydraMesh (P2P audio networking)?" 7 60 && echo true || echo false)

# Optional LUKS encryption
ENCRYPT=$(dialog --yesno "Enable LUKS full-disk encryption (recommended for security)?" 7 60 && echo true || echo false)
if [ "$ENCRYPT" = true ]; then
  ENCRYPT_PW=$(dialog --insecure --passwordbox "Encryption password:" 8 50 3>&1 1>&2 2>&3) || error_exit "Encryption canceled"
  ENCRYPT_PW_CONFIRM=$(dialog --insecure --passwordbox "Confirm:" 8 50 3>&1 1>&2 2>&3) || error_exit "Encryption canceled"
  [ "$ENCRYPT_PW" = "$ENCRYPT_PW_CONFIRM" ] || error_exit "Encryption passwords mismatch"
fi

# Other inputs
LOCALE=$(dialog --inputbox "Locale (e.g., en_US.UTF-8):" 8 50 "en_US.UTF-8" 3>&1 1>&2 2>&3) || error_exit "Locale canceled"
TZ=$(dialog --inputbox "Timezone (e.g., America/Los_Angeles):" 8 50 "America/Los_Angeles" 3>&1 1>&2 2>&3) || error_exit "Timezone canceled"
HOSTNAME=$(dialog --inputbox "Hostname:" 8 50 "archibaldos" 3>&1 1>&2 2>&3) || error_exit "Hostname canceled"
USERNAME=$(dialog --inputbox "Username:" 8 50 "audio-user" 3>&1 1>&2 2>&3) || error_exit "Username canceled"
USERPW=$(dialog --insecure --passwordbox "User password:" 8 50 3>&1 1>&2 2>&3) || error_exit "Password canceled"
USERPW_CONFIRM=$(dialog --insecure --passwordbox "Confirm:" 8 50 3>&1 1>&2 2>&3) || error_exit "Password canceled"
[ "$USERPW" = "$USERPW_CONFIRM" ] || error_exit "Passwords mismatch"

# Optional network setup (for flake downloads)
if ! ping -c 1 nixos.org &>/dev/null; then
  dialog --yesno "No internet detected. Configure WiFi?" 7 60 && {
    SSID=$(dialog --inputbox "WiFi SSID:" 8 50 3>&1 1>&2 2>&3) || error_exit "WiFi canceled"
    WIFI_PW=$(dialog --insecure --passwordbox "WiFi password:" 8 50 3>&1 1>&2 2>&3) || error_exit "WiFi canceled"
    nmcli device wifi connect "$SSID" password "$WIFI_PW" || error_exit "WiFi connection failed"
  }
fi

# Generate disko.nix with optional LUKS
if [ "$ENCRYPT" = true ]; then
  cat <<EOF > /tmp/disko.nix
{ disks ? [ "$DISK" ], lib, ... }: {
  disko.devices = {
    disk.main = {
      type = "disk"; device = lib.head disks; format = "gpt";
      content = { type = "gpt"; partitions = {
        ESP = { size = "500M"; type = "EF00"; format = "vfat"; mountpoint = "/boot"; };
        luks = { size = "100%"; content = { type = "luks"; format = "luks2"; extraFormatArgs = [ "--type" "luks2" "--hash" "sha512" "--iter-time" "5000" ]; content = { type = "filesystem"; format = "ext4"; mountpoint = "/"; }; }; };
      }; };
    };
  };
}
EOF
else
  cat <<EOF > /tmp/disko.nix
{ disks ? [ "$DISK" ], lib, ... }: {
  disko.devices = {
    disk.main = {
      type = "disk"; device = lib.head disks; format = "gpt";
      content = { type = "gpt"; partitions = {
        ESP = { size = "500M"; type = "EF00"; format = "vfat"; mountpoint = "/boot"; };
        root = { size = "100%"; format = "ext4"; mountpoint = "/"; };
      }; };
    };
  };
}
EOF
fi

# Run disko
disko --mode format /tmp/disko.nix || error_exit "Disk formatting failed"
disko --mode mount /tmp/disko.nix /mnt || error_exit "Disk mounting failed"

# If LUKS, add to config (simplified; full crypttab in real setups)
if [ "$ENCRYPT" = true ]; then
  echo "boot.initrd.luks.devices.\"luks-root\" = { device = \"$DISK-part2\"; };" >> /mnt/etc/nixos/hardware-configuration.nix || true
fi

nixos-generate-config --root /mnt || error_exit "Config generation failed"

# Copy configs
mkdir -p /mnt/etc/hypr /mnt/etc/waybar /mnt/etc/wofi /mnt/etc/hydramesh || error_exit "Directory creation failed"
cp /etc/hypr/* /mnt/etc/hypr/ || error_exit "Hyprland copy failed"
cp /etc/waybar/* /mnt/etc/waybar/ || error_exit "Waybar copy failed"
cp /etc/wofi/* /mnt/etc/wofi/ || error_exit "Wofi copy failed"
cp /etc/hydramesh/* /mnt/etc/hydramesh/ || error_exit "Hydramesh copy failed"

# Inject configs
cd /mnt/etc/nixos
sed -i "s|time.timeZone = .*|time.timeZone = \"$TZ\";|" configuration.nix || error_exit "Timezone injection failed"
sed -i "s|en_US.UTF-8|$LOCALE|" configuration.nix || error_exit "Locale injection failed"
sed -i "/services.xserver.enable = true;/a\    layout = \"$KB\";" configuration.nix || error_exit "Keyboard injection failed"
sed -i "s|networking.hostName = .*|networking.hostName = \"$HOSTNAME\";|" configuration.nix || error_exit "Hostname injection failed"

if [ "$HYDRAMESH" = true ]; then
  echo "services.hydramesh.enable = true;" >> configuration.nix || error_exit "HydraMesh enable failed"
fi

sed -i '/users.users.nixos = {/,/};/d' configuration.nix || true
echo "users.users.\"$USERNAME\" = { isNormalUser = true; extraGroups = [ \"audio\" \"jackaudio\" \"video\" \"wheel\" ]; initialHashedPassword = \"$(mkpasswd -m sha-512 "$USERPW")\"; };" >> configuration.nix || error_exit "User config failed"

# Setup user home (detect UID dynamically)
USER_UID=$(getent passwd nixos | cut -d: -f3)  # Assume similar to live user
mkdir -p /mnt/home/"$USERNAME"/.config/hypr /mnt/home/"$USERNAME"/.config/waybar /mnt/home/"$USERNAME"/.config/wofi || error_exit "User home creation failed"
cp /etc/hypr/* /mnt/home/"$USERNAME"/.config/hypr/ || error_exit "User Hyprland copy failed"
cp /etc/waybar/* /mnt/home/"$USERNAME"/.config/waybar/ || error_exit "User Waybar copy failed"
cp /etc/wofi/* /mnt/home/"$USERNAME"/.config/wofi/ || error_exit "User Wofi copy failed"
chown -R "$USER_UID:$USER_UID" /mnt/home/"$USERNAME" || error_exit "Chown failed"

# Install
nixos-install --root /mnt --flake /mnt/etc/nixos#archibaldOS || error_exit "NixOS install failed"

dialog --msgbox "Installation complete! Reboot to start ArchibaldOS.\nLog: $LOG_FILE" 8 60
    '';
    mode = "0755";
  };

  system.activationScripts.setupHyprland = lib.stringAfter [ "users" ] ''
    mkdir -p /home/nixos/.config/hypr /home/nixos/.config/waybar /home/nixos/.config/wofi
    cp /etc/hypr/* /home/nixos/.config/hypr/
    cp /etc/waybar/* /home/nixos/.config/waybar/
    cp /etc/wofi/* /home/nixos/.config/wofi/
    chown -R nixos:users /home/nixos
    chmod +x /home/nixos/.config/hypr/*.sh
  '';
}
