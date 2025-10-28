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

echo "Welcome to Lean RT Audio ArchibaldOS Installer"
KB=$(dialog --menu "Select keyboard layout:" 20 60 12 us "US English" gb "UK English" de "German" fr "French" es "Spanish" it "Italian" ru "Russian" pl "Polish" pt "Portuguese (Portugal)" br "Portuguese (Brazil)" nl "Dutch" se "Swedish" no "Norwegian" dk "Danish" fi "Finnish" tr "Turkish" cz "Czech" hu "Hungarian" ro "Romanian" ua "Ukrainian" jp "Japanese" kr "Korean" cn "Chinese" ca "Canadian" au "Australian" latam "Latin American" in "Indian" af "Afghani" ara "Arabic" al "Albanian" am "Armenian" az "Azerbaijani" by "Belarusian" 3>&1 1>&2 2>&3)
setxkbmap -layout "$KB" || echo "Failed to set layout"
DISKS=$(lsblk -dno NAME,SIZE,TYPE | grep disk | awk '{print "/dev/"$1 " ("$2")"}')
DISK=$(dialog --menu "Select disk to wipe:" 15 50 5 $DISKS 3>&1 1>&2 2>&3)
dialog --yesno "WARNING: Erase $DISK?" 7 60 || exit
HYDRAMESH=$(dialog --yesno "Enable HydraMesh?" 7 60 && echo true || echo false)
LOCALE=$(dialog --inputbox "Locale (e.g., en_US.UTF-8):" 8 50 "en_US.UTF-8" 3>&1 1>&2 2>&3)
TZ=$(dialog --inputbox "Timezone (e.g., America/Los_Angeles):" 8 50 "America/Los_Angeles" 3>&1 1>&2 2>&3)
HOSTNAME=$(dialog --inputbox "Hostname:" 8 50 "archibaldos" 3>&1 1>&2 2>&3)
USERNAME=$(dialog --inputbox "Username:" 8 50 "audio-user" 3>&1 1>&2 2>&3)
USERPW=$(dialog --insecure --passwordbox "User password:" 8 50 3>&1 1>&2 2>&3)
USERPW_CONFIRM=$(dialog --insecure --passwordbox "Confirm:" 8 50 3>&1 1>&2 2>&3)
[ "$USERPW" = "$USERPW_CONFIRM" ] || { dialog --msgbox "Mismatch" 7 50; exit 1; }

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

disko --mode format /tmp/disko.nix
disko --mode mount /tmp/disko.nix /mnt
nixos-generate-config --root /mnt

mkdir -p /mnt/etc/hypr /mnt/etc/waybar /mnt/etc/wofi /mnt/etc/hydramesh
cp /etc/hypr/* /mnt/etc/hypr/
cp /etc/waybar/* /mnt/etc/waybar/
cp /etc/wofi/* /mnt/etc/wofi/
cp /etc/hydramesh/* /mnt/etc/hydramesh/

cd /mnt/etc/nixos
sed -i "s|time.timeZone = .*|time.timeZone = \"$TZ\";|" configuration.nix || true
sed -i "s|en_US.UTF-8|$LOCALE|" configuration.nix || true
sed -i "/services.xserver.enable = true;/a\    layout = \"$KB\";" configuration.nix || true
sed -i "s|networking.hostName = .*|networking.hostName = \"$HOSTNAME\";|" configuration.nix || true

if [ "$HYDRAMESH" = "true" ]; then
  echo "services.hydramesh.enable = true;" >> configuration.nix
fi

sed -i '/users.users.nixos = {/,/};/d' configuration.nix || true
echo "users.users.$USERNAME = { isNormalUser = true; extraGroups = [ \"audio\" \"jackaudio\" \"video\" \"wheel\" ]; initialHashedPassword = \"$(mkpasswd -m sha-512 "$USERPW")\"; };" >> configuration.nix

mkdir -p /mnt/home/"$USERNAME"/.config/hypr /mnt/home/"$USERNAME"/.config/waybar /mnt/home/"$USERNAME"/.config/wofi
cp /etc/hypr/* /mnt/home/"$USERNAME"/.config/hypr/
cp /etc/waybar/* /mnt/home/"$USERNAME"/.config/waybar/
cp /etc/wofi/* /mnt/home/"$USERNAME"/.config/wofi/
chown -R 1000:100 /mnt/home/"$USERNAME"

nixos-install --root /mnt --flake /mnt/etc/nixos#archibaldOS
dialog --msgbox "Installation complete. Reboot." 7 50
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
