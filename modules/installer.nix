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
# ArchibaldOS Production Installer
# Hardened, user-friendly, and production-ready

set -euo pipefail
IFS=$'\n\t'

# ============================================================================
# Configuration & Setup
# ============================================================================

readonly SCRIPT_VERSION="1.0.0"
readonly LOG_FILE="/tmp/archibaldos-install-$(date +%s).log"
readonly STATE_DIR="/tmp/archibaldos-state"
readonly INSTALL_ROOT="/mnt"

# ANSI color codes
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# Global state
declare -A CONFIG=(
  [keyboard]=""
  [disk]=""
  [encrypt]="false"
  [encrypt_pw]=""
  [locale]="en_US.UTF-8"
  [timezone]="UTC"
  [hostname]="archibaldos"
  [username]="audio-user"
  [userpw]=""
  [hydramesh]="false"
  [wifi_ssid]=""
  [wifi_pw]=""
  [has_internet]="false"
  [luks_name]="cryptroot"
)

# ============================================================================
# Logging & Error Handling
# ============================================================================

setup_logging() {
  mkdir -p "$STATE_DIR"
  exec 1> >(tee -a "$LOG_FILE")
  exec 2>&1
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] ArchibaldOS Installer v$SCRIPT_VERSION started"
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] Log: $LOG_FILE"
}

log_info() {
  echo -e "''${BLUE}[INFO]''${NC} $(date '+%H:%M:%S') $*"
}

log_success() {
  echo -e "''${GREEN}[✓]''${NC} $(date '+%H:%M:%S') $*"
}

log_warning() {
  echo -e "''${YELLOW}[WARN]''${NC} $(date '+%H:%M:%S') $*"
}

log_error() {
  echo -e "''${RED}[✗]''${NC} $(date '+%H:%M:%S') $*" >&2
}

error_exit() {
  log_error "$1"
  dialog --title "Installation Error" --msgbox "Error: $1\n\nLog: $LOG_FILE" 10 70
  cleanup
  exit 1
}

# ============================================================================
# Cleanup & Trap Handlers
# ============================================================================

cleanup() {
  log_info "Running cleanup..."
  
  # Securely clear sensitive data
  unset CONFIG[encrypt_pw] CONFIG[userpw] CONFIG[wifi_pw] 2>/dev/null || true
  
  # Unmount on failure
  if [ -d "$INSTALL_ROOT" ] && mountpoint -q "$INSTALL_ROOT" 2>/dev/null; then
    log_warning "Unmounting $INSTALL_ROOT (installation may have failed)"
    umount -R "$INSTALL_ROOT" 2>/dev/null || log_warning "Unmount had issues"
  fi
  
  # Clean temp files
  rm -f /tmp/disko-*.nix /tmp/config-*.nix 2>/dev/null || true
  
  log_info "Cleanup complete. Exiting."
}

trap cleanup EXIT
trap 'error_exit "Interrupted by user"' INT TERM

# ============================================================================
# Validation Functions
# ============================================================================

validate_disk() {
  local disk="$1"
  
  # Check if device exists and is a block device
  if [ ! -b "$disk" ]; then
    return 1
  fi
  
  # Check if already mounted
  if mountpoint -q "$disk" 2>/dev/null; then
    return 1
  fi
  
  # Check for partitions mounted
  if lsblk -n -o MOUNTPOINT "$disk" | grep -q "^/"; then
    return 1
  fi
  
  return 0
}

validate_hostname() {
  local hostname="$1"
  # RFC 1123: alphanumeric and hyphens, start/end with alphanumeric
  [[ $hostname =~ ^[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?$ ]] && return 0
  return 1
}

validate_username() {
  local username="$1"
  # POSIX username: lowercase, digits, underscore, hyphen
  [[ $username =~ ^[a-z0-9_-]{3,32}$ ]] && return 0
  return 1
}

validate_password() {
  local pw="$1"
  [ ''${#pw} -ge 8 ] && return 0
  return 1
}

validate_locale() {
  local locale="$1"
  # Check if locale is available
  locale -a | grep -q "^''${locale}$" 2>/dev/null && return 0
  return 1
}

validate_timezone() {
  local tz="$1"
  [ -f "/usr/share/zoneinfo/$tz" ] && return 0
  return 1
}

# ============================================================================
# Dialog Wrappers
# ============================================================================

dialog_menu() {
  local title="$1" prompt="$2" height="$3" width="$4"
  shift 4
  local items=("$@")
  
  dialog --clear --title "$title" --menu "$prompt" "$height" "$width" $((height - 8)) \
    "''${items[@]}" 3>&1 1>&2 2>&3 || return 1
}

dialog_input() {
  local title="$1" prompt="$2" default="''${3:-}"
  
  local result
  result=$(dialog --clear --title "$title" --inputbox "$prompt" 10 70 "$default" 3>&1 1>&2 2>&3) || return 1
  echo "$result"
}

dialog_password() {
  local title="$1" prompt="$2"
  
  dialog --clear --title "$title" --insecure --passwordbox "$prompt" 10 70 3>&1 1>&2 2>&3 || return 1
}

dialog_yesno() {
  local title="$1" prompt="$2"
  
  dialog --clear --title "$title" --yesno "$prompt" 10 70
}

dialog_confirm() {
  local title="$1"
  
  dialog --clear --title "$title" --textbox /dev/stdin 25 75 3>&1 1>&2 2>&3
}

dialog_info() {
  local title="$1" message="$2"
  
  dialog --clear --title "$title" --msgbox "$message" 12 75
}

# ============================================================================
# Network & Internet
# ============================================================================

test_internet() {
  ping -c 1 -W 2 8.8.8.8 &>/dev/null && return 0
  return 1
}

setup_wifi() {
  log_info "Configuring WiFi..."
  
  # List available networks
  local networks
  networks=$(nmcli -t -f SSID device wifi list 2>/dev/null | sort -u | grep -v '^$' | head -20)
  
  if [ -z "$networks" ]; then
    log_warning "No WiFi networks found"
    return 1
  fi
  
  # Show SSID selection
  local ssid
  ssid=$(echo "$networks" | dialog --clear --title "WiFi Networks" \
    --menu "Select network:" 20 60 10 $(echo "$networks" | nl -v 1 | awk '{print $1 " " $2}') \
    3>&1 1>&2 2>&3) || return 1
  
  CONFIG[wifi_ssid]="$ssid"
  
  # Get password
  local wifi_pw
  wifi_pw=$(dialog_password "WiFi Password" "Enter password for '$ssid':") || return 1
  CONFIG[wifi_pw]="$wifi_pw"
  
  # Connect
  log_info "Connecting to WiFi: $ssid..."
  if nmcli device wifi connect "$ssid" password "$wifi_pw" 2>/dev/null; then
    sleep 3
    if test_internet; then
      log_success "WiFi connected and internet verified"
      CONFIG[has_internet]="true"
      unset CONFIG[wifi_pw]  # Clear password
      return 0
    else
      log_warning "WiFi connected but internet test failed"
      return 1
    fi
  else
    log_error "WiFi connection failed"
    return 1
  fi
}

# ============================================================================
# Disk Management
# ============================================================================

list_disks() {
  lsblk -dno NAME,SIZE,TYPE,MODEL 2>/dev/null | grep disk | awk '
    {
      size = $2; name = $1; model = $4
      # Filter out live USB (usually smallest or contains "boot")
      if (model ~ /boot|live|usb|flash/i) { next }
      printf "%s\0(%s)\0%s\0", "/dev/" name, size, model
    }
  ' | tr '\0' ' '
}

select_disk() {
  log_info "Detecting disks..."
  
  local disks_raw
  disks_raw=$(list_disks)
  
  if [ -z "$disks_raw" ]; then
    error_exit "No suitable disks found"
  fi
  
  local disk
  disk=$(dialog_menu "Disk Selection" \
    "Select disk to FORMAT (will be completely erased):" 18 70 $disks_raw) || \
    error_exit "Disk selection canceled"
  
  if ! validate_disk "$disk"; then
    error_exit "Invalid disk selected: $disk (may be mounted or inaccessible)"
  fi
  
  CONFIG[disk]="$disk"
  log_success "Selected disk: $disk ($(lsblk -ndo SIZE "$disk"))"
}

confirm_disk_wipe() {
  local disk="''${CONFIG[disk]}"
  local size
  size=$(lsblk -ndo SIZE "$disk")
  
  local confirm_msg
  confirm_msg=$(cat <<EOF
⚠️  DISK WILL BE COMPLETELY ERASED ⚠️

Device:    $disk
Size:      $size
Label:     $(lsblk -ndo MODEL "$disk")

This action is IRREVERSIBLE!

Type the device name to confirm: $disk
EOF
)
  
  local response
  response=$(dialog --clear --title "FINAL CONFIRMATION" \
    --inputbox "$confirm_msg" 15 75 3>&1 1>&2 2>&3) || \
    error_exit "Disk wipe canceled"
  
  if [ "$response" != "$disk" ]; then
    error_exit "Confirmation mismatch (typed: $response, expected: $disk)"
  fi
  
  log_success "Disk wipe confirmed by user"
}

# ============================================================================
# Encryption
# ============================================================================

setup_encryption() {
  if ! dialog_yesno "Full-Disk Encryption" \
    "Enable LUKS2 full-disk encryption?\n\nRecommended for security." 2>/dev/null; then
    log_info "Encryption disabled"
    return 0
  fi
  
  CONFIG[encrypt]="true"
  
  local pw1 pw2
  while true; do
    pw1=$(dialog_password "Encryption Password" "Enter encryption password (min 8 chars):") || \
      error_exit "Encryption setup canceled"
    
    if ! validate_password "$pw1"; then
      dialog_info "Password Too Short" "Password must be at least 8 characters"
      continue
    fi
    
    pw2=$(dialog_password "Confirm Password" "Confirm encryption password:") || \
      error_exit "Encryption setup canceled"
    
    if [ "$pw1" = "$pw2" ]; then
      CONFIG[encrypt_pw]="$pw1"
      log_success "Encryption password set"
      unset pw1 pw2
      return 0
    else
      dialog_info "Password Mismatch" "Passwords do not match. Try again."
    fi
  done
}

# ============================================================================
# System Configuration
# ============================================================================

select_keyboard() {
  local kb
  kb=$(dialog_menu "Keyboard Layout" "Select your keyboard layout:" 25 60 \
    us "US English" \
    gb "UK English" \
    de "German" \
    fr "French" \
    es "Spanish" \
    it "Italian" \
    ru "Russian" \
    pl "Polish" \
    pt "Portuguese (Portugal)" \
    br "Portuguese (Brazil)" \
    nl "Dutch" \
    se "Swedish" \
    ca "Canadian" \
    jp "Japanese" \
    kr "Korean" \
    cn "Chinese" 2>/dev/null) || error_exit "Keyboard selection canceled"
  
  CONFIG[keyboard]="$kb"
  log_success "Selected keyboard: $kb"
  
  # Test it
  setxkbmap "$kb" 2>/dev/null || log_warning "Could not set keyboard live"
}

select_locale() {
  local locale
  locale=$(dialog_input "Locale Selection" \
    "Enter locale (default: en_US.UTF-8):" "en_US.UTF-8") || \
    error_exit "Locale selection canceled"
  
  if ! validate_locale "$locale"; then
    log_warning "Locale '$locale' not available, but will be installed"
  fi
  
  CONFIG[locale]="$locale"
  log_success "Selected locale: $locale"
}

select_timezone() {
  # Get common timezones
  local tz
  tz=$(dialog_menu "Timezone" "Select your timezone:" 25 70 \
    "UTC" "Coordinated Universal Time" \
    "America/New_York" "Eastern Time" \
    "America/Chicago" "Central Time" \
    "America/Denver" "Mountain Time" \
    "America/Los_Angeles" "Pacific Time" \
    "Europe/London" "London" \
    "Europe/Paris" "Paris" \
    "Europe/Berlin" "Berlin" \
    "Asia/Tokyo" "Tokyo" \
    "Asia/Shanghai" "Shanghai" \
    "Australia/Sydney" "Sydney" \
    "CUSTOM" "Enter custom timezone" 2>/dev/null) || \
    error_exit "Timezone selection canceled"
  
  if [ "$tz" = "CUSTOM" ]; then
    tz=$(dialog_input "Custom Timezone" "Enter timezone (e.g., America/Los_Angeles):") || \
      error_exit "Timezone canceled"
  fi
  
  if ! validate_timezone "$tz"; then
    error_exit "Invalid timezone: $tz"
  fi
  
  CONFIG[timezone]="$tz"
  log_success "Selected timezone: $tz"
}

set_hostname() {
  while true; do
    local hostname
    hostname=$(dialog_input "Hostname" \
      "Enter system hostname (alphanumeric, hyphens ok):" "archibaldos") || \
      error_exit "Hostname setup canceled"
    
    if validate_hostname "$hostname"; then
      CONFIG[hostname]="$hostname"
      log_success "Hostname: $hostname"
      return 0
    else
      dialog_info "Invalid Hostname" "Hostname must start/end with alphanumeric characters"
    fi
  done
}

set_user_credentials() {
  # Username
  while true; do
    local username
    username=$(dialog_input "Username" \
      "Enter username (lowercase, 3-32 chars):" "audio-user") || \
      error_exit "Username setup canceled"
    
    if validate_username "$username"; then
      if id "$username" &>/dev/null 2>&1; then
        dialog_info "User Exists" "User '$username' already exists"
        continue
      fi
      CONFIG[username]="$username"
      log_success "Username: $username"
      break
    else
      dialog_info "Invalid Username" "Username must be 3-32 chars, lowercase, alphanumeric/hyphen/underscore"
    fi
  done
  
  # Password
  while true; do
    local pw1 pw2
    pw1=$(dialog_password "User Password" "Enter password for $username (min 8 chars):") || \
      error_exit "Password setup canceled"
    
    if ! validate_password "$pw1"; then
      dialog_info "Password Too Short" "Password must be at least 8 characters"
      continue
    fi
    
    pw2=$(dialog_password "Confirm Password" "Confirm password:") || \
      error_exit "Password setup canceled"
    
    if [ "$pw1" = "$pw2" ]; then
      CONFIG[userpw]="$pw1"
      log_success "User password set"
      unset pw1 pw2
      return 0
    else
      dialog_info "Password Mismatch" "Passwords do not match. Try again."
    fi
  done
}

setup_hydramesh() {
  if dialog_yesno "HydraMesh P2P Audio" \
    "Enable HydraMesh for peer-to-peer audio networking?" 2>/dev/null; then
    CONFIG[hydramesh]="true"
    log_success "HydraMesh enabled"
  else
    CONFIG[hydramesh]="false"
    log_info "HydraMesh disabled"
  fi
}

# ============================================================================
# Review & Confirmation
# ============================================================================

review_config() {
  local msg
  msg=$(cat <<EOF
╔════════════════════════════════════════════════════════════════╗
║         INSTALLATION CONFIGURATION REVIEW                      ║
╚════════════════════════════════════════════════════════════════╝

SYSTEM CONFIGURATION
  Hostname:        ''${CONFIG[hostname]}
  Keyboard:        ''${CONFIG[keyboard]}
  Locale:          ''${CONFIG[locale]}
  Timezone:        ''${CONFIG[timezone]}

USER ACCOUNT
  Username:        ''${CONFIG[username]}
  Password:        $([ -n "''${CONFIG[userpw]}" ] && echo "***SET***" || echo "NOT SET")

DISK & ENCRYPTION
  Disk:            ''${CONFIG[disk]}
  Size:            $(lsblk -ndo SIZE "''${CONFIG[disk]}")
  Encryption:      ''${CONFIG[encrypt]} $([ "''${CONFIG[encrypt]}" = "true" ] && echo "(LUKS2)" || echo "")

FEATURES
  HydraMesh:       ''${CONFIG[hydramesh]}
  Internet:        ''${CONFIG[has_internet]}

╚════════════════════════════════════════════════════════════════╝

This will ERASE all data on ''${CONFIG[disk]}!
EOF
)
  
  dialog_confirm "Review Configuration" <<< "$msg"
  
  if dialog_yesno "Proceed" "Start installation with above configuration?" 2>/dev/null; then
    return 0
  else
    error_exit "Installation canceled by user"
  fi
}

# ============================================================================
# Disko Configuration Generation
# ============================================================================

generate_disko_config() {
  local disk="''${CONFIG[disk]}"
  local encrypt="''${CONFIG[encrypt]}"
  local encrypt_pw="''${CONFIG[encrypt_pw]}"
  local luks_name="''${CONFIG[luks_name]}"
  
  local disko_file="/tmp/disko-$$.nix"
  
  if [ "$encrypt" = "true" ]; then
    cat > "$disko_file" <<'DISKO_EOF'
{ disks ? [ "$DISK" ], lib, ... }: {
  disko.devices = {
    disk.main = {
      type = "disk";
      device = lib.head disks;
      format = "gpt";
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            size = "512M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "defaults" "umask=0077" ];
            };
          };
          root = {
            size = "100%";
            content = {
              type = "luks";
              name = "$LUKS_NAME";
              keyFile = "/tmp/luks-key";
              settings.hashAlgorithm = "sha512";
              settings.iterTime = 5000;
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
              };
            };
          };
        };
      };
    };
  };
}
DISKO_EOF
  else
    cat > "$disko_file" <<'DISKO_EOF'
{ disks ? [ "$DISK" ], lib, ... }: {
  disko.devices = {
    disk.main = {
      type = "disk";
      device = lib.head disks;
      format = "gpt";
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            size = "512M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "defaults" "umask=0077" ];
            };
          };
          root = {
            size = "100%";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/";
            };
          };
        };
      };
    };
  };
}
DISKO_EOF
  fi
  
  # Replace variables safely
  sed -i "s|\$DISK|''${disk}|g" "$disko_file"
  sed -i "s|\$LUKS_NAME|''${luks_name}|g" "$disko_file"
  
  # Validate Nix syntax
  if ! nix eval -f "$disko_file" 2>/dev/null >/dev/null; then
    error_exit "Generated disko config has invalid Nix syntax"
  fi
  
  echo "$disko_file"
}

# ============================================================================
# System Installation
# ============================================================================

format_disk() {
  local disk="''${CONFIG[disk]}"
  local disko_file
  disko_file=$(generate_disko_config) || error_exit "Disko config generation failed"
  
  log_info "Formatting disk with disko..."
  
  if ! disko --mode format "$disko_file" 2>&1 | tee -a "$LOG_FILE"; then
    error_exit "Disk formatting failed (see log for details)"
  fi
  
  log_success "Disk formatted"
}

mount_disk() {
  local disk="''${CONFIG[disk]}"
  local disko_file
  disko_file=$(generate_disko_config) || error_exit "Disko config generation failed"
  
  log_info "Mounting disk..."
  
  if ! disko --mode mount "$disko_file" "$INSTALL_ROOT" 2>&1 | tee -a "$LOG_FILE"; then
    error_exit "Disk mounting failed"
  fi
  
  # Verify mount
  sleep 1
  if ! mountpoint -q "$INSTALL_ROOT"; then
    error_exit "Mount verification failed: $INSTALL_ROOT is not mounted"
  fi
  
  log_success "Disk mounted at $INSTALL_ROOT"
}

generate_nixos_config() {
  local keyboard="''${CONFIG[keyboard]}"
  local timezone="''${CONFIG[timezone]}"
  local locale="''${CONFIG[locale]}"
  local hostname="''${CONFIG[hostname]}"
  local username="''${CONFIG[username]}"
  local userpw_hash
  local encrypt="''${CONFIG[encrypt]}"
  local hydramesh="''${CONFIG[hydramesh]}"
  
  log_info "Generating NixOS configuration..."
  
  # Generate password hash
  userpw_hash=$(echo "''${CONFIG[userpw]}" | mkpasswd -m sha-512 -s)
  
  # Generate hardware config
  nixos-generate-config --root "$INSTALL_ROOT" 2>&1 | tee -a "$LOG_FILE" || \
    error_exit "Hardware configuration generation failed"
  
  # Create main configuration
  local config_file="$INSTALL_ROOT/etc/nixos/configuration.nix"
  
  cat > "$config_file" <<EOF
{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # Boot & Kernel
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Localization
  time.timeZone = "$timezone";
  i18n.defaultLocale = "$locale";
  i18n.supportedLocales = [ "$locale" ];

  # Keyboard
  services.xserver.xkb.layout = "$keyboard";
  console.keyMap = "$keyboard";

  # Networking
  networking.hostName = "$hostname";
  networking.networkmanager.enable = true;

  # Audio Setup (RT tweaks)
  boot.kernel.sysctl = {
    "vm.swappiness" = 10;
    "fs.inotify.max_user_watches" = 600000;
  };

  # LUKS/Encryption
$(if [ "$encrypt" = "true" ]; then cat <<LUKS_CONFIG
  boot.initrd.luks.devices."''${CONFIG[luks_name]}" = {
    device = "/dev/disk/by-uuid/$(blkid -s UUID -o value /dev/\$(lsblk -no NAME "''${CONFIG[disk]}" | tail -1)2)";
    preLVM = true;
  };
LUKS_CONFIG
else echo "  # Encryption disabled"; fi)

  # Users
  users.users."$username" = {
    isNormalUser = true;
    initialHashedPassword = "$userpw_hash";
    extraGroups = [ "wheel" "audio" "jackaudio" "video" "networkmanager" ];
    shell = pkgs.bash;
  };

  # Packages
  environment.systemPackages = with pkgs; [
    curl wget git htop tmux
    alsa-utils pulseaudio
    jack2
    ardour
    dialog
  ];

  # Services
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };

$(if [ "$hydramesh" = "true" ]; then cat <<HYDRAMESH_CONFIG
  # HydraMesh P2P Audio
  services.hydramesh.enable = true;
HYDRAMESH_CONFIG
else echo "  # HydraMesh disabled"; fi)

  # Hyprland WM
  programs.hyprland.enable = true;
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # Security
  security.sudo.wheelNeedsPassword = true;
  security.lockKernelModules = false;

  # System
  system.stateVersion = "24.05";
}
EOF
  
  log_success "NixOS configuration generated"
}

install_system() {
  log_info "Installing NixOS (this may take 10-15 minutes)..."
  
  if ! nixos-install --root "$INSTALL_ROOT" --no-root-passwd 2>&1 | tee -a "$LOG_FILE"; then
    error_exit "NixOS installation failed"
  fi
  
  log_success "NixOS installation complete"
}

setup_user_configs() {
  local username="''${CONFIG[username]}"
  mkdir -p "$INSTALL_ROOT/home/$username/.config/hypr" "$INSTALL_ROOT/home/$username/.config/waybar" "$INSTALL_ROOT/home/$username/.config/wofi" || error_exit "User config directories failed"
  cp /etc/hypr/* "$INSTALL_ROOT/home/$username/.config/hypr/" || error_exit "Hyprland copy failed"
  cp /etc/waybar/* "$INSTALL_ROOT/home/$username/.config/waybar/" || error_exit "Waybar copy failed"
  cp /etc/wofi/* "$INSTALL_ROOT/home/$username/.config/wofi/" || error_exit "Wofi copy failed"
  chown -R 1000:1000 "$INSTALL_ROOT/home/$username" || log_warning "Chown failed (using default UID 1000)"
  chmod +x "$INSTALL_ROOT/home/$username/.config/hypr/"*.sh 2>/dev/null || true
  log_success "User configs copied"
}

# ============================================================================
# Main Installation Flow
# ============================================================================

main() {
  clear
  
  # Show splash/branding if available
  if [ -f /etc/installer-ascii.txt ]; then
    cat /etc/installer-ascii.txt
    sleep 2
  fi
  
  dialog_info "Welcome" "Welcome to ArchibaldOS Installer v$SCRIPT_VERSION\n\nThis will guide you through installing a production-ready audio workstation on NixOS."
  
  # Check internet
  log_info "Checking internet connectivity..."
  if test_internet; then
    CONFIG[has_internet]="true"
    log_success "Internet available"
  else
    log_warning "No internet detected"
    if dialog_yesno "No Internet" "Internet not detected. Setup WiFi now?" 2>/dev/null; then
      if setup_wifi; then
        log_success "Internet configured"
      else
        log_warning "Skipping WiFi setup; will proceed without internet"
      fi
    fi
  fi
  
  # Disk selection
  select_disk
  confirm_disk_wipe
  
  # System configuration
  select_keyboard
  select_locale
  select_timezone
  set_hostname
  set_user_credentials
  setup_encryption
  setup_hydramesh
  
  # Review
  review_config
  
  # Installation
  log_info "Starting disk formatting..."
  format_disk
  mount_disk
  
  generate_nixos_config
  install_system
  
  # Setup user configs (post-install)
  setup_user_configs
  
  # Cleanup sensitive data
  unset CONFIG[userpw] CONFIG[encrypt_pw]
  
  dialog_info "Success" "Installation complete!\n\nSystem will boot into ArchibaldOS on next reboot.\n\nLog saved to: $LOG_FILE"
  log_success "Installation completed successfully"
}

# ============================================================================
# Entry Point
# ============================================================================

setup_logging

if [ $EUID -ne 0 ]; then
  error_exit "This script must be run as root"
fi

if [ ! -x "$(command -v disko)" ]; then
  error_exit "disko not found; ensure it's available in the live environment"
fi

if [ ! -x "$(command -v dialog)" ]; then
  error_exit "dialog not found; required for interactive setup"
fi

main
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
