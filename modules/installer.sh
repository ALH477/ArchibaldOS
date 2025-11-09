#!/usr/bin/env bash
# modules/installer.sh
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
  log_info "ArchibaldOS Installer v$SCRIPT_VERSION started"
  log_info "Log: $LOG_FILE"
}

log_info() {
  echo -e "${BLUE}[INFO]${NC} $(date '+%H:%M:%S') $*" | tee -a "$LOG_FILE"
}

log_success() {
  echo -e "${GREEN}[✓]${NC} $(date '+%H:%M:%S') $*" | tee -a "$LOG_FILE"
}

log_warning() {
  echo -e "${YELLOW}[WARN]${NC} $(date '+%H:%M:%S') $*" | tee -a "$LOG_FILE"
}

log_error() {
  echo -e "${RED}[✗]${NC} $(date '+%H:%M:%S') $*" | tee -a "$LOG_FILE" >&2
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
  [ ${#pw} -ge 8 ] && return 0
  return 1
}

validate_locale() {
  local locale="$1"
  # Check if locale is available
  locale -a | grep -q "^${locale}$" 2>/dev/null && return 0
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
  local title="$1"
  local prompt="$2"
  local height="$3"
  local width="$4"
  shift 4
  local items=("$@")
  
  dialog --clear --stdout --title "$title" --menu "$prompt" "$height" "$width" $((height - 8)) \
    "${items[@]}" 3>&1 1>&2 2>&3 || return 1
}

dialog_input() {
  local title="$1"
  local prompt="$2"
  local default="${3:-}"
  
  local result
  result=$(dialog --clear --stdout --title "$title" --inputbox "$prompt" 10 70 "$default" 3>&1 1>&2 2>&3) || return 1
  echo "$result"
}

dialog_password() {
  local title="$1"
  local prompt="$2"
  
  local result
  result=$(dialog --clear --stdout --title "$title" --insecure --passwordbox "$prompt" 10 70 3>&1 1>&2 2>&3) || return 1
  echo "$result"
}

dialog_yesno() {
  local title="$1"
  local prompt="$2"
  
  dialog --clear --title "$title" --yesno "$prompt" 10 70
}

dialog_confirm() {
  local title="$1"
  
  dialog --clear --title "$title" --textbox /dev/stdin 25 75 3>&1 1>&2 2>&3
}

dialog_info() {
  local title="$1"
  local message="$2"
  
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
  
  local networks
  networks=$(nmcli -t -f SSID device wifi list 2>/dev/null | sort -u | grep -v '^$' | head -20)
  
  if [ -z "$networks" ]; then
    log_warning "No WiFi networks found"
    return 1
  fi
  
  local menu_args=()
  local count=0
  local temp_net=$(mktemp)
  echo "$networks" > "$temp_net"
  while IFS= read -r ssid; do
    if [ -n "$ssid" ]; then
      count=$((count + 1))
      menu_args+=("$count" "$ssid")
    fi
  done < "$temp_net"
  rm "$temp_net"
  
  if [ $count -eq 0 ]; then
    return 1
  fi
  
  local choice
  choice=$(dialog_menu "WiFi Networks" "Select network:" 20 60 "${menu_args[@]}") || return 1
  
  local ssid
  ssid=$(sed -n "${choice}p" <<< "$networks")
  if [ -z "$ssid" ]; then
    log_error "Invalid SSID selection"
    return 1
  fi
  
  CONFIG[wifi_ssid]="$ssid"
  
  local wifi_pw
  wifi_pw=$(dialog_password "WiFi Password" "Enter password for '$ssid':") || return 1
  CONFIG[wifi_pw]="$wifi_pw"
  
  log_info "Connecting to WiFi: $ssid..."
  if nmcli device wifi connect "$ssid" password "$wifi_pw" 2>/dev/null; then
    sleep 3
    if test_internet; then
      log_success "WiFi connected and internet verified"
      CONFIG[has_internet]="true"
      unset CONFIG[wifi_pw]
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
  lsblk -dno NAME,SIZE,MODEL | awk '
    {
      name = "/dev/" $1;
      size = $2;
      model = $3;
      if (model ~ /boot|live|usb|flash/i) { next }
      desc = name " (" size ") " model;
      printf "%s\n%s\n", name, desc;
    }'
}

select_disk() {
  log_info "Detecting disks..."
  
  local disks_raw
  disks_raw=$(list_disks)
  
  if [ -z "$disks_raw" ]; then
    error_exit "No suitable disks found"
  fi
  
  local menu_args=()
  local temp_file=$(mktemp)
  echo "$disks_raw" > "$temp_file"
  while IFS= read -r dev && IFS= read -r desc; do
    menu_args+=("$dev" "$desc")
  done < "$temp_file"
  rm "$temp_file"
  
  if [ ${#menu_args[@]} -eq 0 ]; then
    error_exit "No valid disks to build menu"
  fi
  
  local dialog_output
  dialog_output=$(dialog_menu "Disk Selection" \
    "Select disk to FORMAT (will be completely erased):" 18 70 "${menu_args[@]}") || \
    error_exit "Disk selection canceled"
  
  if ! validate_disk "$dialog_output"; then
    error_exit "Invalid disk selected: $dialog_output (may be mounted or inaccessible)"
  fi
  
  CONFIG[disk]="$dialog_output"
  log_success "Selected disk: ${CONFIG[disk]} ($(lsblk -ndo SIZE "${CONFIG[disk]}"))"
}

confirm_disk_wipe() {
  local disk="${CONFIG[disk]}"
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
  response=$(dialog --clear --stdout --title "FINAL CONFIRMATION" \
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
  
  local pw1
  local pw2
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
  
  setxkbmap "$kb" 2>/dev/null || log_warning "Could not set keyboard live (console-only env?)"
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
  
  local pw1
  local pw2
  while true; do
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
  Hostname:        ${CONFIG[hostname]}
  Keyboard:        ${CONFIG[keyboard]}
  Locale:          ${CONFIG[locale]}
  Timezone:        ${CONFIG[timezone]}

USER ACCOUNT
  Username:        ${CONFIG[username]}
  Password:        $([ -n "${CONFIG[userpw]}" ] && echo "***SET***" || echo "NOT SET")

DISK & ENCRYPTION
  Disk:            ${CONFIG[disk]}
  Size:            $(lsblk -ndo SIZE "${CONFIG[disk]}")
  Encryption:      ${CONFIG[encrypt]} $([ "${CONFIG[encrypt]}" = "true" ] && echo "(LUKS2)" || echo "")

FEATURES
  HydraMesh:       ${CONFIG[hydramesh]}
  Internet:        ${CONFIG[has_internet]}

╚════════════════════════════════════════════════════════════════╝

This will ERASE all data on ${CONFIG[disk]}!
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
  local disk="${CONFIG[disk]}"
  local encrypt="${CONFIG[encrypt]}"
  local encrypt_pw="${CONFIG[encrypt_pw]}"
  local luks_name="${CONFIG[luks_name]}"
  
  local disko_file="/tmp/disko-$$.nix"
  
  if [ "$encrypt" = "true" ]; then
    cat > "$disko_file" <<'DISKO_EOF'
{ disks ? [ "$DISK" ], lib, ... }: {
  disko.devices = {
    disk.main = {
      type = "disk";
      device = lib.head disks;
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
              settings = {
                hashAlgorithm = "sha512";
                iterTime = 5000;
              };
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
  
  sed -i "s|\$DISK|${disk}|g" "$disko_file"
  sed -i "s|\$LUKS_NAME|${luks_name}|g" "$disko_file"
  
  if ! nix eval -f "$disko_file" 2>/dev/null >/dev/null; then
    error_exit "Generated disko config has invalid Nix syntax"
  fi
  
  echo "$disko_file"
}

# ============================================================================
# System Installation
# ============================================================================

format_disk() {
  local disk="${CONFIG[disk]}"
  local disko_file
  disko_file=$(generate_disko_config) || error_exit "Disko config generation failed"
  
  log_info "Formatting disk with disko..."
  
  if ! disko --mode format "$disko_file" 2>&1 | tee -a "$LOG_FILE"; then
    error_exit "Disk formatting failed (see log for details)"
  fi
  
  log_success "Disk formatted"
}

mount_disk() {
  local disk="${CONFIG[disk]}"
  local disko_file
  disko_file=$(generate_disko_config) || error_exit "Disko config generation failed"
  
  log_info "Mounting disk..."
  
  if ! disko --mode mount "$disko_file" "$INSTALL_ROOT" 2>&1 | tee -a "$LOG_FILE"; then
    error_exit "Disk mounting failed"
  fi
  
  sleep 1
  if ! mountpoint -q "$INSTALL_ROOT"; then
    error_exit "Mount verification failed: $INSTALL_ROOT is not mounted"
  fi
  
  log_success "Disk mounted at $INSTALL_ROOT"
}

generate_nixos_config() {
  local keyboard="${CONFIG[keyboard]}"
  local timezone="${CONFIG[timezone]}"
  local locale="${CONFIG[locale]}"
  local hostname="${CONFIG[hostname]}"
  local username="${CONFIG[username]}"
  local userpw_hash
  local encrypt="${CONFIG[encrypt]}"
  local hydramesh="${CONFIG[hydramesh]}"
  
  log_info "Generating NixOS configuration..."
  
  userpw_hash=$(echo "${CONFIG[userpw]}" | mkpasswd -m sha-512 -s)
  
  nixos-generate-config --root "$INSTALL_ROOT" 2>&1 | tee -a "$LOG_FILE" || \
    error_exit "Hardware configuration generation failed"
  
  local config_file="$INSTALL_ROOT/etc/nixos/configuration.nix"
  
  cat > "$config_file" <<EOF
{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_rt_latest;

  time.timeZone = "$timezone";
  i18n.defaultLocale = "$locale";
  i18n.supportedLocales = [ "$locale" ];

  services.xserver.xkb.layout = "$keyboard";
  console.keyMap = "$keyboard";

  networking.hostName = "$hostname";
  networking.networkmanager.enable = true;

  boot.kernel.sysctl = {
    "vm.swappiness" = 10;
    "fs.inotify.max_user_watches" = 600000;
  };

$(if [ "$encrypt" = "true" ]; then cat <<LUKS_CONFIG
  boot.initrd.luks.devices."${CONFIG[luks_name]}" = {
    device = "/dev/disk/by-uuid/$(blkid -s UUID -o value /dev/$(lsblk -no NAME "${CONFIG[disk]}" | tail -1)2)";
    preLVM = true;
  };
LUKS_CONFIG
else echo "  # Encryption disabled"; fi)

  users.users."$username" = {
    isNormalUser = true;
    initialHashedPassword = "$userpw_hash";
    extraGroups = [ "wheel" "audio" "jackaudio" "video" "networkmanager" ];
    shell = pkgs.bash;
  };

  environment.systemPackages = with pkgs; [
    curl wget git htop tmux
    alsa-utils pulseaudio
    jack2
    ardour
    dialog
    dwm dmenu st
  ];

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };
  services.xserver.enable = true;
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.windowManager.dwm.enable = true;

$(if [ "$hydramesh" = "true" ]; then cat <<HYDRAMESH_CONFIG
  services.hydramesh.enable = true;
HYDRAMESH_CONFIG
else echo "  # HydraMesh disabled"; fi)

  security.sudo.wheelNeedsPassword = true;
  security.lockKernelModules = false;

  system.stateVersion = "24.11";
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
  local username="${CONFIG[username]}"
  local home_dir="$INSTALL_ROOT/home/$username"
  mkdir -p "$home_dir" || error_exit "User home directory creation failed"
  local uid=1000
  local gid=1000
  chown -R "$uid:$gid" "$home_dir" || log_warning "Chown failed (using $uid:$gid)"
  log_success "User home directory set up"
}

# ============================================================================
# Main Installation Flow
# ============================================================================

main() {
  clear
  
  if [ -f /etc/installer-ascii.txt ]; then
    cat /etc/installer-ascii.txt
    sleep 2
  fi
  
  dialog_info "Welcome" "Welcome to ArchibaldOS Installer v$SCRIPT_VERSION\n\nThis will guide you through installing a production-ready audio workstation on NixOS."
  
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
  
  select_disk
  confirm_disk_wipe
  
  select_keyboard
  select_locale
  set_hostname
  set_user_credentials
  setup_encryption
  setup_hydramesh
  
  review_config
  
  log_info "Starting disk formatting..."
  format_disk
  mount_disk
  
  generate_nixos_config
  install_system
  
  setup_user_configs
  
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
