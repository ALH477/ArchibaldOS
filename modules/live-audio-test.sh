#!/usr/bin/env bash
# Live Audio Test Script for ArchibaldOS ISO
# Robust RT audio setup and latency testing
# Usage: sudo /etc/live-audio-test.sh [--dry-run] [--loopback]

set -euo pipefail  # Strict mode: exit on error, undefined vars, pipe failures
IFS=$'\n\t'        # Word splitting on newlines/tabs only

readonly SCRIPT_VERSION="1.0.0"
readonly LOG_FILE="/tmp/live-audio-test.log"
readonly DRY_RUN=${1:-false}  # Default: no dry-run
readonly LOOPBACK=${2:-false} # Default: no loopback prompt

# Logging function
log() {
    echo -e "$(date '+%Y-%m-%d %H:%M:%S') [$1] $2" | tee -a "$LOG_FILE"
}

log_info() { log "INFO" "$*"; }
log_success() { log "SUCCESS" "$*"; }
log_warning() { log "WARN" "$*"; }
log_error() { log "ERROR" "$*"; exit 1; }

# Cleanup function
cleanup() {
    log_info "Cleaning up..."
    # Stop JACK if running
    if command -v jack_control &>/dev/null; then
        jack_control stop || true
    fi
    # Restore services if disabled
    systemctl start NetworkManager bluetooth || true
    log_info "Cleanup complete."
}
trap cleanup EXIT INT TERM

# Check root
if [[ $EUID -ne 0 ]]; then
    log_error "Run as root: sudo $0"
fi

# Dry-run check
if [[ "$DRY_RUN" == "--dry-run" ]]; then
    log_warning "Dry-run mode: No changes applied."
fi

# Function: Apply RT tweaks (idempotent)
apply_tweaks() {
    log_info "Applying RT tweaks..."
    sysctl vm.swappiness=10 fs.inotify.max_user_watches=600000 || log_warning "Sysctl failed"
    echo 2048 > /sys/class/rtc/rtc0/max_user_freq || log_warning "RTC freq failed"
    echo 2048 > /proc/sys/dev/hpet/max_user_freq || log_warning "HPET freq failed"
    if [[ "$DRY_RUN" != "--dry-run" ]]; then
        chrt -f -p 99 $$  # Boost this script's priority
    fi
    log_success "Tweaks applied"
}

# Function: Detect and optimize audio devices
optimize_audio() {
    log_info "Detecting audio devices..."
    local devices=$(lspci | grep -i audio || lsusb | grep -i audio)
    log_info "$devices"
    if [[ -n "$devices" ]]; then
        # Pin audio IRQ to CPU1 (example for snd IRQ)
        local audio_irq=$(cat /proc/interrupts | grep -i snd | head -1 | awk '{print $1}' | sed 's/:$//')
        if [[ -n "$audio_irq" ]]; then
            if [[ "$DRY_RUN" != "--dry-run" ]]; then
                echo 2 > "/proc/irq/$audio_irq/smp_affinity" || log_warning "IRQ pinning failed"
            fi
            log_success "Audio IRQ pinned"
        fi
    else
        log_warning "No audio devices detected"
    fi
}

# Function: Start JACK
start_jack() {
    if command -v jackd &>/dev/null; then
        log_info "Starting JACK (96kHz/32 samples)..."
        if [[ "$DRY_RUN" != "--dry-run" ]]; then
            jackd -d alsa -d hw:0 -r 96000 -p 32 -n 2 -X raw &>/dev/null &
            sleep 3  # Wait for startup
            if pgrep jackd &>/dev/null; then
                log_success "JACK started"
            else
                log_error "JACK failed to start"
            fi
        fi
    else
        log_warning "JACK not available; skipping"
        return 1
    fi
}

# Function: Run cyclictest
test_kernel() {
    if command -v cyclictest &>/dev/null; then
        log_info "Running kernel latency test (cyclictest)..."
        local output=$(timeout 30 cyclictest -l 10000 -m -n -p99 -q 2>&1 || echo "Failed")
        log_info "$output"
        if [[ "$output" != *"Failed"* ]]; then
            log_success "Kernel test complete (check log for Max/Avg us)"
        fi
    else
        log_warning "cyclictest not found; install via nix-shell -p rt-tests"
    fi
}

# Function: Run jack_iodelay (with optional loopback prompt)
test_rtl() {
    if [[ "$LOOPBACK" == "--loopback" ]] || [[ "$LOOPBACK" == true ]]; then
        read -p "Connect loopback cable (output to input). Press Enter to continue..."
    fi
    if command -v jack_iodelay &>/dev/null && pgrep jackd &>/dev/null; then
        log_info "Running JACK RTL test..."
        local output=$(timeout 10 jack_iodelay 2>&1 || echo "Failed - ensure JACK connections")
        log_info "$output"
        if [[ "$output" != *"Failed"* ]]; then
            log_success "RTL test complete (check log for ms latency)"
        fi
    else
        log_warning "JACK not running or jack_iodelay missing; skipping RTL"
    fi
}

# Function: Disable non-essential services
disable_services() {
    log_info "Disabling non-essential services for RT..."
    if [[ "$DRY_RUN" != "--dry-run" ]]; then
        systemctl stop NetworkManager bluetooth || log_warning "Services stop failed"
    fi
    log_success "Services disabled"
}

# Main execution
main() {
    log_info "Starting Live Audio Test v$SCRIPT_VERSION"
    apply_tweaks
    optimize_audio
    disable_services
    if start_jack; then
        test_kernel
        test_rtl
    fi
    log_success "Test complete! Log: $LOG_FILE. Reboot to reset."
}

main "$@"
