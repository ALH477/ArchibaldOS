# ArchibaldOS-builder
# Use official Nix base for flakes support (adaptable to other tags like :2.18 for versions)
FROM nixos/nix:latest AS builder

# Enable flakes and other experimental features globally
RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

# Set working dir for flake files
WORKDIR /build

# Copy flake and modules
COPY flake.nix /build/flake.nix
COPY modules/audio.nix /build/modules/audio.nix
COPY modules/desktop.nix /build/modules/desktop.nix
COPY modules/users.nix /build/modules/users.nix
COPY modules/branding.nix /build/modules/branding.nix
COPY modules/assets/ /build/modules/assets/

# Install minimal build-time deps (keeps RT audio focus; adaptable)
RUN nix-env -iA nixpkgs.git nixpkgs.cacert

# Build the ISO (targets packages.x86_64-linux.installer; adaptable for VM or other outputs)
RUN nix --extra-experimental-features "nix-command flakes" build .#packages.x86_64-linux.installer

# Final stage: Copy built ISO to a slim image for extraction (headless, no bloat)
FROM scratch
COPY --from=builder /build/result/iso/*.iso /output/archibaldos.iso
