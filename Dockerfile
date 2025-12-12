# Dockerfile
# ============================================================================
# Multi-platform build environment for ArchibaldOS
# Supports: x86_64-linux, aarch64-linux (via QEMU), and cross-compilation
# Enables building NixOS images on macOS, Windows (WSL), or any Docker host
# ============================================================================

# Use official Nix image with multi-arch support
FROM nixos/nix:latest AS nix-base

# Install essential tools
RUN apk add --no-cache \
      git \
      curl \
      wget \
      jq \
      bash \
      sudo \
      openssh \
      zstd \
      qemu-system-aarch64 \
      qemu-system-x86_64 \
      binfmt-support \
      && rm -rf /var/cache/apk/*

# Register QEMU binfmt for cross-architecture execution
RUN qemu-binfmt-conf.sh --qemu-path /usr/bin/qemu-aarch64 --persistent yes || true
RUN qemu-binfmt-conf.sh --qemu-path /usr/bin/qemu-x86_64 --persistent yes || true

# Create non-root user
RUN adduser -D -g '' builder && \
    echo "builder ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

USER builder
WORKDIR /home/builder

# Configure Nix
RUN mkdir -p /home/builder/.config/nix && \
    echo "experimental-features = nix-command flakes" > /home/builder/.config/nix/nix.conf && \
    echo "max-jobs = auto" >> /home/builder/.config/nix/nix.conf

# ============================================================================
# Stage 1: Clone and prefetch flake inputs
# ============================================================================
FROM nix-base AS flake-cache

ARG ARCHIBALDOS_REPO=https://github.com/ALH477/ArchibaldOS.git
ARG ARCHIBALDOS_REV=main

RUN git clone --branch $ARCHIBALDOS_REV $ARCHIBALDOS_REPO /home/builder/ArchibaldOS

WORKDIR /home/builder/ArchibaldOS

# Prefetch and cache all flake inputs
RUN nix flake update && \
    nix flake archive --json > /dev/null

# ============================================================================
# Stage 2: Build x86_64 ISO (native)
# ============================================================================
FROM flake-cache AS iso-x86_64

ARG BUILD_TARGET=packages.x86_64-linux.iso

RUN nix build .#${BUILD_TARGET} --print-build-logs

# ============================================================================
# Stage 3: Build aarch64 images (via QEMU emulation)
# ============================================================================
FROM flake-cache AS images-arm64

# Build all ARM64 targets
RUN nix build .#packages.aarch64-linux.orangepi5 --print-build-logs && \
    nix build .#packages.aarch64-linux.rpi3b --print-build-logs && \
    nix build .#packages.aarch64-linux.generic --print-build-logs

# ============================================================================
# Stage 4: Output artifacts
# ============================================================================
FROM scratch AS artifacts

# Copy x86_64 ISO
COPY --from=iso-x86_64 /home/builder/ArchibaldOS/result/iso/*.iso /iso-x86_64/

# Copy ARM64 images
COPY --from=images-arm64 /home/builder/ArchibaldOS/result/sd-image/*.img* /arm64/
COPY --from=images-arm64 /home/builder/ArchibaldOS/result/toplevel /arm64-generic/

# ============================================================================
# Stage 5: Developer shell (default)
# ============================================================================
FROM flake-cache AS dev-shell

WORKDIR /home/builder/ArchibaldOS

# Auto-source Nix in interactive sessions
RUN echo '. /nix/var/nix/profiles/default/etc/profile.d/nix.sh' >> /home/builder/.bashrc

# Helper script
RUN cat > /home/builder/welcome.sh << 'EOF'
#!/bin/bash
echo "ArchibaldOS Cross-Platform Build Environment"
echo "==========================================="
echo ""
echo "Available targets:"
echo "  x86_64 ISO: nix build .#packages.x86_64-linux.iso"
echo "  Orange Pi 5: nix build .#packages.aarch64-linux.orangepi5"
echo "  RPi 3B: nix build .#packages.aarch64-linux.rpi3b"
echo "  Generic ARM: nix build .#packages.aarch64-linux.generic"
echo ""
echo "Outputs appear in ./result/"
echo ""
echo "Run 'nix flake update' to refresh inputs"
EOF

RUN chmod +x /home/builder/welcome.sh

ENTRYPOINT ["/bin/bash", "-l", "-c", "/home/builder/welcome.sh && exec bash"]


# Copyright 2025 DeMoD LLC

# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


