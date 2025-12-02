# ============================================================================
# Stage 1: Base reproducible Nix environment (builder foundation)
# Uses Debian-slim + manual Nix install for full control + stability.
# ============================================================================

FROM debian:stable-slim AS nix-env

ENV DEBIAN_FRONTEND=noninteractive
ENV USER=builder
ENV HOME=/home/builder

# System dependencies
RUN apt-get update && apt-get install -y \
      curl \
      ca-certificates \
      sudo \
      git \
      xz-utils \
      bash \
      bzip2 \
      && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -s /bin/bash $USER && \
    echo "$USER ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER $USER
WORKDIR $HOME

# Install Nix (daemon)
RUN sh <(curl -L https://nixos.org/nix/install) --daemon

# Add Nix paths
ENV PATH=$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH

# Enable flakes globally
RUN mkdir -p $HOME/.config/nix && \
    echo "experimental-features = nix-command flakes" > $HOME/.config/nix/nix.conf



# ============================================================================
# Stage 2: Import project files + prefetch flake inputs (reproducible caching)
# Everything needed for deterministic future builds.
# ============================================================================

FROM nix-env AS flake-cache

# ARG allows overriding branch or commit for reproducible CI
ARG ARCHIBALDOS_REV=main
ARG ARCHIBALDOS_REPO=https://github.com/ALH477/ArchibaldOS.git

# Clone project at pinned or branch revision
RUN git clone --branch $ARCHIBALDOS_REV $ARCHIBALDOS_REPO $HOME/ArchibaldOS

WORKDIR $HOME/ArchibaldOS

# Prefetch ALL flake inputs during image build:
RUN nix flake archive --json > /dev/null



# ============================================================================
# Stage 3 (optional): CI mode — Build the ISO automatically.
# This stage ONLY runs during docker build if triggered via:
#   docker build --target iso-builder ...
# ============================================================================

FROM flake-cache AS iso-builder

WORKDIR /home/builder/ArchibaldOS

# Optional: expose configurable build target
ARG BUILD_TARGET=packages.x86_64-linux.installer

# Build the ISO non-interactively
RUN nix build .#${BUILD_TARGET}



# ============================================================================
# Stage 4: Output container — contains ONLY the finished ISO.
# Perfect for CI/CD artifact publishing.
# ============================================================================

FROM scratch AS iso-artifact

# Expose ISO
COPY --from=iso-builder /home/builder/ArchibaldOS/result/iso/*.iso /archibaldos.iso



# ============================================================================
# Stage 5: Developer shell — fully cached environment for interactive use.
# This is the default stage unless another --target is selected.
# ============================================================================
FROM flake-cache AS dev-shell

WORKDIR /home/builder/ArchibaldOS

ENTRYPOINT ["/bin/bash"]
