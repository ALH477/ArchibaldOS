{
  description = "StreamDB: A reverse Trie index key-value database for path-based queries with prefix searches";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }: 
    flake-utils.lib.eachDefaultSystem (system: let
      overlays = [ (self: super: rec {
        rustToolchain = rust-overlay.packages.${system}.rust.override {
          extensions = [ "rust-src" ];
        };
      }) ];
      pkgs = import nixpkgs { inherit system overlays; };
    in {
      packages.default = pkgs.rustPlatform.buildRustPackage rec {
        pname = "streamdb";
        version = "0.1.0";
        src = ./.;
        cargoHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";  # Replace with `nix build .#default`
        buildPhase = "cargo build --release --lib";  # Build as library
        installPhase = ''
          mkdir -p $out/lib
          cp target/release/libstreamdb.so $out/lib/
        '';
        meta = with pkgs.lib; {
          description = "A reverse Trie index key-value database for path-based queries with prefix searches.";
          license = licenses.lgpl3;
          homepage = "https://github.com/xai/streamdb";
        };
      };

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [ pkgs.rustToolchain pkgs.pkg-config ];
        buildInputs = with pkgs; [ openssl ];  # For ring (encryption feature)
      };
    });
}
