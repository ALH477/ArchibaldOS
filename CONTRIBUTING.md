# Contributing to ArchibaldOS

## Copyright and outside contributions

DeMoD LLC holds the copyright in ArchibaldOS. The project is published under
the BSD-3-Clause licence in [LICENSE](LICENSE); third-party portions keep
their own holders and licences, recorded in [REUSE.toml](REUSE.toml).

Outside contributions will require a contributor licence agreement (CLA) with
DeMoD LLC. That agreement is being prepared. Until it is published, pull
requests from outside contributors are not merged.

## Before a change

- `nix flake check --no-update-lock-file` runs the gates CI runs
  ([tests/README.md](tests/README.md) lists them).
- `reuse lint` must pass. Licensing for files without their own header is in
  [REUSE.toml](REUSE.toml); licence texts are in [LICENSES/](LICENSES/).
- The files under `installer/` that Oligarchy vendors stay byte-identical with
  Oligarchy's copies. A change to one goes to both trees in the same piece of
  work.
- No image built here may carry software the project has no right to
  redistribute. Nothing in `flake.nix` sets `allowUnfree`, so such a package
  fails evaluation. An unfree package whose licence does allow
  redistribution is allowed by name (`nixpkgs.config.allowUnfreePackages`),
  next to where it is used.
