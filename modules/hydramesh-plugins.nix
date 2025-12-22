# modules/hydramesh-plugins.nix
{ pkgs, lib, ... }: {
  environment.etc = {
    "hydramesh/plugins/lora.lisp".source = ./../HydraMesh/plugins/lorawan-transport.lisp;
    "hydramesh/plugins/zigbee.lisp".source = ./../HydraMesh/plugins/zigbee-transport.lisp;
    "hydramesh/plugins/can-bus.lisp".source = ./../HydraMesh/plugins/can-transport.lisp;
    "hydramesh/plugins/serial.lisp".source = ./../HydraMesh/plugins/serial-transport.lisp;
    "hydramesh/plugins/thunderbolt-transport.lisp".source = ./../HydraMesh/plugins/thunderbolt-transport.lisp;
    "hydramesh/plugins/thunderbolt-transport-tests.lisp".source = ./../HydraMesh/plugins/thunderbolt-transport-tests.lisp;
    # Add more as needed
  };

  # Pre-enable useful ones
  services.hydramesh.configFile = lib.mkDefault "/etc/hydramesh/config.json";

  environment.etc."hydramesh/config.json".text = lib.mkAfter (builtins.toJSON {
    plugins = {
      "audio-compressor" = { enabled = true; path = "/etc/hydramesh/plugins/audio-compressor.lisp"; };
      "can-bus" = { enabled = false; path = "/etc/hydramesh/plugins/can-bus.lisp"; };
    };
  });
}
