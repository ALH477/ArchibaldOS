# modules/hydramesh-plugins.nix
{ pkgs, lib, ... }: {
  environment.etc = {
    "hydramesh/plugins/lora.lisp".source = ./../HydraMesh/plugins/lora.lisp;
    "hydramesh/plugins/zigbee.lisp".source = ./../HydraMesh/plugins/zigbee.lisp;
    "hydramesh/plugins/can-bus.lisp".source = ./../HydraMesh/plugins/can-bus.lisp;
    "hydramesh/plugins/serial.lisp".source = ./../HydraMesh/plugins/serial.lisp;
    "hydramesh/plugins/thunderbolt-transport.lisp".source = ./../HydraMesh/plugins/thunderbolt-transport.lisp;
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
