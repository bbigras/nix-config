{
  imports = [ ./documentation.nix ];

  environment = {
    enableDebugInfo = true;
  };

  programs = {
    wireshark.enable = true;
  };
}
