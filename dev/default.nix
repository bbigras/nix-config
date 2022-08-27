{ pkgs, ... }:

{
  imports = [ ./documentation.nix ];

  environment = {
    enableDebugInfo = true;
  };

  fonts.fonts = with pkgs; [
    fira-code
    fira-code-symbols
    # meslo-lgs-nf
  ];
}
