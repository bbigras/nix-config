{ pkgs, ... }: {
  imports = [ ./documentation.nix ];

  environment = {
    enableDebugInfo = true;
    systemPackages = with pkgs; [ git tmate upterm nixpkgs-review nix-update ];
  };

  programs = {
    wireshark.enable = true;
  };
}
