{ pkgs, ... }: {
  imports = [ ./documentation.nix ];

  environment = {
    enableDebugInfo = true;
    systemPackages = with pkgs; [ git tmate upterm nixpkgs-review nix-update ];
  };

  programs = {
    wireshark.enable = true;
  };

  services.dictd.enable = true;

  services.udev.extraRules = ''
    ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="0660", GROUP="dialout", TAG+="uaccess"
  '';
}
