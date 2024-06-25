{ pkgs, ... }: {
  networking.nftables.enable = true;
  networking.firewall = {
    extraCommands = pkgs.lib.mkForce "";
  };
  virtualisation = {
    incus = {
      enable = true;
    };
  };
}
