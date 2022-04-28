{ pkgs, ... }: {
  imports = [
    ./boot-silent.nix
    ./fonts.nix
    # ./greetd.nix
  ];

  programs.dconf.enable = true;

  services.dbus.packages = with pkgs; [ dconf ];
  services.gnome.at-spi2-core.enable = true;
}
