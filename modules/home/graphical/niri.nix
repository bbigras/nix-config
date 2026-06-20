{
  lib,
  pkgs,
  ...
}:

{
  services.gnome-keyring.enable = lib.mkForce false;

  home.packages = [
    pkgs.thunar
    pkgs.xwayland-satellite
  ];

  wayland.windowManager.niri = {
    enable = true;
    enableDefaultConfig = true;
    settings = {
      hotkey-overlay.skip-at-startup = true;
      input.keyboard.xkb = {
        layout = "ca";
        variant = "fr";
      };
    };
  };
}
