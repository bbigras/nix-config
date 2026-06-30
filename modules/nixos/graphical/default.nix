{ lib, pkgs, ... }:

{
  # silent boot for plymouth
  boot = {
    consoleLogLevel = 0;
    kernelParams = [
      "quiet"
      "udev.log_level=3"
    ];
  };

  programs.dconf.enable = true;
  programs.wireshark.enable = false;

  environment.systemPackages = [
    (lib.hiPrio (pkgs.writeShellScriptBin "flatpak" ''
      exec ${pkgs.coreutils}/bin/env -u PATH ${pkgs.flatpak}/bin/flatpak "$@"
    ''))
  ];

  services = {
    envfs.enable = true;

    # displayManager.gdm = {
    #   enable = true;
    #   autoSuspend = true;
    #   wayland = true;
    # };
    # gnome.at-spi2-core.enable = true;
    # xserver.enable = true;
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
    config.common.default = "*";
  };
}
