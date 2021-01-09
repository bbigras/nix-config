{ pkgs, ... }:
{
  home = {
    # stateVersion = "20.03";
    packages = with pkgs; [
      peek

      gnome3.eog # image viewer
      # gnome3.gnome-tweaks
      gnome3.gnome-tweak-tool
      # tilix

      # extensions
      gnomeExtensions.appindicator
      gnomeExtensions.caffeine
      gnomeExtensions.clipboard-indicator
      gnomeExtensions.dash-to-dock
      # gnomeExtensions.gsconnect # kde connect
      gnomeExtensions.mpris-indicator-button
      gnomeExtensions.paperwm
    ];
  };

  programs.firefox = {
    enable = true;
    package = pkgs.firefox-wayland;
  };

  gtk = {
    enable = true;
    gtk2.extraConfig = "gtk-application-prefer-dark-theme = true";
    gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
  };

  qt = {
    enable = false;
    platformTheme = "gnome";
  };

  services.gpg-agent.pinentryFlavor = "gnome3";
  services.flameshot.enable = true;
  services.rsibreak.enable = true;
  services.unclutter.enable = true;
  services.gnome-keyring.enable = true;
}
