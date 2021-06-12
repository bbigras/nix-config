{ pkgs, ... }:
{
  home = {
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

  gtk = {
    enable = true;
    gtk2.extraConfig = "gtk-application-prefer-dark-theme = true";
    gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
  };

  # qt = {
  #   enable = false;
  #   platformTheme = "gnome";
  # };

  services = {
    flameshot.enable = true;
    gnome-keyring.enable = true;
    gpg-agent.pinentryFlavor = "gnome3";
    rsibreak.enable = true;
    unclutter.enable = true;
  };
}
