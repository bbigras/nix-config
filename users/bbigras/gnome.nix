{ pkgs, ... }:
{
  home = {
    # stateVersion = "20.03";
    packages = with pkgs; [
      flameshot
      peek
      gnomeExtensions.mpris-indicator-button
      gnomeExtensions.paperwm
      gnomeExtensions.dash-to-dock
      gnomeExtensions.caffeine
      gnomeExtensions.clipboard-indicator
      # gnomeExtensions.gsconnect # kde connect
      gnome3.gnome-tweaks
      tilix
    ];
  };

  programs.firefox = {
    enable = true;
    package = pkgs.latest.firefox-nightly-bin;
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
}
