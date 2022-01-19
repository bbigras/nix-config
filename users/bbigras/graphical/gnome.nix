{ pkgs, ... }:
{
  home = {
    packages = with pkgs; [
      peek

      gnome3.eog # image viewer
      gnome3.gnome-tweaks
      # tilix

      # extensions
      gnomeExtensions.appindicator
      gnomeExtensions.caffeine
      gnomeExtensions.clipboard-indicator
    ];
  };

  programs.mpv = {
    enable = true;
    config = {
      profile = "gpu-hq";
      # gpu-context = "wayland";
      vo = "gpu";
      hwdec = "auto";
    };
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
