{ pkgs, ... }:
{
  imports = [
    ./alacritty.nix
  ];

  home = {
    packages = with pkgs; [
      # peek

      gnome.eog # image viewer
      gnome.gnome-tweaks
      # tilix

      # extensions
      gnomeExtensions.appindicator
      gnomeExtensions.caffeine
      gnomeExtensions.clipboard-indicator
      # gnomeExtensions.just-perfection
      # gnomeExtensions.pop-shell
      gnomeExtensions.forge
      gnomeExtensions.tiling-assistant
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

  services = {
    flameshot.enable = true;
    gnome-keyring.enable = true;
    gpg-agent.pinentryFlavor = "gnome3";
    rsibreak.enable = true;
    unclutter.enable = true;
  };
}
