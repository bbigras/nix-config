{ hostType, pkgs, ... }:
{
  imports = [
    (
      if hostType == "nixos" || hostType == "homeManager" then
        ./linux.nix
      else if hostType == "darwin" then
        ./darwin.nix
      else
        throw "Unknown hostType '${hostType}' for users/bbigras/graphical"
    )
  ];

  home.packages =
    with pkgs;
    [
      dbeaver-bin
      element-desktop
      libnotify
      pavucontrol
      qalculate-gtk
      qbittorrent
      wireshark
      xdg-utils
      xournalpp
    ]
    ++ lib.filter (lib.meta.availableOn stdenv.hostPlatform) [
      # iterm2
      # ledger-live-desktop
      # plexamp
      # signal-desktop
      # thunderbird
    ];

  services = {
    kdeconnect = {
      enable = true;
      indicator = true;
    };
    remmina.enable = true;
  };

  programs = {
    alacritty.enable = true;
    chromium.enable = true;
    ghostty.enable = true;
    mangohud.enable = true;
  };
}
