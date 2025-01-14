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
      chatterino2
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
    # kdeconnect.enable = true;
    remmina.enable = true;
  };

  programs = {
    chromium.enable = true;
    ghostty.enable = true;
    mangohud.enable = true;
  };
}
