{ hostType, pkgs, ... }: {
  imports = [
    (
      if hostType == "nixos" || hostType == "homeManager" then ./linux.nix
      else if hostType == "darwin" then ./darwin.nix
      else throw "Unknown hostType '${hostType}' for users/bbigras/graphical"
    )
    ./kitty.nix
  ];

  home.packages = with pkgs; [
    element-desktop
    libnotify
    qalculate-gtk
    xdg-utils

    remmina


    # media
    pavucontrol

    # games
    # lutris

    # twitch
    # streamlink
    chatterino2

    qbittorrent
    wireshark

    # games
    # starsector
    mangohud
    # heroic

    dbeaver-bin
    josm


    # remote

    # anytype

    joplin-desktop

    xournalpp

  ] ++ lib.filter (lib.meta.availableOn stdenv.hostPlatform) [
    # iterm2
    # ledger-live-desktop
    # plexamp
    # signal-desktop
    # thunderbird
  ];

  services = {
    # kdeconnect.enable = true;
  };
}
