{ config, lib, pkgs, ... }:
let
  nix-matrix-yggdrasil_src = (import ../../../nix).nix-matrix-yggdrasil;
  nix-matrix-yggdrasil = import nix-matrix-yggdrasil_src { };

  neuron = (import (import ../../../nix).neuron { });
in
{
  imports = [
    ./git.nix
    ./email.nix
  ];

  programs.aria2.enable = true;
  programs.bat.enable = true;
  programs.broot.enable = true;
  programs.command-not-found.enable = true;
  programs.direnv.enable = true;
  programs.fzf.enable = true;
  programs.htop.enable = true;
  programs.jq.enable = true;
  programs.starship.enable = true;
  programs.vscode.enable = true;
  programs.zoxide.enable = true;

  services.dropbox.enable = true;
  services.lorri.enable = true;
  services.spotifyd.enable = true;

  programs.alacritty = {
    enable = true;
    settings = {
      env = {
        TERM = "xterm-256color";
      };
    };
  };

  programs.fish = {
    enable = true;
    shellAliases = {
      cat = "${pkgs.bat}/bin/bat";
      ls = "${pkgs.exa}/bin/exa";
    };
  };

  programs.ssh = {
    enable = true;
    controlMaster = "auto";
    controlPersist = "10m";

    extraOptionOverrides = {
      AddKeysToAgent = "confirm";
      VerifyHostKeyDNS = "ask";
    };
  };

  home = {
    stateVersion = "20.03";
    packages = with pkgs; [
      mosh
      neofetch

      ripgrep
      tealdeer

      google-chrome
      remmina

      # espeak
      tmuxinator
      # socat
      # websocat

      # notes
      # joplin-desktop

      # media
      handbrake
      vlc
      gimp
      mediainfo

      # games
      lutris

      # comm
      discord

      # dev
      gitAndTools.git-absorb
      gitAndTools.gitui
      gitAndTools.hub
      gist
      colordiff
      wrangler # cloudflare workers
      tcpdump

      # net
      croc
      ffsend
      qbittorrent
      syncthing
      # youtube-dl
      wireshark
      dnsutils
      riot-desktop
      # oneshot

      # nix
      cachix
      nix-index
      nix-prefetch
      nix-prefetch-scripts
      nix-prefetch-github
      nix-review
      nix-update
      nixpkgs-fmt
      nixos-shell

      # cool cli tools
      fd
      hexyl
      zenith
      dust
      procs
      hyperfine
      ytop
      pwgen
      httpie

      # Android
      # android-studio
      scrcpy

      # security?
      bitwarden-cli

      # pour Laptop
      powerstat

      # dejavu_fonts
      font-awesome

      restic

      # exa gist gopass  weechat

      # utils
      file

      neuron
    ];
  };

  systemd.user.services.matrix-yggdrasil = {
    Unit = {
      Description = "matrix-yggdrasil";
    };

    Install = {
      WantedBy = [ "default.target" ];
    };

    Service = {
      ExecStart = "${nix-matrix-yggdrasil}/bin/dendrite-demo-yggdrasil -peer tcp://127.0.0.1:9977";
      WorkingDirectory = "/home/bbigras/matrix-p2p";
      Restart = "on-failure";
      PrivateTmp = true;
      ProtectSystem = "full";
      Nice = 10;
    };
  };

  home.sessionVariables = {
    # BROWSER = "${pkgs.google-chrome}/bin/google-chrome-stable";
    # ALTERNATE_EDITOR = "";
    # EDITOR = "emacsclient -t"; # $EDITOR opens in terminal
    # VISUAL = "emacsclient -c -a emacs"; # $VISUAL opens in GUI mode
    # EDITOR = "emacs";
    # GS_OPTIONS = "-sPAPERSIZE=letter";
    # ASPELL_CONF = "data-dir /home/bbigras/.nix-profile/lib/aspell";
    # RUSTC_WRAPPER = "${pkgs.sccache}/bin/sccache";
    GOPATH = "$HOME/go";
  };

  home.language.base = "fr_CA.UTF-8";
}
