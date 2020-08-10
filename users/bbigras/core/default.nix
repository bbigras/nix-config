{ config, lib, pkgs, ... }:
let
  nix-matrix-yggdrasil_src = (import ../../../nix).nix-matrix-yggdrasil;
  nix-matrix-yggdrasil = import nix-matrix-yggdrasil_src { };

  neuron = (import (import ../../../nix).neuron { });

  my_cdda = pkgs.cataclysm-dda.withMods (mods: with mods; [
    tileset.UndeadPeople
  ]);
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
    promptInit = ''
      eval (direnv hook fish)
      any-nix-shell fish --info-right | source
    '';
    shellAliases = {
      cat = "${pkgs.bat}/bin/bat";
      ls = "${pkgs.exa}/bin/exa";
    };
    shellInit = ''
      set -x FZF_DEFAULT_OPTS "--preview='bat {} --color=always'" \n
      set -x SKIM_DEFAULT_COMMAND "rg --files || fd || find ."

      set -g theme_display_date no
      set -g theme_nerd_fonts yes
      set -g theme_display_git_master_branch no
      set -g theme_nerd_fonts yes
      set -g theme_newline_cursor yes
      set -g theme_color_scheme solarized

      bind \t accept-autosuggestion
      set fish_greeting
    '';
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
      joplin-desktop

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
      element-desktop
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
      tcpdump

      neuron

      my_cdda

      tmux
      dust

      any-nix-shell # fish support for nix shell
      asciinema # record the terminal
      # docker-compose # docker manager
      ncdu # disk space info (a better du)
      prettyping # a nicer ping
      rnix-lsp # nix lsp server
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
    BROWSER = "firefox";
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
