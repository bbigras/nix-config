{ config, lib, pkgs, ... }:
let
  nix-matrix-yggdrasil = (import (import ../../../nix).nix-matrix-yggdrasil);

  neuron = (import (import ../../../nix).neuron { });
  crate2nix = (import (import ../../../nix).crate2nix { });

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
  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
    stdlib = ''
      layout_postgres() {
        export PGDATA="$(direnv_layout_dir)/postgres"
        export PGHOST="$PGDATA"

        if [[ ! -d "$PGDATA" ]]; then
          initdb
          cat >> "$PGDATA/postgresql.conf" <<EOF
listen_addresses = ''\'''\'
unix_socket_directories = ''\'$PGHOST''\'
EOF
          echo "CREATE DATABASE $USER;" | postgres --single -E postgres
        fi
      }
    '';
  };
  programs.htop.enable = true;
  programs.jq.enable = true;
  programs.starship.enable = true;
  programs.vscode.enable = true;
  programs.zoxide.enable = true;
  programs.mcfly.enable = true;

  services.dropbox.enable = true;
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
    hashKnownHosts = true;

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
      # lutris

      # twitch
      streamlink
      chatterino2

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

      networkmanager_dmenu

      # net
      croc
      ffsend
      qbittorrent
      syncthing
      # youtube-dl
      wireshark
      dnsutils
      element-desktop
      oneshot
      tailscale

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
      rnix-lsp

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

      # backup
      restic
      kopia

      # exa gist gopass  weechat

      # utils
      file
      tcpdump
      strace

      neuron

      # rust
      crate2nix
      cargo
      cargo-audit
      cargo-outdated
      # cargo-asm
      # cargo-bloat
      # cargo-crev
      cargo-expand
      cargo-flamegraph
      # cargo-fuzz
      cargo-geiger
      cargo-sweep
      cargo-tarpaulin
      cargo-udeps

      # games
      my_cdda
      dwarf-fortress-packages.dwarf-fortress-full

      tmux
      dust
      compsize

      jellyfin-mpv-shim
      freecad
      pv
      rclone

      any-nix-shell # fish support for nix shell
      asciinema # record the terminal
      docker-compose # docker manager
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
      ExecStart = "${nix-matrix-yggdrasil}/bin/dendrite-demo-yggdrasil -peer tcp://64.112.177.94:1617";
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
