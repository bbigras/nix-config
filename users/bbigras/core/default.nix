{ config, lib, pkgs, ... }:
let
  my_cdda = pkgs.cataclysm-dda.withMods (mods: with mods; [
    tileset.UndeadPeople
    soundpack.Otopack
  ]);

  my_dwarf_fortress = pkgs.dwarf-fortress-packages.dwarf-fortress-full.override { theme = "vettlingr"; enableIntro = false; };
in
{
  imports = [
    ./git.nix
    ./email.nix
  ];

  programs.tmux = {
    enable = true;
    tmuxp.enable = true;
  };
  programs.aria2.enable = true;
  programs.bat.enable = true;
  programs.broot.enable = true;
  programs.pet = {
    enable = true;
    snippets = [
      {
        command = "curl ifconfig.co";
        description = "get my public ip";
        output = "127.0.0.1";
      }
      {
        command = "echo | openssl s_client -connect example.com:443 2>/dev/null |openssl x509 -dates -noout";
        description = "Show expiration date of SSL certificate";
      }
    ];
  };
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
  services.syncthing.enable = true;

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
      less = ''${pkgs.bat}/bin/bat --paging=always --pager "${pkgs.less}/bin/less -RF"'';
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
      socat
      websocat

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
      gitAndTools.git-machete
      gist
      gitAndTools.gh
      colordiff
      wrangler # cloudflare workers
      tcpdump

      networkmanager_dmenu

      # net
      croc
      ffsend
      qbittorrent
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
      manix

      # cool cli tools
      fd
      hexyl
      zenith
      dust
      procs
      hyperfine
      pwgen
      httpie
      rage
      navi # cheat sheet
      sd # find & replace
      pastel # color tool
      eva # calc
      bandwhich
      dogdns

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

      # rust
      cargo
      cargo-audit
      cargo-outdated
      # cargo-asm
      # cargo-bloat
      # cargo-crev
      cargo-expand
      cargo-flamegraph
      # cargo-fuzz
      # cargo-geiger
      cargo-sweep
      cargo-tarpaulin
      cargo-udeps

      # games
      my_cdda
      my_dwarf_fortress

      dust
      compsize

      jellyfin-mpv-shim
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
