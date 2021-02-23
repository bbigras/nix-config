{ pkgs, lib, ... }:
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

  systemd.user.services.node-red = {
    Unit = {
      Description = "node-red";
      After = [ "network.target" ];
    };

    Service = {
      ExecStart = "${pkgs.nodePackages.node-red}/bin/node-red";
    };
  };

  programs = {
    alacritty = {
      enable = true;
      settings = {
        env = {
          TERM = "xterm-256color";
        };
        font.normal.family = lib.mkDefault "MesloLGS NF";
      };
    };

    aria2.enable = true;
    bat.enable = true;
    broot.enable = true;
    command-not-found.enable = true;
    direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
      stdlib = builtins.readFile ./direnv.cfg;
    };
    htop.enable = true;
    jq.enable = true;
    mcfly.enable = true;
    pet = {
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

    ssh = {
      enable = true;
      controlMaster = "auto";
      controlPersist = "10m";
      hashKnownHosts = true;

      extraOptionOverrides = {
        AddKeysToAgent = "confirm";
        VerifyHostKeyDNS = "ask";
      };
    };

    tmux = {
      enable = true;
      tmuxp.enable = true;
    };
    vscode.enable = true;
    zoxide.enable = true;

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      enableVteIntegration = true;
      # dirHashes = {
      #   docs = "$HOME/Documents";
      #   vids = "$HOME/Videos";
      #   dl = "$HOME/Downloads";
      # };
      plugins = [
        {
          name = "powerlevel10k";
          src = pkgs.zsh-powerlevel10k;
          file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
        }
        {
          name = "powerlevel10k-config";
          src = lib.cleanSource ./p10k-config;
          file = "p10k.zsh";
        }
      ];

      shellAliases = {
        cat = "${pkgs.bat}/bin/bat";
        ls = "${pkgs.exa}/bin/exa";
        less = ''${pkgs.bat}/bin/bat --paging=always --pager "${pkgs.less}/bin/less -RF"'';
      };

      initExtra = ''
        ${pkgs.any-nix-shell}/bin/any-nix-shell zsh | source /dev/stdin
      '';
    };

  };

  services = {
    dropbox.enable = true;
    spotifyd.enable = true;
    syncthing.enable = true;
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
      pavucontrol

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
      git-filter-repo
      gist
      gitAndTools.gh
      colordiff
      wrangler # cloudflare workers
      tcpdump
      radicle-upstream

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
      ht-rust

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
      # dust
      procs
      hyperfine
      pwgen
      rage
      navi # cheat sheet
      sd # find & replace
      pastel # color tool
      eva # calc
      bandwhich
      dogdns

      vault
      awscli2
      terraform_0_14

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

      compsize

      pv
      rclone

      asciinema # record the terminal
      docker-compose # docker manager
      ncdu # disk space info (a better du)
      prettyping # a nicer ping
      rnix-lsp # nix lsp server

      the-powder-toy
      dragon-drop
      feh # light-weight image viewer
      killall
      ghidra-bin

      unar
      tab-rs
      barrier
      dbeaver
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
