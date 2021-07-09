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
    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
        enableFlakes = true;
      };
      stdlib = builtins.readFile ./direnv.cfg;
    };
    exa = {
      enable = true;
      enableAliases = true;
    };
    htop.enable = true;
    jq.enable = true;
    mcfly.enable = true;
    nix-index.enable = true;
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
      terminal = "screen-256color";
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
          name = "zsh-autosuggestions";
          src = pkgs.zsh-autosuggestions;
        }
        {
          name = "zsh-syntax-highlighting";
          src = pkgs.zsh-syntax-highlighting;
        }
        {
          name = "zsh-history-substring-search";
          src = pkgs.zsh-history-substring-search;
        }
        {
          name = "zsh-completions";
          src = pkgs.zsh-completions;
        }
        {
          name = "powerlevel10k-config";
          src = lib.cleanSource ./p10k-config;
          file = "p10k.zsh";
        }
        # {
        #   name = "async";
        #   src = pkgs.zsh-async;
        # }
        {
          name = "zsh-you-should-use";
          src = pkgs.zsh-you-should-use;
        }
      ];

      shellAliases = {
        cat = "${pkgs.bat}/bin/bat";
        less = ''${pkgs.bat}/bin/bat --paging=always --pager "${pkgs.less}/bin/less -RF"'';
      };

      initExtra = ''
        ${pkgs.any-nix-shell}/bin/any-nix-shell zsh | source /dev/stdin
        if [[ "$TERM" != 'dumb' && -z "$INSIDE_EMACS" ]]; then
          source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
          [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
        fi
      '';

      initExtraFirst = ''
        DIRSTACKSIZE=10
        setopt   notify globdots correct cdablevars autolist
        setopt   correctall autocd recexact longlistjobs
        setopt   autoresume
        setopt   rcquotes mailwarning
        unsetopt bgnice
        setopt   autopushd pushdminus pushdsilent pushdtohome pushdignoredups
        setopt COMPLETE_IN_WORD    # Complete from both ends of a word.
        setopt ALWAYS_TO_END       # Move cursor to the end of a completed word.
        setopt AUTO_MENU           # Show completion menu on a successive tab press.
        setopt AUTO_LIST           # Automatically list choices on ambiguous completion.
        setopt EXTENDED_GLOB       # Needed for file modification glob modifiers with compinit
        unsetopt AUTO_PARAM_SLASH    # If completed parameter is a directory, do not add a trailing slash.
        unsetopt MENU_COMPLETE     # Do not autoselect the first completion entry.
        unsetopt FLOW_CONTROL      # Disable start/stop characters in shell editor.
      '';

      localVariables = {
        # This way, C-w deletes words (path elements)
        WORDCHARS = "*?_-.[]~&;!#$%^(){}<>";

        ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=8";
      };

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
      webwormhole
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

      # gist gopass  weechat

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
      obsidian
      cloudflared
      tmate
      zerotierone
      josm

      # Go
      go
      gopls
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
