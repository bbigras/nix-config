{ hostType, impermanence, nix-index-database, pkgs, stylix, ... }: {
  imports = [
    impermanence.nixosModules.home-manager.impermanence
    nix-index-database.hmModules.nix-index
    stylix.homeManagerModules.stylix

    ./atuin.nix
    ./btop.nix
    ./git.nix
    ./emacs.nix
    ./ssh.nix
    ./tmux.nix
    ./xdg.nix
    ./zsh.nix
  ];

  # XXX: Manually enabled in the graphic module
  dconf.enable = false;

  home = {
    username = "bbigras";
    stateVersion = "22.11";
    packages = with pkgs; [
      libqalculate

      mosh

      ripgrep

      # dev
      bfg-repo-cleaner
      gitAndTools.git-machete
      git-filter-repo
      gist
      gitAndTools.gh
      colordiff

      # net
      croc

      rclone
      tailscale
      tcpdump
      webwormhole
      xh

      # nix
      cachix
      comma
      manix
      nix-update
      nixpkgs-fmt

      # cool cli tools
      fd
      hexyl
      procs
      pwgen
      sd # find & replace
      bandwhich
      dogdns
      btop

      # Android
      # android-studio
      # scrcpy

      # security?
      bitwarden-cli

      # backup
      restic
      # kopia

      # gist gopass  weechat

      # utils
      file
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


      compsize

      pv

      # asciinema # record the terminal
      docker-compose # docker manager
      ncdu # disk space info (a better du)
      prettyping # a nicer ping
      rnix-lsp # nix lsp server

      feh # light-weight image viewer
      killall

      unar
      ntp

      # Go
      # go
      # gopls

      # kubernetes
      k9s
      kdash
      kubectl
      kubectx
      kubelogin-oidc
      # istioctl
      kubernetes-helm
      kind

      # perf
      sysstat

      ventoy
      docker-credential-helpers
      viddy
      beets-unstable
      natscli
      just

      yt-dlp
      zrok
      aichat
      quickemu
      mediainfo

      git-annex
      git-remote-gcrypt
      qalculate-gtk
    ];
    shellAliases = {
      cat = "${pkgs.bat}/bin/bat";
      cls = "clear";
      j = "${pkgs.just}/bin/just";
      ".j" = "${pkgs.just}/bin/just --justfile ~/.user.justfile";
      less = ''${pkgs.bat}/bin/bat --paging=always --pager "${pkgs.less}/bin/less -RF"'';
      man = "${pkgs.bat-extras.batman}/bin/batman";
    };
  };

  programs = {
    aria2.enable = true;
    atuin = {
      enable = true;
      settings.auto_sync = true;
    };
    bat.enable = true;
    exa = {
      enable = true;
      enableAliases = true;
    };
    jq.enable = true;
    fzf = {
      enable = true;
      tmux.enableShellIntegration = true;
      tmux.shellIntegrationOptions = [ "-d 40%" ];
      defaultCommand = "fd --type f";
      defaultOptions = [
        "--height 40%"
        "--border"
      ];
      changeDirWidgetCommand = "fd --type d"; # alt+c
      changeDirWidgetOptions = [
        "--preview 'tree -C {} | head -200'"
      ];
      fileWidgetCommand = "fd --type f";
      fileWidgetOptions = [
        "--preview 'head {}'"
      ];
      colors = {
        bg = "#1e1e1e";
        "bg+" = "#1e1e1e";
        fg = "#d4d4d4";
        "fg+" = "#d4d4d4";
      };
    };
    gpg.enable = true;
    navi.enable = true;
    nix-index.enable = true;
    sqls.enable = true;

    bashmount.enable = true;

    ssh = {
      enable = true;
      hashKnownHosts = true;

      extraOptionOverrides = {
        AddKeysToAgent = "confirm";
        VerifyHostKeyDNS = "ask";
      };
    };
    tealdeer.enable = true;
    zoxide.enable = true;
    nushell.enable = false;
    zellij.enable = true;
  };

  services = {
    # spotifyd.enable = true;
    syncthing.enable = true;
    systembus-notify.enable = true;
  };

  home.language.base = "fr_CA.UTF-8";

  stylix = {
    base16Scheme = "${pkgs.base16-schemes}/share/themes/tomorrow-night.yaml";
    image = pkgs.nixos-artwork.wallpapers.simple-dark-gray.gnomeFilePath;
    targets.gnome.enable = hostType == "nixos";
    targets.gtk.enable = hostType == "nixos";
  };

  systemd.user.startServices = "sd-switch";

  xdg.configFile."nixpkgs/config.nix".text = "{ allowUnfree = true; }";
}
