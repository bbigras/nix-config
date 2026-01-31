# Shared home-manager configuration for all platforms
# External modules (impermanence, nix-index-database, nixvim, stylix) are imported in configurations/
{
  flake,
  lib,
  # osConfig ? null,
  pkgs,
  ...
}:
let
  inherit (flake) self;
  # When integrated with NixOS/Darwin, osConfig is the parent system config
  # When standalone, osConfig is null
  # isIntegrated = osConfig != null;
in
{
  imports = with self.homeModules; [
    asciinema
    bash
    btop
    delta
    dev
    fish
    git
    htop
    jujutsu
    jjui
    ssh
    starship
    # syncthing
    television
    tmux
    xdg
    zsh
  ];

  # XXX: Manually enabled in the graphic module
  dconf.enable = false;

  catppuccin = {
    enable = false;
    flavor = "mocha";
    accent = "blue";
    cursors.enable = false;
  };

  gtk.enable = false;
  # home.pointerCursor = {
  #   x11.enable = false;
  #   gtk.enable = false;
  # };

  home = {
    stateVersion = lib.mkDefault "25.11";
    packages = lib.filter (lib.meta.availableOn pkgs.stdenv.hostPlatform) (
      with pkgs;
      [
        attic-client
        ccinit
        fd
        git-annex
        git-remote-gcrypt
        incus.client
        mosh
        nix-closure-size
        nix-output-monitor
        pwgen-secure
        resticprofile
        rsync
        truecolor-check
        viddy
        xh
        zrok
        # xournalpp
        # dbeaver-bin

        nix-closure-size
        mosh

        # dev
        colordiff

        # net
        dumbpipe
        croc
        rclone
        sendme
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
        sd # find & replace
        doggo

        # Android
        # android-studio
        # scrcpy

        # backup
        restic

        # utils
        file
        strace

        # rust
        cargo

        pv

        docker-compose
        prettyping # a nicer ping

        killall

        unar

        # kubernetes
        kubectl
        kubectx
        kubelogin-oidc
        # istioctl
        kubernetes-helm
        kind

        docker-credential-helpers
        viddy
        just

        # media
        mediainfo
        # kooha

        zrok

        # devpod
        # altair
        broot
        distrobox
        incus.client
        srgn
        attic-client
        # aider-chat
        git-annex
        git-remote-gcrypt
        spacer
        # faketty
        resticprofile
        tkey-ssh-agent
        # jan
        mergiraf
        # winboat
      ]
      ++ (with pkgs.hunspellDicts; [
        fr-moderne
        en_CA
      ])
    );
    shellAliases = {
      cat = "bat";
      cls = "clear";
      j = "${pkgs.just}/bin/just";
      l = "ls";
      la = "ls --all";
      ls = "eza --binary --header --long";
      man = "batman";
    };
  };

  programs = {
    atuin = {
      enable = true;
      settings.auto_sync = true;
      flags = [ "--disable-up-arrow" ];
    };
    bat = {
      enable = true;
      extraPackages = with pkgs.bat-extras; [ batman ];
    };
    carapace.enable = true;
    difftastic.enable = true;
    eza.enable = true;
    fastfetch.enable = true;
    fd.enable = true;
    fzf = {
      enable = true;
      tmux.enableShellIntegration = true;
    };
    gcc = {
      enable = true;
      colors = {
        error = "01;31";
      };
    };
    gpg.enable = true;
    jq.enable = true;
    kubecolor = {
      enable = true;
      enableAlias = true;
      settings = {
        preset = "dark";
        paging = "auto";
      };
    };
    kubeswitch = {
      enable = true;
    };
    # mangohud.enable = true;
    nh = {
      enable = true;
      flake = "git+https://github.com/bbigras/nix-config";
    };
    opencode = {
      enable = true;
      enableMcpIntegration = true;
      settings = {
        plugin = [ "@simonwjackson/opencode-direnv" ];
      };
    };
    mcp = {
      enable = true;
      servers = {
        context7 = {
          url = "https://mcp.context7.com/mcp";
          headers = {
            CONTEXT7_API_KEY = "{env:CONTEXT7_API_KEY}";
          };
        };
      };
    };
    pay-respects.enable = true;
    ripgrep.enable = true;
    sesh.enable = true;
    # streamlink = {
    #   enable = true;
    #   settings = {
    #     player = "${pkgs.mpv}/bin/mpv";
    #     player-args = "--cache 2048";
    #     player-no-close = true;
    #     twitch-disable-ads = true;
    #     twitch-low-latency = true;
    #   };
    # };
    # swappy = {
    #   enable = true;
    #   settings = {
    #     Default = {
    #       auto_save = false;
    #       custom_color = "rgba(193,125,17,1)";
    #       early_exit = false;
    #       fill_shape = false;
    #       line_size = 5;
    #       paint_mode = "brush";
    #       save_dir = "$HOME/Desktop";
    #       save_filename_format = "swappy-%Y%m%d-%H%M%S.png";
    #       show_panel = false;
    #       text_font = "sans-serif";
    #       text_size = 20;
    #       transparency = 50;
    #       transparent = false;
    #     };
    #   };
    # };
    trippy.enable = true;
    # vesktop = {
    #   enable = false;
    #   settings = {
    #     appBadge = false;
    #     arRPC = true;
    #     checkUpdates = false;
    #     customTitleBar = false;
    #     disableMinSize = true;
    #     minimizeToTray = false;
    #     tray = false;
    #     splashBackground = "#000000";
    #     splashColor = "#ffffff";
    #     splashTheming = true;
    #     staticTitle = true;
    #     hardwareAcceleration = true;
    #     discordBranch = "stable";
    #   };
    #   vencord.settings = {
    #     autoUpdate = false;
    #     autoUpdateNotification = false;
    #     notifyAboutUpdates = false;
    #     useQuickCss = true;
    #     disableMinSize = true;
    #     plugins = {
    #       # MessageLogger = {
    #       #   enabled = true;
    #       #   ignoreSelf = true;
    #       # };
    #       FakeNitro.enabled = true;
    #     };
    #   };
    # };
    vivid = {
      enable = true;
      activeTheme = "catppuccin-mocha";
      themes = {
        catppuccin-mocha = builtins.fetchurl {
          url = "https://raw.githubusercontent.com/sharkdp/Vivid/refs/heads/master/themes/catppuccin-mocha.yml";
          sha256 = "sha256:1hfwaf8lfq32w9vcdlbwrq5hwwz725i7icavg6qs66awpzqqb34k";
        };
      };
    };
    tealdeer.enable = true;
    zoxide.enable = true;
  };

  systemd.user.startServices = "sd-switch";

  services.tailscale-systray.enable = true;

  xdg.configFile."nixpkgs/config.nix".text = "{ allowUnfree = true; }";
}
