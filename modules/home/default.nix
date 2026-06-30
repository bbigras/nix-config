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

  nixCleanScript = pkgs.writers.writeBash "nix-clean" ''
    sudo sh <<'EOF'
    nix-env -p /nix/var/nix/profiles/system --list-generations \
      | awk '{print $1, $2}' \
      | sort -k2,2 -k1,1rn \
      | awk 'seen[$2]++ {print $1}' \
      | xargs -r nix-env -p /nix/var/nix/profiles/system --delete-generations
    EOF
  '';
in
{
  imports = with self.homeModules; [
    asciinema
    bash
    btop
    delta
    dev
    docker
    fish
    git
    htop
    jujutsu
    jjui
    ssh
    starship
    syncthing
    television
    tmux
    xdg
    zsh
  ];

  # XXX: Manually enabled in the graphic module
  dconf.enable = false;

  catppuccin = {
    enable = true;
    autoEnable = true;
    flavor = "mocha";
    accent = "blue";
    cursors.enable = true;
  };

  gtk.enable = true;
  home.pointerCursor = {
    x11.enable = true;
    gtk.enable = true;
  };

  home = {
    stateVersion = lib.mkDefault "26.05";
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
        rsync
        truecolor-check
        viddy
        xh
        zrok
        xournalpp
        dbeaver-bin

        nix-closure-size
        mosh

        # dev
        colordiff

        # net
        dumbpipe
        rclone
        sendme
        tailscale
        tcpdump
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
        scrcpy

        # backup
        restic

        # utils
        file
        strace

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
        natscli
        just

        # media
        mediainfo
        kooha

        zrok

        devpod
        altair
        broot
        distrobox
        incus.client
        srgn
        attic-client
        # aider-chat
        git-annex
        git-remote-gcrypt
        spacer
        faketty
        jan
        hl-log-viewer
        android-tools
        resticprofile
        openbao
        nmap
        ctx7
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
      nix-clean = "${nixCleanScript}";
      nh-clean = "nh clean all --keep-since 7d --keep 4 --no-gc";
    };
  };

  programs = {
    alacritty.enable = true;
    atool = {
      enable = true;
      extraPackages = with pkgs; [
        bzip2
        gnutar
        gzip
        lhasa
        lzop
        p7zip
        unzip
        xz
        zip
      ];
    };
    atuin = {
      enable = true;
      settings.auto_sync = true;
      flags = [ "--disable-up-arrow" ];
      settings = {
        sync.records = true;
        cwd_filter = [
          "^/tmp"
          "^/var/tmp"
          "^/run"
        ];
        history_filter = [
          # Leading space (manual "don't record this" convention)
          "^ "

          # Secrets passed as flags
          ".*--password[= ]"
          ".*--passwd[= ]"
          ".*--secret[= ]"
          ".*--token[= ]"
          ".*--api-key[= ]"

          # Inline env var assignments with secrets
          ".*API_KEY="
          ".*SECRET="
          ".*PASSWORD="
          ".*TOKEN="

          # Specific noisy/useless commands
          "^ls$"
          "^cd$"
          "^pwd$"
          "^exit$"
          "^clear$"
          "^history$"
        ];
      };
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
    grype = {
      enable = true;
      settings = {
        check-for-app-update = false;
      };
    };
    jq.enable = true;
    k9s = {
      enable = true;
      aliases = {
        # Use pp as an alias for Pod
        pp = "v1/pods";
      };
      hotKeys = {
        shift-0 = {
          shortCut = "Shift-0";
          description = "Viewing pods";
          command = "pods";
        };
      };
      plugins = {
        # Defines a plugin to provide a `ctrl-l` shortcut to
        # tail the logs while in pod view.
        fred = {
          shortCut = "Ctrl-L";
          description = "Pod logs";
          scopes = [ "po" ];
          command = "kubectl";
          background = false;
          args = [
            "logs"
            "-f"
            "$NAME"
            "-n"
            "$NAMESPACE"
            "--context"
            "$CLUSTER"
          ];
        };
        jsonLogs = {
          shortCut = "Shift-L";
          description = "Prettified json logs";
          scopes = [
            "daemonset"
            "deploy"
            "job"
            "pod"
            "replicaset"
            "service"
            "statefulset"
          ];
          command = "sh";
          args = [
            "-c"
            ''
              kubectl logs --follow --context="$CONTEXT" --namespace="$NAMESPACE" "$RESOURCE_NAME/$NAME" | ${lib.getExe pkgs.hl-log-viewer} --paging=never
            ''
          ];
        };
      };
      settings = {
        k9s = {
          refreshRate = 2;
        };
      };
      views = {
        "v1/pods" = {
          columns = [
            "AGE"
            "NAMESPACE"
            "NAME"
            "IP"
            "NODE"
            "STATUS"
            "READY"
          ];
        };
      };
    };
    qalculate = {
      enable = true;
      package = pkgs.qalculate-gtk;
      settings = {
        General = {
          precision = 10;
          colorize = 1;
          save_mode_on_exit = 1;
          save_definitions_on_exit = 0;
        };
        Mode = {
          angle_unit = 1;
          number_base = 10;
          min_deci = 0;
          max_deci = -1;
        };
      };
    };
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
    mangohud.enable = true;
    nh = {
      enable = true;
      flake = "git+https://github.com/bbigras/nix-config";
    };
    mcp = {
      enable = true;
    };
    pay-respects.enable = true;
    ripgrep.enable = true;
    sesh.enable = true;
    streamlink = {
      enable = true;
      settings = {
        player = "${pkgs.mpv}/bin/mpv";
        player-args = "--cache 2048";
        player-no-close = true;
        twitch-disable-ads = true;
        twitch-low-latency = true;
      };
    };
    swappy = {
      enable = true;
      settings = {
        Default = {
          auto_save = false;
          custom_color = "rgba(193,125,17,1)";
          early_exit = false;
          fill_shape = false;
          line_size = 5;
          paint_mode = "brush";
          save_dir = "$HOME/Desktop";
          save_filename_format = "swappy-%Y%m%d-%H%M%S.png";
          show_panel = false;
          text_font = "sans-serif";
          text_size = 20;
          transparency = 50;
          transparent = false;
        };
      };
    };
    tirith = {
      enable = false;
      policy = {
        version = 1;
        fail_mode = "open";
        allow_bypass = true;
        severity_overrides = {
          docker_untrusted_registry = "CRITICAL";
        };
      };
    };
    npm.enable = true;
    codex = {
      enable = true;
      enableMcpIntegration = true;
    };
    trippy.enable = true;
    equibop = {
      enable = true;
      settings = {
        appBadge = false;
        arRPC = true;
        checkUpdates = false;
        customTitleBar = false;
        disableMinSize = true;
        minimizeToTray = false;
        tray = false;
        splashBackground = "#000000";
        splashColor = "#ffffff";
        splashTheming = true;
        staticTitle = true;
        hardwareAcceleration = true;
        discordBranch = "stable";
      };
      equicord.settings = {
        autoUpdate = false;
        autoUpdateNotification = false;
        notifyAboutUpdates = false;
        useQuickCss = true;
        disableMinSize = true;
        plugins = {
          # MessageLogger = {
          #   enabled = true;
          #   ignoreSelf = true;
          # };
          FakeNitro.enabled = true;
        };
      };
    };
    vivid.enable = true;
    tealdeer.enable = true;
    zoxide.enable = true;
  };

  systemd.user.startServices = "sd-switch";

  services.tailscale-systray.enable = true;

  xdg.configFile."nixpkgs/config.nix".text = "{ allowUnfree = true; }";
}
