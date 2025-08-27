{
  pkgs,
  nur,
  catppuccin,
  ...
}:

let
  nurNoPkgs = import nur {
    pkgs = null;
    nurpkgs = pkgs;
  };
in
{
  user.shell = "${pkgs.zsh}/bin/zsh";

  user = {
    gid = 10160;
    uid = 10160;
  };

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  # Simply install just the packages
  environment.packages = with pkgs; [
    # User-facing stuff that you really really want to have
    vim # or some other editor, e.g. nano or neovim

    # Some common stuff that people expect to have
    diffutils
    findutils
    tzdata
    hostname
    man
    gawk
    gnugrep
    gnupg
    gnused
    gnutar
    bzip2
    gzip
    xz
    zip
    unzip
    dnsutils
    which
  ];

  # Backup etc files instead of failing to activate generation if a file already exists in /etc
  environment.etcBackupExtension = ".bak";

  environment.motd = null;
  environment.etc = {
    "tmp-sshd".text = ''
      HostKey /data/data/com.termux.nix/files/home/ssh_host_ed25519_key
      Port 8022
    '';
  };
  environment.sessionVariables = {
  };

  android-integration = {
    am.enable = true;
    termux-open.enable = true;
    termux-open-url.enable = true;
    termux-setup-storage.enable = true;
    xdg-open.enable = true;
  };

  terminal = {
    font = "${pkgs.nerd-fonts.iosevka}/share/fonts/truetype/NerdFonts/IosevkaNerdFontMono-Regular.ttf";
    colors = {
      background = "#1e1e2e";
      foreground = "#cdd6f4";

      color0 = "#45475a";
      color8 = "#585b70";

      color1 = "#f38ba8";
      color9 = "#f38ba8";

      color2 = "#a6e3a1";
      color10 = "#a6e3a1";

      color3 = "#f9e2af";
      color11 = "#f9e2af";

      color4 = "#89b4fa";
      color12 = "#89b4fa";

      color5 = "#f5c2e7";
      color13 = "#f5c2e7";

      color6 = "#94e2d5";
      color14 = "#94e2d5";

      color7 = "#bac2de";
      color15 = "#a6adc8";
    };
  };

  time.timeZone = "America/Montreal";

  # Read the changelog before changing this value
  system.stateVersion = "20.09";

  # After installing home-manager channel like
  #   nix-channel --add https://github.com/rycee/home-manager/archive/release-20.09.tar.gz home-manager
  #   nix-channel --update
  # you can configure home-manager in here like

  home-manager.useGlobalPkgs = true;

  home-manager.config =
    { pkgs, lib, ... }:
    {
      # Read the changelog before changing this value
      home.stateVersion = "20.09";

      imports = [
        ../../users/bbigras/core/atuin.nix
        ../../users/bbigras/core/git.nix
        #../../users/bbigras/core/jujutsu.nix
        ../../users/bbigras/core/tmux.nix
        ../../users/bbigras/core/yazi.nix
        ../../users/bbigras/core/zsh.nix
        # ../../users/bbigras/core/emacs
        nurNoPkgs.repos.rycee.hmModules.emacs-init
        catppuccin.homeModules.catppuccin
      ]
      ++ (
        if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/pixel6.nix") then
          [ (builtins.getEnv "PWD" + "/secrets/pixel6.nix") ]
        else
          [ ]
      );

      # Use the same overlays as the system packages
      # nixpkgs.overlays = config.nixpkgs.overlays;

      home.language.base = "fr_CA.UTF-8";

      catppuccin = {
        enable = true;
        flavor = "mocha";
        accent = "blue";
      };

      # insert home-manager config
      programs = {
        atuin = {
          enable = true;
          settings.auto_sync = true;
        };
        bat = {
          enable = true;
        };
        carapace.enable = true;
        command-not-found.enable = true;
        direnv = {
          enable = true;
          silent = true;
          nix-direnv.enable = true;
        };
        emacs = {
          enable = true;
          init.usePackage = {
            god-mode.enable = true;
          };
        };
        eza.enable = true;
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
        };
        jq.enable = true;
        htop.enable = true;
        ripgrep.enable = true;
        pay-respects.enable = true;
        ssh = {
          enable = true;
          controlMaster = "auto";
          controlPersist = "10m";
          hashKnownHosts = true;
          includes = [
            "~/.ssh/devpod"
          ];
          extraOptionOverrides = {
            AddKeysToAgent = "confirm";
            VerifyHostKeyDNS = "ask";
          };
        };
        starship.enable = true;
        tealdeer.enable = true;
        zoxide.enable = true;
        zed-editor.enable = true;
        fish = {
          shellAliases = {
            ssh-server = "${pkgs.openssh}/bin/sshd -f /etc/tmp-sshd";
          };
        };
      };

      home.packages = with pkgs; [
        libqalculate
        cachix
        croc
        sendme
        dumbpipe
        doggo
        fd
        hyperfine
        just
        mosh
        (neofetch.override { x11Support = false; })
        oneshot
        pwgen
        #rage
        xh
        openssh
        prettyping
        # tab-rs
        kubernetes
        kubernetes-helm
        k9s
        kubie
        kubectl
        kubectx
        kubelogin-oidc
        # socat
        # websocat
        git-annex
        devpod
        natscli
        viddy
        zrok
        restic
        incus.client
        glasskube
        talosctl
        nix-output-monitor
        attic-client
        git-annex
        git-remote-gcrypt
        spacer
        faketty
        mesa.drivers
        vulkan-tools
        vulkan-tools-lunarg
        vulkan-headers
        vulkan-loader
        vulkan-loader.dev
        vulkan-validation-layers
        vulkan-extension-layer
        shaderc
      ];

    };
}

# vim: ft=nix
