{ pkgs, ... }:

{
  build.arch = "aarch64";
  user.shell = "${pkgs.zsh}/bin/zsh";

  user = {
    gid = 10382;
    uid = 10382;
  };

  # Simply install just the packages
  environment.packages = with pkgs; [
    # User-facing stuff that you really really want to have
    vim # or some other editor, e.g. nano or neovim

    # Some common stuff that people expect to have
    diffutils
    findutils
    utillinux
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

  environment.etc = {
    "tmp-sshd".text = ''
      HostKey /data/data/com.termux.nix/files/home/ssh_host_ed25519_key
      Port 8022
    '';
  };

  # Read the changelog before changing this value
  system.stateVersion = "20.09";

  # After installing home-manager channel like
  #   nix-channel --add https://github.com/rycee/home-manager/archive/release-20.09.tar.gz home-manager
  #   nix-channel --update
  # you can configure home-manager in here like

  home-manager.useGlobalPkgs = true;

  home-manager.config =
    { pkgs, ... }:
    {
      # Read the changelog before changing this value
      home.stateVersion = "20.09";

      imports = [
        ../../users/bbigras/core/atuin.nix
        ../../users/bbigras/core/git.nix
        ../../users/bbigras/core/taskwarrior.nix
        ../../users/bbigras/core/tmux.nix
        ../../users/bbigras/core/zsh.nix
      ] ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/pixel6.nix") then [ (builtins.getEnv "PWD" + "/secrets/pixel6.nix") ] else [ ]);

      # Use the same overlays as the system packages
      # nixpkgs.overlays = config.nixpkgs.overlays;

      home.sessionVariables = {
        VAULT_ADDR = "http://100.118.252.12:8200";
      };

      home.language.base = "fr_CA.UTF-8";

      # insert home-manager config
      programs = {
        aria2.enable = true;
        atuin = {
          enable = true;
          settings.auto_sync = true;
        };
        bat.enable = true;
        command-not-found.enable = true;
        emacs = {
          enable = false;
          package = pkgs.emacs-nox;
        };
        exa = {
          enable = true;
          enableAliases = true;
        };
        fzf.enable = true;
        just.enable = true;
        jq.enable = true;
        htop.enable = true;
        nushell.enable = true;
        ssh = {
          enable = true;
          # controlMaster = "auto";
          # controlPersist = "10m";
          hashKnownHosts = true;

          extraOptionOverrides = {
            AddKeysToAgent = "confirm";
            VerifyHostKeyDNS = "ask";
          };
        };
        tealdeer.enable = true;
        zoxide.enable = true;
        zellij.enable = true;
        zsh = {
          shellAliases = {
            ssh-server = "${pkgs.openssh}/bin/sshd -dD -f /etc/tmp-sshd";
          };
        };
      };

      home.packages = with pkgs; [
        kalker
        cachix
        croc
        dogdns
        fd
        mosh
        (neofetch.override { x11Support = false; })
        oneshot
        pwgen
        #rage
        ht-rust
        openssh
        prettyping
        # tab-rs
        ripgrep
        vault
        kubernetes
        kubernetes-helm
        k9s
        kubie
        kubectl
        # socat
        # websocat
        hyperspace-cli
      ];
    };
}

# vim: ft=nix
