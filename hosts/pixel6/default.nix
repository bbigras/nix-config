{ base16-schemes, pkgs, nur, stylix, ... }:

let
  nurNoPkgs = import nur { pkgs = null; nurpkgs = pkgs; };
in
{
  build.arch = "aarch64";
  user.shell = "${pkgs.zsh}/bin/zsh";

  user = {
    gid = 10382;
    uid = 10382;
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
    { pkgs, config, lib, ... }:
    {
      # Read the changelog before changing this value
      home.stateVersion = "20.09";

      home.file.".ssh/config".text = ''
        Include ~/.ssh/devpod
      '';

      home.activation = {
        copyFont =
          let
            font_src = "${pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; }}/share/fonts/truetype/NerdFonts/FiraCodeNerdFontMono-Regular.ttf";
            font_dst = "${config.home.homeDirectory}/.termux/font.ttf";
          in
          lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            ( test ! -e "${font_dst}" || test $(sha1sum "${font_src}"|cut -d' ' -f1 ) != $(sha1sum "${font_dst}" |cut -d' ' -f1)) && $DRY_RUN_CMD install $VERBOSE_ARG -D "${font_src}" "${font_dst}"
          '';
      };

      imports = [
        stylix.homeManagerModules.stylix
        ../../users/bbigras/core/atuin.nix
        ../../users/bbigras/core/git.nix
        ../../users/bbigras/core/tmux.nix
        ../../users/bbigras/core/zsh.nix
        ../../users/bbigras/core/emacs
        nurNoPkgs.repos.rycee.hmModules.emacs-init
      ] ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/pixel6.nix") then [ (builtins.getEnv "PWD" + "/secrets/pixel6.nix") ] else [ ]);

      # Use the same overlays as the system packages
      # nixpkgs.overlays = config.nixpkgs.overlays;

      home.language.base = "fr_CA.UTF-8";

      # insert home-manager config
      programs = {
        aria2.enable = true;
        atuin = {
          enable = true;
          settings.auto_sync = true;
        };
        bat.enable = true;
        carapace.enable = true;
        command-not-found.enable = true;
        emacs = {
          enable = true;
          package = lib.mkForce pkgs.emacs29-nox;
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
          colors = {
            bg = "#1e1e1e";
            "bg+" = "#1e1e1e";
            fg = "#d4d4d4";
            "fg+" = "#d4d4d4";
          };
        };
        jq.enable = true;
        htop.enable = true;
        nushell.enable = false;
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
        libqalculate
        cachix
        croc
        doggo
        fd
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
        ripgrep
        kubernetes
        kubernetes-helm
        k9s
        kdash
        kubie
        kubectl
        kubectx
        kubelogin-oidc
        # socat
        # websocat
        git-annex
        devpod
        zrok
        truecolor-check
      ];

      dconf.enable = lib.mkForce false;
      stylix = {
        base16Scheme = "${base16-schemes}/tomorrow-night.yaml";
        image = pkgs.nixos-artwork.wallpapers.simple-dark-gray.gnomeFilePath;
      };
    };
}

# vim: ft=nix
