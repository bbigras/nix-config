{ pkgs, config, ... }:

{
  build.arch = "aarch64";

  # Simply install just the packages
  environment.packages = with pkgs; [
    # User-facing stuff that you really really want to have
    vim # or some other editor, e.g. nano or neovim

    # Some common stuff that people expect to have
    #diffutils
    #findutils
    #utillinux
    #tzdata
    #hostname
    #man
    #gnugrep
    #gnupg
    #gnused
    #gnutar
    #bzip2
    #gzip
    #xz
    #zip
    #unzip
  ];

  # Backup etc files instead of failing to activate generation if a file already exists in /etc
  environment.etcBackupExtension = ".bak";

  # Read the changelog before changing this value
  system.stateVersion = "20.09";

  # After installing home-manager channel like
  #   nix-channel --add https://github.com/rycee/home-manager/archive/release-20.09.tar.gz home-manager
  #   nix-channel --update
  # you can configure home-manager in here like
  home-manager.config =
    { pkgs, ... }:
    {
      # Read the changelog before changing this value
      home.stateVersion = "20.09";

      # Use the same overlays as the system packages
      nixpkgs.overlays = config.nixpkgs.overlays;

      # insert home-manager config
      programs = {
        bash.enable = true;
        ssh.enable = true;
        # tmux.enable = true;
        starship.enable = true;
      };

      home.packages = with pkgs; [
        eternal-terminal
        mosh
        croc
        #dogdns
        # prettyping
      ];

    };
}

# vim: ft=nix
