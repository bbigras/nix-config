{ pkgs, ... }:

{
  build.arch = "aarch64";
  # user.shell = "${pkgs.zsh}/bin/zsh";

  user = {
    gid = 10202;
    uid = 10202;
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
  home-manager.config =
    { pkgs, ... }:
    {
      # Read the changelog before changing this value
      home.stateVersion = "20.09";

      imports = [ ] ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/pixel2.nix") then [ (builtins.getEnv "PWD" + "/secrets/pixel2.nix") ] else [ ]);

      # Use the same overlays as the system packages
      # nixpkgs.overlays = config.nixpkgs.overlays;

      home.sessionVariables = {
        VAULT_ADDR = "http://100.118.252.12:8200";
      };

      # insert home-manager config
      programs = {
        aria2.enable = true;
        bash = {
          enable = true;
          shellAliases = {
            cat = "${pkgs.bat}/bin/bat";
            less = ''${pkgs.bat}/bin/bat --paging=always --pager "${pkgs.less}/bin/less -RF"'';
            vault-login = "${pkgs.vault}/bin/vault login -method=oidc -path=/oidc-google";
            vault-ssh = "${pkgs.vault}/bin/vault ssh -mount-point=ssh-client-signer -mode=ca -role=my-role";
            ssh-server = "${pkgs.openssh}/bin/sshd -dD -f /etc/tmp-sshd";
          };
        };
        bat.enable = true;
        command-not-found.enable = true;
        exa = {
          enable = true;
          enableAliases = true;
        };
        git = {
          enable = true;
          delta.enable = true;
          userName = "Bruno Bigras";
          userEmail = "bigras.bruno@gmail.com";
          aliases = {
            st = "status";
            co = "checkout";
            ci = "commit";
            br = "branch";
            lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
            recent = "for-each-ref --sort=-committerdate --format='%(committerdate:short): %(refname:short)' refs/heads/";
          };
          extraConfig = {
            pull.ff = "only";
          };
        };
        jq.enable = true;
        mcfly.enable = true;
        ssh.enable = true;
        starship.enable = true;
        tmux = {
          enable = true;
          tmuxp.enable = true;
          terminal = "screen-256color";
        };
        zoxide.enable = true;
      };

      home.packages = with pkgs; [
        cachix
        croc
        dogdns
        fd
        # meslo-lgs-nf
        mosh
        neofetch
        oneshot
        pwgen
        #rage
        ht-rust
        prettyping
        # tab-rs
        ripgrep
        tealdeer
        vault
        kubernetes
        kubernetes-helm
        k9s
        kubie
      ];

      fonts.fontconfig.enable = true;
    };
}

# vim: ft=nix
