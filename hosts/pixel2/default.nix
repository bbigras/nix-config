{ pkgs, lib, ... }:

{
  build.arch = "aarch64";
  user.shell = "${pkgs.zsh}/bin/zsh";

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

      home.file.".zshenv".text = ''
        typeset -U path cdpath fpath manpath
        # Set PATH for both interactive and non-interactive shell
        path+=($HOME/.nix-profile/bin /etc/profiles/per-user/$USER/bin /nix/var/nix/profiles/default/bin /run/current-system/sw/bin)
      '';

      imports = [ ] ++ (if builtins.pathExists (builtins.getEnv "PWD" + "/secrets/pixel2.nix") then [ (builtins.getEnv "PWD" + "/secrets/pixel2.nix") ] else [ ]);

      # Use the same overlays as the system packages
      # nixpkgs.overlays = config.nixpkgs.overlays;

      home.sessionVariables = {
        VAULT_ADDR = "http://100.118.252.12:8200";
      };

      home.language.base = "fr_CA.UTF-8";

      # insert home-manager config
      programs = {
        aria2.enable = true;
        bat.enable = true;
        command-not-found.enable = true;
        emacs = {
          enable = true;
          package = pkgs.emacs-nox;
        };
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
        htop.enable = true;
        mcfly.enable = true;
        ssh.enable = true;
        tmux = {
          enable = true;
          tmuxp.enable = true;
          terminal = "screen-256color";
        };
        zoxide.enable = true;

        zsh = {
          enable = true;
          enableAutosuggestions = true;
          enableCompletion = true;
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
              src = lib.cleanSource ../../users/bbigras/core/p10k-config;
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
            vault-login = "${pkgs.vault}/bin/vault login -method=oidc -path=/oidc-google";
            vssh = "${pkgs.vault}/bin/vault ssh -mount-point=ssh-client-signer -mode=ca -role=my-role -private-key-path=~/.ssh/id_ed25519 -public-key-path=~/.ssh/id_ed25519.pub";
            ssh-server = "${pkgs.openssh}/bin/sshd -dD -f /etc/tmp-sshd";
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

      home.packages = with pkgs; [
        cachix
        croc
        dogdns
        fd
        # meslo-lgs-nf
        mosh
        (neofetch.override { x11Support = false; })
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
        kubectl
      ];
    };
}

# vim: ft=nix
