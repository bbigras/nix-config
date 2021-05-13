{ pkgs, lib, ... }:

{
  build.arch = "aarch64";
  user.shell = "${pkgs.zsh}/bin/zsh";

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

      # insert home-manager config
      programs = {
        aria2.enable = true;
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

        zsh = {
          enable = true;
          enableAutosuggestions = true;
          enableCompletion = true;
          enableVteIntegration = true;
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
            # {
            #   name = "async";
            #   src = pkgs.zsh-async;
            # }
            {
              name = "powerlevel10k";
              src = pkgs.zsh-powerlevel10k;
              file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
            }
            {
              name = "powerlevel10k-config";
              src = lib.cleanSource ../../users/bbigras/core/p10k-config;
              file = "p10k.zsh";
            }
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
      ];

      fonts.fontconfig.enable = true;
    };
}

# vim: ft=nix
