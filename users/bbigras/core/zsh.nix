{ config, lib, pkgs, ... }: {
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    enableVteIntegration = true;
    autocd = true;
    dotDir = ".config/zsh";
    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreDups = true;
      path = "${config.xdg.dataHome}/zsh/history";
      save = 10000;
      share = true;
    };
    envExtra = ''
      export LESSHISTFILE="${config.xdg.dataHome}/less_history"
      # export CARGO_HOME="${config.xdg.cacheHome}/cargo"
    '';
    initExtra = ''
        if [[ "$TERM" != 'dumb' && -z "$INSIDE_EMACS" ]]; then
          source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
          [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
        fi

      nix-closure-size() {
        nix-store -q --size $(nix-store -qR $(${pkgs.coreutils}/bin/readlink -e $1) ) |
          ${pkgs.gawk}/bin/gawk '{ a+=$1 } END { print a }' |
          ${pkgs.coreutils}/bin/numfmt --to=iec-i
      }

      bindkey "$${terminfo[khome]}" beginning-of-line
      bindkey "$${terminfo[kend]}" end-of-line
      bindkey "$${terminfo[kdch1]}" delete-char
      bindkey '\eOA' history-substring-search-up
      bindkey '\eOB' history-substring-search-down
      bindkey "^[[A" history-substring-search-up
      bindkey "^[[B" history-substring-search-down
      bindkey "$$terminfo[kcuu1]" history-substring-search-up
      bindkey "$$terminfo[kcud1]" history-substring-search-down
      bindkey "^[[1;5C" forward-word
      bindkey "^[[1;3C" forward-word
      bindkey "^[[1;5D" backward-word
      bindkey "^[[1;3D" backward-word

      bindkey -s "^O" 'fzf | xargs -r $VISUAL^M'

      bindkey -rpM viins '^[^['
      KEYTIMEOUT=1

      ${pkgs.any-nix-shell}/bin/any-nix-shell zsh | source /dev/stdin
    '';
    sessionVariables = {
      RPROMPT = "";
    };
    shellAliases = {
      cat = "${pkgs.bat}/bin/bat";
      man = "${pkgs.bat-extras.batman}/bin/batman";
      less = ''${pkgs.bat}/bin/bat --paging=always --pager "${pkgs.less}/bin/less -RF"'';
      j = "${pkgs.just}/bin/just";
      ".j" = "${pkgs.just}/bin/just --justfile ~/.user.justfile";
      ntp-google = "sudo ${pkgs.ntp}/bin/ntpdate time.google.com";
    };
    plugins = [
      {
        name = "powerlevel10k-config";
        src = lib.cleanSource ./p10k-config;
        file = "p10k.zsh";
      }
      {
        name = "zsh-you-should-use";
        src = pkgs.zsh-you-should-use;
      }
      {
        # https://github.com/softmoth/zsh-vim-mode
        name = "zsh-vim-mode";
        file = "zsh-vim-mode.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "softmoth";
          repo = "zsh-vim-mode";
          rev = "abef0c0c03506009b56bb94260f846163c4f287a";
          sha256 = "0cnjazclz1kyi13m078ca2v6l8pg4y8jjrry6mkvszd383dx1wib";
        };
      }
      # {
      #   # https://github.com/zdharma/fast-syntax-highlighting
      #   name = "fast-syntax-highlighting";
      #   file = "fast-syntax-highlighting.plugin.zsh";
      #   src = pkgs.fetchFromGitHub {
      #     owner = "zdharma";
      #     repo = "fast-syntax-highlighting";
      #     rev = "be2f385453670c18c40320a7384333f98fcd9f79";
      #     sha256 = "sha256-SRM+PmbyEhIDo3V3X2BxtbYlfa/lKZ69CtLm8JTYUMo=";
      #   };
      # }
      # {
      #   # https://github.com/zdharma/history-search-multi-word
      #   name = "history-search-multi-word";
      #   file = "history-search-multi-word.plugin.zsh";
      #   src = pkgs.fetchFromGitHub {
      #     owner = "zdharma";
      #     repo = "history-search-multi-word";
      #     rev = "f4efe71dbfae5f027c08f8ff8e8dca9bc946601c";
      #     sha256 = "0v0jiv9wbf6dyxzsqai2j87nlbhy8l097v3pmss9ayr8h2faxkpx";
      #   };
      # }
      {
        # https://github.com/hlissner/zsh-autopair
        name = "zsh-autopair";
        file = "zsh-autopair.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "hlissner";
          repo = "zsh-autopair";
          rev = "34a8bca0c18fcf3ab1561caef9790abffc1d3d49";
          sha256 = "1h0vm2dgrmb8i2pvsgis3lshc5b0ad846836m62y8h3rdb3zmpy1";
        };
      }
      {
        # https://github.com/zsh-users/zsh-history-substring-search
        name = "zsh-history-substring-search";
        file = "zsh-history-substring-search.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-history-substring-search";
          rev = "0f80b8eb3368b46e5e573c1d91ae69eb095db3fb";
          sha256 = "0y8va5kc2ram38hbk2cibkk64ffrabfv1sh4xm7pjspsba9n5p1y";
        };
      }
    ];
  };
}
