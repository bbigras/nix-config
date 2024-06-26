{ config, lib, pkgs, ... }: {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableVteIntegration = pkgs.stdenv.isLinux;
    autocd = true;
    autosuggestion.enable = true;
    dotDir = ".config/zsh";
    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreDups = true;
      ignoreSpace = true;
      path = "${config.xdg.dataHome}/zsh/history";
      save = 10000;
      share = true;
    };
    envExtra = ''
      export LESSHISTFILE="${config.xdg.dataHome}/less_history"
      #export CARGO_HOME="${config.xdg.cacheHome}/cargo" # doesn't work with emacs
    '';
    initExtra = ''
      bindkey "$${terminfo[khome]}" beginning-of-line
      bindkey "$${terminfo[kend]}" end-of-line
      bindkey "$${terminfo[kdch1]}" delete-char
      ${pkgs.nix-your-shell}/bin/nix-your-shell zsh | source /dev/stdin
      bindkey "^[[1;5C" forward-word
      bindkey "^[[1;3C" forward-word
      bindkey "^[[1;5D" backward-word
      bindkey "^[[1;3D" backward-word
      bindkey -s "^O" 'fzf | xargs -r $EDITOR^M'

      bindkey -rpM viins '^[^['

    '';
    sessionVariables = {
      RPROMPT = "";
      DIRENV_LOG_FORMAT = "";
      PAGER = "less -FRX";
    };
    plugins = [
      {
        name = "zsh-you-should-use";
        src = pkgs.zsh-you-should-use;
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
