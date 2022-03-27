{ pkgs, ... }: {
  programs.git = {
    enable = true;
    package = pkgs.gitFull;
    lfs.enable = false;
    userEmail = "bigras.bruno@gmail.com";
    userName = "Bruno Bigras";

    ignores = [
      "*~"
      "*.swp"
      ".direnv/"
      # exclude nix-build result
      "result"
      "result-*"
    ];

    delta = {
      enable = true;
      options = {
        line-numbers = true;
        side-by-side = true;
        whitespace-error-style = "22 reverse";
        syntax-theme = "ansi";
      };
    };

    aliases = {
      st = "status";
      co = "checkout";
      ci = "commit";
      br = "branch";
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      recent = "for-each-ref --sort=-committerdate --format='%(committerdate:short): %(refname:short)' refs/heads/";
    };

    extraConfig = {
      # core.pager = "${pkgs.delta}/bin/delta --dark";
      difftool.prompt = true;
      github.user = "bbigras";
      mergetool.prompt = true;
      init.defaultBranch = "master";
      #
      pull.ff = "only";
      merge.conflictstyle = "diff3";
      credential.helper = "${pkgs.gitAndTools.gitFull}/bin/git-credential-libsecret";
      diff.tool = "diffsitter";
      # difftool.prompt = false;
      difftool.difftastic.cmd = "/home/bbigras/.cargo/bin/difftastic \"$LOCAL\" \"$REMOTE\"";
      difftool.diffsitter.cmd = "/home/bbigras/.cargo/bin/diffsitter \"$LOCAL\" \"$REMOTE\"";
    };
  };
}
