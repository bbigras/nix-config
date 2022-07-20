{ pkgs, ... }: {
  programs.git = {
    enable = true;
    delta = {
      enable = true;
      options = {
        navigate = true;
        syntax-theme = "Nord";
      };
    };
    package = pkgs.gitFull;
    lfs.enable = false;
    userEmail = "bigras.bruno@gmail.com";
    userName = "Bruno Bigras";
    extraConfig = {
      diff = {
        colorMoved = "default";
        age.textconv = "${pkgs.rage}/bin/rage -i ~/.ssh/id_ed25519 --decrypt";
      };
      difftool.prompt = true;
      github.user = "bbigras";
      init.defaultBranch = "main";
      merge.conflictstyle = "diff3";
      mergetool.prompt = true;
      pull.ff = "only";
      credential.helper = "${pkgs.gitAndTools.gitFull}/bin/git-credential-libsecret";
      diff.tool = "diffsitter";
      # difftool.prompt = false;
      difftool.difftastic.cmd = "/home/bbigras/.cargo/bin/difftastic \"$LOCAL\" \"$REMOTE\"";
      difftool.diffsitter.cmd = "/home/bbigras/.cargo/bin/diffsitter \"$LOCAL\" \"$REMOTE\"";
    };

    aliases = {
      st = "status";
      co = "checkout";
      ci = "commit";
      br = "branch";
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      recent = "for-each-ref --sort=-committerdate --format='%(committerdate:short): %(refname:short)' refs/heads/";
    };

    ignores = [
      "*~"
      "*.swp"
      # exclude nix-build result
      "result"
      "result-*"
    ];
  };
}
