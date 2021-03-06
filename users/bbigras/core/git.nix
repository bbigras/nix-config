{ pkgs, ... }: {
  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userName = "Bruno Bigras";
    userEmail = "bigras.bruno@gmail.com";

    ignores = [
      "*~"
      "*.swp"
      ".direnv/"
      # exclude nix-build result
      "result"
      "result-*"
    ];

    delta.enable = true;
    delta.options = {
      line-numbers = true;
      side-by-side = true;
      whitespace-error-style = "22 reverse";
      syntax-theme = "ansi-dark";
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
      pull.ff = "only";
      merge.conflictstyle = "diff3";
      credential.helper = "${pkgs.gitAndTools.gitFull}/bin/git-credential-libsecret";
    };
  };
}
