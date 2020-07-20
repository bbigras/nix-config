{ pkgs, ... }: {
  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

    ignores = [ "*~" "*.swp" ];

    delta.enable = true;
    aliases = {
      st = "status";
      co = "checkout";
      ci = "commit";
      br = "branch";
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      recent = "for-each-ref --sort=-committerdate --format='%(committerdate:short): %(refname:short)' refs/heads/";
    };

    extraConfig = {
      core = {
        excludesfile = "~/.gitignore";
      };

      color.ui = true;

      color."diff-highlight" = {
        newHighlight = "green bold 22";
        newNormal = "green bold";
        oldHighlight = "red bold 52";
        oldNormal = "red bold";
      };

      color."diff" = {
        commit = "yellow bold";
        frag = "magenta bold";
        meta = "yellow";
        new = "green bold";
        old = "red bold";
        whitespace = "red reverse";
      };
    };
  };
}
