{ pkgs, ... }: {
  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userName = "Bruno Bigras";
    userEmail = "bigras.bruno@gmail.com";

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
    };
  };
}
