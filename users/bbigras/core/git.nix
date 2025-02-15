{ pkgs, ... }:
{
  programs.git = {
    enable = true;
    package = pkgs.gitFull;
    lfs.enable = true;
    userEmail = "bigras.bruno@gmail.com";
    userName = "Bruno Bigras";
    signing.format = "ssh";
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
      rebase.updateRefs = true;
      credential.helper = "${pkgs.gitAndTools.gitFull}/bin/git-credential-libsecret";
      rerere.enabled = true;
      maintenance.prefetch.enabled = false;
    };

    aliases = {
      st = "status";
      co = "checkout";
      ci = "commit";
      br = "branch";
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      recent = "for-each-ref --sort=-committerdate --format='%(committerdate:short): %(refname:short)' refs/heads/";

      # So you think you know git - Fosdem 2024
      staash = "stash --all";
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
