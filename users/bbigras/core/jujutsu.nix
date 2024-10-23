{ lib, pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      kdiff3
      lazyjj
      meld
      watchman
    ];
  };

  # environment.systemPackages = with pkgs; [
  #   watchman
  # ];

  programs = {
    jujutsu = {
      enable = true;
      settings = {
        aliases.l = [ "log" "-r" "(trunk()..@):: | (trunk()..@)-" ];
        core.fsmonitor = "watchman";
        user = {
          name = "Bruno Bigras";
          email = "bigras.bruno@gmail.com";
        };
        template-aliases = {
          "format_short_signature(signature)" = "signature.username()";
        };
        ui.diff-editor = "meld-3";
        revset-aliases = {
          "HEAD" = "@-";
          "user()" = ''user("bigras.bruno@gmail.com")'';
          "user(x)" = "author(x) | committer(x)";
        };
      };
    };
  };

}
