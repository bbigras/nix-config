{ lib, pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      jjui
      kdiff3
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
        aliases = {
          l = [
            "log"
            "-r"
            "(trunk()..@):: | (trunk()..@)-"
          ];
          dlog = [
            "log"
            "-r"
          ];
        };
        core.fsmonitor = "watchman";
        core.watchman.register-snapshot-trigger = true;
        user = {
          name = "Bruno Bigras";
          email = "bigras.bruno@gmail.com";
        };
        template-aliases = {
          "format_short_signature(signature)" = "signature.email().local()";
        };
        ui.default-command = "log";
        revset-aliases = {
          "HEAD" = "@-";
          "desc(x)" = "description(x)";
          "user()" = ''user("bigras.bruno@gmail.com")'';
          "user(x)" = "author(x) | committer(x)";
        };
      };
    };
  };

}
