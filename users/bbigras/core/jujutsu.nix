{ lib, pkgs, ... }:

{
  home = {
    packages = with pkgs; [
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
        core.fsmonitor = "watchman";
        ui.allow-filesets = true;
        user = {
          name = "Bruno Bigras";
          email = "bigras.bruno@gmail.com";
        };
        template-aliases = {
          "format_short_signature(signature)" = "signature.username()";
        };
      };
    };
  };

}
