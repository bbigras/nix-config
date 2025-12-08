{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      kdiff3
      meld
      # watchman
    ];
  };

  # environment.systemPackages = with pkgs; [
  #   watchman
  # ];

  programs = {
    mergiraf = {
      enable = true;
      enableJujutsuIntegration = true;
    };
    jujutsu = {
      enable = true;
      settings = {
        user = {
          name = "Bruno Bigras";
          email = "bigras.bruno@gmail.com";
        };
        ui.revsets-use-glob-by-default = true;
      };
    };
  };

}
