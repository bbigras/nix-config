{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      radicle-tui
    ];
  };

  programs = {
    radicle = {
      enable = true;
      settings = {
        node = {
          listen = [ "0.0.0.0:8776" ];
        };
      };
    };
  };

  services = {
    radicle.node = {
      enable = true;
      lazy.enable = true;
    };
  };
}
