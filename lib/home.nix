{ config, pkgs, ... }:
{
  home-manager = {
    useGlobalPkgs = true;
    users = {
      bbigras = {
        imports = [
          pkgs.nur.repos.rycee.hmModules.emacs-init
        ];
      };
    };
  };
}
