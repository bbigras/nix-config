{ lib, pkgs, ... }:
{
  programs.starship = {
    enable = true;
    settings = {
      custom = {
        jj = {
          when = "${lib.getExe pkgs.jj-starship} detect";
          shell = [ "${lib.getExe pkgs.jj-starship}" ];
          format = "$output ";
        };
      };
      git_branch.disabled = true;
      git_status.disabled = true;
    };
    presets = [
      "nerd-font-symbols"
      "pure-preset"
    ];
  };
}
