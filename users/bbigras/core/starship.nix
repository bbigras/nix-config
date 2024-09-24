{ config, lib, pkgs, ... }:

let
  jj2-wrapped = pkgs.writeShellApplication
    {
      name = "jj2-wrapped";

      runtimeInputs = [ pkgs.sd ];

      text = ./jj2.sh;
    };
in
{
  programs.starship = {
    enable = true;
    enableTransience = true;
    settings = {
      custom.jj = {
        command = ./jj.sh;
        detect_folders = [ ".jj" ];
        symbol = "jj";
      };
      custom.jjstate = {
        detect_folders = [ ".jj" ];
        command = "${jj2-wrapped}/bin/jj2-wrapped";
      };
    };
  };
}
