{ lib, pkgs, ... }:
{
  programs.starship = {
    enable = true;
    settings = {
      add_newline = false;
      custom = {
        jj = {
          when = "${lib.getExe pkgs.jj-starship} detect";
          shell = [ "${lib.getExe pkgs.jj-starship}" ];
          format = "$output ";
        };
      };
      format = lib.concatStrings [
        "$username"
        "$hostname"
        "$directory"
        "$git_branch"
        "$git_commit"
        "$git_state"
        "$git_status"
        "$package"
        "$haskell"
        "$python"
        "$rust"
        "$nix_shell"
        "$line_break"
        "$jobs"
        "$character"
      ];
    };
    presets = [
      "nerd-font-symbols"
      "pure-preset"
    ];
  };
}
