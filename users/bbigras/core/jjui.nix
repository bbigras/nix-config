{ pkgs, ... }:

let
  yaml = pkgs.formats.toml { };
in
{
  xdg.configFile."jjui/config.toml".source = yaml.generate "config.toml" {
    custom_commands = {
      "show diff" = {
        key = [ "U" ];
        args = [
          "diff"
          "-r"
          "$change_id"
          "--color"
          "always"
        ];
        show = "diff";
      };
      "show oplog diff" = {
        key = [ "ctrl+o" ];
        args = [
          "op"
          "show"
          "$operation_id"
          "--color"
          "always"
        ];
        show = "diff";
      };
      "resolve vscode" = {
        key = [ "R" ];
        args = [
          "resolve"
          "--tool"
          "vscode"
        ];
        show = "interactive";
      };
      "new main" = {
        args = [
          "new"
          "main"
        ];
      };
      "tug" = {
        key = [ "ctrl+t" ];
        args = [
          "bookmark"
          "move"
          "--from"
          "closest_bookmark($change_id)"
          "--to"
          "closest_pushable($change_id)"
        ];
      };
    };
  };
}
