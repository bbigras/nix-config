{ ... }:

{
  programs.jjui = {
    enable = true;
    settings = {
      revisions = {
        template = "builtin_log_compact";
        # revset = "ancestors(@ | heads(remote_bookmarks())) ~ empty()";
      };
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
  };
}
