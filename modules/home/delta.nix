{
  programs = {
    delta = {
      enable = true;
      enableGitIntegration = true;
      enableJujutsuIntegration = true;
    };

    jjui = {
      settings = {
        preview = {
          file_command = [
            "diff"
            "--color"
            "always"
            "--config"
            "ui.diff-formatter=delta"
            "-r"
            "$change_id"
            "$file"
          ];
          revision_command = [
            "show"
            "--color"
            "always"
            "-r"
            "$change_id"
            "--config"
            "ui.diff-formatter=delta"
          ];
        };
      };
    };
  };
}
