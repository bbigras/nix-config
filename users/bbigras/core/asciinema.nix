{
  programs.asciinema = {
    enable = true;
    settings = {
      notifications = {
        command = "tmux display-message \"$TEXT\"";
        enable = false;
      };
      playback = {
        next_marker_key = "m";
        pause_key = "^p";
        speed = 2;
        step_key = "s";
      };
      server = {
        url = "https://asciinema.example.com";
      };
      session = {
        add_marker_key = "^x";
        capture_env = "SHELL,TERM,USER";
        capture_input = true;
        command = "/run/current-system/sw/bin/bash -l";
        idle_time_limit = 2;
        pause_key = "^p";
        prefix_key = "^a";
      };
    };
  };
}
