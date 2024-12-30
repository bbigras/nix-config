{
  programs.tmux = {
    enable = true;
    aggressiveResize = true;
    clock24 = true;
    escapeTime = 0;
    #secureSocket = false;
    #shortcut = "g";
    historyLimit = 30000;
    newSession = true;
    terminal = "tmux-256color";
    extraConfig = ''
      # update the env when attaching to an existing session
      set -g update-environment -r

      # automatically renumber windows
      set -g renumber-windows on

      set -g base-index 0
      set-window-option -g automatic-rename
    '';
  };
}
