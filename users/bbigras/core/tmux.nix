{ pkgs, ... }:
{
  home = {
    packages = with pkgs; [
    ];
  };

  programs.tmux = {
    enable = true;
    sensibleOnTop = true;
    aggressiveResize = true;
    clock24 = true;
    escapeTime = 0;
    newSession = true;
    plugins = with pkgs.tmuxPlugins; [
      extrakto
      fuzzback
      tmux-fzf
      tmux-thumbs
    ];
    mouse = true;
    # secureSocket = false;
    terminal = "tmux-256color";
    historyLimit = 30000;
    extraConfig = ''
      # update the env when attaching to an existing session
      set -g update-environment -r

      # automatically renumber windows
      set -g renumber-windows on
      bind R source-file ~/.config/tmux/tmux.conf \; display-message "Config reloaded..."
      set -g base-index 0
      set-window-option -g automatic-rename
      setw -g monitor-activity on
      set -g visual-activity off

      # for t-smart-tmux-session-manager
      bind-key x kill-pane # skip "kill-pane 1? (y/n)" prompt
      set -g detach-on-destroy off  # don't exit from tmux when closing a session
      set -g @t-fzf-find-binding 'ctrl-f:change-prompt(  )+reload(fd -H -d 2 -t d . ~)'
      set -g @t-fzf-prompt '  '
    '';
  };
}
