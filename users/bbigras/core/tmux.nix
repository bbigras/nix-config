{ pkgs, ... }: {
  home = {
    packages = with pkgs; [
      tmuxPlugins.t-smart-tmux-session-manager
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
      copycat
      copy-toolkit
      extrakto
      fuzzback
      prefix-highlight
      yank
      t-smart-tmux-session-manager
      tmux-thumbs
      tmux-fzf
    ];
    mouse = true;
    secureSocket = false;
    terminal = "tmux-256color";
    historyLimit = 30000;
    extraConfig = ''
      bind r source-file ~/.config/tmux/tmux.conf \; display-message "Config reloaded..."
      setw -g monitor-activity on

      # for t-smart-tmux-session-manager
      bind-key x kill-pane # skip "kill-pane 1? (y/n)" prompt
      set -g detach-on-destroy off  # don't exit from tmux when closing a session
      set -g @t-fzf-find-binding 'ctrl-f:change-prompt(  )+reload(fd -H -d 2 -t d . ~)'
      set -g @t-fzf-prompt '  '
    '';
  };
}
