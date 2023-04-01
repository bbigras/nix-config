{ pkgs, ... }: {
  home = {
    packages = with pkgs; [
      tmux-sessionizer
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
      nord
      prefix-highlight
      yank
      tmux-thumbs
      tmux-fzf
      {
        plugin = resurrect;
        extraConfig = ''
          ## Restore Panes
          set -g @resurrect-capture-pane-contents 'on'
        '';
      }
      {
        plugin = continuum;
        extraConfig = ''
          set -g @continuum-boot 'on'

          ## Restore last saved environment (automatically)
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '60' # minutes
        '';
      }
    ];
    secureSocket = false;
    terminal = "tmux-256color";
    historyLimit = 30000;
    extraConfig = ''
      bind r source-file ~/.config/tmux/tmux.conf \; display-message "Config reloaded..."
      setw -g monitor-activity on
      set -g mouse on

      # https://waylonwalker.com/tmux-fzf-session-jump/
      bind C-j display-popup -E "\
          tmux list-sessions -F '#{?session_attached,,#{session_name}}' |\
          sed '/^$/d' |\
          fzf --reverse --header jump-to-session --preview 'tmux capture-pane -pt {}'  |\
          xargs tmux switch-client -t"
    '';
  };
}
