{ pkgs, ... }:

let
  tmux-airline = ./tmux-airline.sh;
in
{
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
      fuzzback
      mode-indicator
      # nord
      prefix-highlight
      yank
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

      # https://waylonwalker.com/tmux-fzf-session-jump/
      bind C-j display-popup -E "\
          tmux list-sessions -F '#{?session_attached,,#{session_name}}' |\
          sed '/^$/d' |\
          fzf --reverse --header jump-to-session --preview 'tmux capture-pane -pt {}'  |\
          xargs tmux switch-client -t"
    '';

    # set -g status-right '#(eval ${tmux-airline} `tmux display -p "#{client_width}"`)'

    # set -g status-right " #(tms sessions)"
    # bind -r '(' switch-client -p\; refresh-client -S
    # bind -r ')' switch-client -n\; refresh-client -S

    # tmuxinator.enable
    # tmuxp.enable
  };
}
