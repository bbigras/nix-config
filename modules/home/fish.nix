{ lib, pkgs, ... }:
{
  programs.fish = {
    enable = true;
    interactiveShellInit = lib.mkMerge [
      (lib.mkBefore ''
        set -g fish_escape_delay_ms 300
        set -g fish_greeting
      '')
      (lib.mkAfter ''
        ${pkgs.nix-your-shell}/bin/nix-your-shell --nom fish | source

        fish_default_key_bindings

        function tmux_refresh_env --on-event fish_prompt
            test -n "$TMUX"; or return

            for var in SSH_AUTH_SOCK SSH_AGENT_PID SSH_CONNECTION SSH_CLIENT SSH_TTY
                set line (tmux show-environment "$var" 2>/dev/null)
                or continue

                if string match -q -- "-$var" "$line"
                    set -e $var
                else
                    set -gx $var (string replace "$var=" "" "$line")
                end
            end
        end
      '')
    ];
    plugins = [
      {
        name = "autopair";
        inherit (pkgs.fishPlugins.autopair) src;
      }
    ];
  };
}
