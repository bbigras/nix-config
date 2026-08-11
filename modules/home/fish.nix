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

        set -U __done_min_cmd_duration 5000
        set -U __done_notification_command "~/dev/send_pushover.sh \$title \$message"
      '')
    ];
    plugins = [
      {
        name = "autopair";
        inherit (pkgs.fishPlugins.autopair) src;
      }
      {
        name = "done";
        inherit (pkgs.fishPlugins.done) src;
      }
    ];
  };
}
