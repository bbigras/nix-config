{ lib, pkgs, ... }:

let
  wakatime = pkgs.fishPlugins.wakatime-fish.overrideAttrs (oldAttrs: {
    postFixup = ''
      mv $out/share/fish/vendor_conf.d $out/share/fish/conf.d
    '';
  });
in
{
  programs.fish = {
    enable = true;
    functions = {
      gitignore = "curl -sL https://www.gitignore.io/api/$argv";
    };
    shellAliases = {
      "..." = "cd ../..";
    };
    interactiveShellInit = lib.mkMerge [
      (lib.mkBefore ''
        set -g fish_escape_delay_ms 300
        set -g fish_greeting
        set -g tide_left_prompt_items os pwd git jj newline character
      '')
      (lib.mkAfter ''
        ${pkgs.nix-your-shell}/bin/nix-your-shell --nom fish | source
      '')
    ];
    plugins = [
      { name = "autopair"; inherit (pkgs.fishPlugins.autopair) src; }
      {
        name = "wakatime";
        src = "${wakatime}/share/fish";
      }
      {
        name = "tide";
        src = pkgs.fetchFromGitHub {
          owner = "ETCaton";
          repo = "tide";
          rev = "1e063799e076117969dd0f5ab1b493ba28cc757d";
          sha256 = "sha256-Ea81varZWqk/+uZRImKAnlkSscTbHsdFj6CA8TL3cYA=";
        };
      }
    ];
  };
}
