{ lib, pkgs, ... }: {
  programs.fish = {
    enable = true;
    functions = {
      gitignore = "curl -sL https://www.gitignore.io/api/$argv";
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
        name = "tide";
        src = pkgs.fetchFromGitHub {
          owner = "ETCaton";
          repo = "tide";
          rev = "d607c9e767b3ce919bbc78d6a1bd4ef9c1bdfd8a";
          sha256 = "sha256-kyOzGBrytaaShuO8rSj6IbJLBsPn8wu/U0Slx0LnBqk=";
        };
      }
    ];
  };
}
