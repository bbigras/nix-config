{ pkgs, ... }: {
  programs.alacritty = {
    enable = true;
    settings = {
      env.TERM = "xterm-256color";
      font = {
        normal = {
          family = "Hack";
          style = "Regular";
        };
        bold = {
          family = "Hack";
          style = "Bold";
        };
        italic = {
          family = "Hack";
          style = "Italic";
        };
        size = 8;
      };
      colors = {
        primary = {
          background = "0x0A0E14";
          foreground = "0xB3B1AD";
        };
        normal = {
          black = "0x01060E";
          blue = "0x53BDFA";
          cyan = "0x90E1C6";
          green = "0x91B362";
          magenta = "0xFAE994";
          red = "0xEA6C73";
          white = "0xC7C7C7";
          yellow = "0xF9AF4F";
        };
        bright = {
          black = "0x686868";
          blue = "0x59C2FF";
          cyan = "0x95E6CB";
          green = "0xC2D94C";
          magenta = "0xFFEE99";
          red = "0xF07178";
          white = "0xFFFFFF";
          yellow = "0xFFB454";
        };
      };
      shell.program = "${pkgs.zsh}/bin/zsh";
    };
  };
}
