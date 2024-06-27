{ pkgs, ... }: {
  programs.foot = {
    enable = true;
    settings = {
      main.font = "monospace:size=12";
      main.notify = "${pkgs.libnotify}/bin/notify-send -a foot -i foot \${title} \${body}";
      mouse.hide-when-typing = "yes";
      scrollback.lines = 32768;
      url.launch = "${pkgs.xdg-utils}/bin/xdg-open \${url}";
      tweak.grapheme-shaping = "yes";
    };
  };
}
