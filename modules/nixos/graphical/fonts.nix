{
  pkgs,
  ...
}:
{
  fonts = {
    packages = with pkgs; [
      dejavu_fonts
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      unifont
      nerd-fonts.noto
      nerd-fonts.symbols-only
    ];
  };
}
