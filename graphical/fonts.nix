{ hostType, lib, pkgs, ... }: {
  fonts = {
    fontDir.enable = hostType == "darwin";
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "Hack" ]; })
      ibm-plex
      dejavu_fonts
      unifont
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      noto-fonts-emoji
      noto-fonts-extra
    ];
  } // lib.optionalAttrs (hostType == "nixos") {
    enableDefaultFonts = false;
    enableGhostscriptFonts = false;
    fontconfig = {
      defaultFonts = {
        sansSerif = [ "IBM Plex Sans" ];
        serif = [ "IBM Plex Sans" ];
        monospace = [ "Hack Nerd Font" ];
        emoji = [ "Noto Color Emoji" ];
      };
      localConf = ''
        <?xml version="1.0"?>
        <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
        <fontconfig>
            <alias binding="weak">
                <family>monospace</family>
                <prefer>
                    <family>emoji</family>
                </prefer>
            </alias>
            <alias binding="weak">
                <family>sans-serif</family>
                <prefer>
                    <family>emoji</family>
                </prefer>
            </alias>
            <alias binding="weak">
                <family>serif</family>
                <prefer>
                    <family>emoji</family>
                </prefer>
            </alias>
        </fontconfig>
      '';
    };
  };
}
