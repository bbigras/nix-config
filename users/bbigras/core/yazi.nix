{ pkgs, ... }:

let
  yazi-plugins = pkgs.fetchFromGitHub {
    owner = "yazi-rs";
    repo = "plugins";
    rev = "4f1d0ae0862f464e08f208f1807fcafcd8778e16";
    hash = "sha256-+d7D6nq/oOzcsvvH0MHmLUDkxAtand+IXKQ730m4Ifs=";
  };
in
{
  programs.yazi = {
    enable = true;
    enableFishIntegration = true;
    shellWrapperName = "y";

    settings = {
      manager = {
        show_hidden = true;
      };
      preview = {
        max_width = 1000;
        max_height = 1000;
      };
    };

    plugins = {
      chmod = "${yazi-plugins}/chmod.yazi";
      max-preview = "${yazi-plugins}/max-preview.yazi";
      starship = pkgs.fetchFromGitHub {
        owner = "Rolv-Apneseth";
        repo = "starship.yazi";
        rev = "77a65f5a367f833ad5e6687261494044006de9c3";
        sha256 = "sha256-sAB0958lLNqqwkpucRsUqLHFV/PJYoJL2lHFtfHDZF8=";
      };
    };

    initLua = ''
      require("starship"):setup()
    '';

    keymap = {
      manager.prepend_keymap = [
        {
          on = "T";
          run = "plugin --sync max-preview";
          desc = "Maximize or restore the preview pane";
        }
        {
          on = [
            "c"
            "m"
          ];
          run = "plugin chmod";
          desc = "Chmod on selected files";
        }
      ];
    };
  };
}
