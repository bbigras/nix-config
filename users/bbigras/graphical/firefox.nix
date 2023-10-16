{ pkgs, lib, ... }: {
  home.file.".mozilla/native-messaging-hosts/ff2mpv.json".source = "${pkgs.ff2mpv}/lib/mozilla/native-messaging-hosts/ff2mpv.json";

  programs.firefox = {
    enable = pkgs.hostPlatform.system == "x86_64-linux";
    package =
      if lib.meta.availableOn pkgs.stdenv.hostPlatform pkgs.firefox-bin
      then pkgs.firefox-bin
      else pkgs.firefox;

    # https://ffprofile.com/
    profiles = {
      default = {
        isDefault = true;
        settings = { };
      };
      travail = {
        isDefault = false;
        id = 1;
      };
    };
  };

  xdg.mimeApps.defaultApplications = {
    "application/x-extension-htm" = "firefox.desktop";
    "application/x-extension-html" = "firefox.desktop";
    "application/x-extension-shtml" = "firefox.desktop";
    "application/x-extension-xht" = "firefox.desktop";
    "application/x-extension-xhtml" = "firefox.desktop";
    "application/xhtml+xml" = "firefox.desktop";
    "text/html" = "firefox.desktop";
    "x-scheme-handler/chrome" = "firefox.desktop";
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
  };
}
