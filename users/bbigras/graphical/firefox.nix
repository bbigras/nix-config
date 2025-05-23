{ pkgs, lib, ... }:
{
  home.file.".mozilla/native-messaging-hosts/ff2mpv.json".source =
    "${pkgs.ff2mpv}/lib/mozilla/native-messaging-hosts/ff2mpv.json";

  programs.firefox = {
    enable = pkgs.hostPlatform.system == "x86_64-linux";
    package =
      if lib.meta.availableOn pkgs.stdenv.hostPlatform pkgs.firefox-bin then
        pkgs.firefox-bin
      else
        pkgs.firefox;

    languagePacks = [
      "en-CA"
      "fr"
    ];

    # https://ffprofile.com/
    profiles = {
      default = {
        isDefault = true;
        settings = {
          "extensions.autoDisableScopes" = 0;
        };
        extensions.force = true;
        extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
          # privacy-badger
          ublock-origin
        ];
        extensions.settings = {
          "FirefoxColor@mozilla.com".settings = builtins.fromJSON (builtins.readFile ./firefox-color.json);
          "{7a7a4a92-a2a0-41d1-9fd7-1e92480d612d}".settings = {
            dbInChromeStorage = true; # required for Stylus
          };
        };
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
