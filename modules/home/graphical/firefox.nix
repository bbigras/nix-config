{ lib, pkgs, ... }:
{
  programs.firefox = {
    enable = true;
    package =
      let
        ff =
          if lib.meta.availableOn pkgs.stdenv.hostPlatform pkgs.firefox-bin then
            pkgs.firefox-bin
          else
            pkgs.firefox;
      in
      ff.override { };

    policies."3rdparty".Extensions."uBlock0@raymondhill.net" = {
      userSettings = [
        [ "advancedUserEnabled" "true" ]
      ];

      toOverwrite.filterLists = [
        "user-filters"
        "ublock-filters"
        "ublock-badware"
        "ublock-privacy"
        "ublock-quick-fixes"
        "ublock-unbreak"
        "easylist"
        "easyprivacy"
        "adguard-spyware-url"
        "urlhaus-1"
        "plowe-0"
        "fanboy-cookiemonster"
        "ublock-cookies-easylist"
        "fanboy-social"
        "fanboy-ai-suggestions"
        "easylist-chat"
        "easylist-newsletters"
        "easylist-notifications"
        "easylist-annoyances"
        "ublock-annoyances"
        "FRA-0"
        "https://cdn.jsdelivr.net/gh/hagezi/dns-blocklists@latest/adblock/pro.mini.txt"
        "https://raw.githubusercontent.com/yokoffing/filterlists/main/privacy_essentials.txt"
      ];

      toAdd.trustedSiteDirectives = [
        "crm.democratik.org"
        "www.greenmangaming.com"
      ];
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
