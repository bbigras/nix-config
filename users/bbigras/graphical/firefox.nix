{ pkgs, ... }: {
  programs.firefox = {
    enable = (pkgs.hostPlatform.system == "x86_64-linux");
    package = pkgs.firefox-bin;

    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      anchors-reveal
      auto-tab-discard
      bypass-paywalls-clean
      cookies-txt
      facebook-container
      french-dictionary
      french-language-pack
      https-everywhere
      i-dont-care-about-cookies
      ipfs-companion
      languagetool
      link-cleaner
      linkhints
      localcdn
      markdownload
      offline-qr-code-generator
      privacy-badger
      private-relay
      react-devtools
      reddit-enhancement-suite
      rust-search-extension
      tree-style-tab
      ublock-origin
    ];

    profiles = {
      default = {
        isDefault = true;
        settings = {
          "beacon.enabled" = false;
          "browser.display.background_color" = "#c5c8c6";
          "browser.display.foreground_color" = "#1d1f21";
          # "browser.download.dir" = "${config.home.homeDirectory}/download";
          "browser.safebrowsing.appRepURL" = "";
          "browser.safebrowsing.malware.enabled" = false;
          "browser.newtabpage.activity-stream.showSponsored" = false;
          "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;

          "browser.search.hiddenOneOffs" =
            "Google,Yahoo,Bing,Amazon.com,Amazon.ca,Twitter";
          "browser.search.suggest.enabled" = false;
          "browser.send_pings" = false;
          "browser.startup.page" = 3;
          "browser.tabs.closeWindowWithLastTab" = false;
          "browser.uidensity" = 1; # Dense.
          "browser.urlbar.placeholderName" = "DuckDuckGo";
          "browser.urlbar.speculativeConnect.enabled" = false;
          # "devtools.theme" = "${config.theme.base16.kind}";
          "dom.battery.enabled" = false;
          # "dom.event.clipboardevents.enabled" = false;
          "experiments.activeExperiment" = false;
          "experiments.enabled" = false;
          "experiments.supported" = false;
          "extensions.pocket.enabled" = false;
          "fission.autostart" = true;
          "general.smoothScroll" = false;
          "geo.enabled" = false;
          "gfx.webrender.all" = true;
          # "layout.css.devPixelsPerPx" = "1.5";
          # "layout.css.devPixelsPerPx"
          "media.navigator.enabled" = false;
          "media.video_stats.enabled" = false;
          "network.IDN_show_punycode" = true;
          "network.allow-experiments" = false;
          "network.dns.disablePrefetch" = true;
          "network.http.referer.XOriginPolicy" = 2;
          "network.http.referer.XOriginTrimmingPolicy" = 2;
          "network.http.referer.trimmingPolicy" = 1;
          "network.prefetch-next" = false;
          "permissions.default.shortcuts" = 2; # Don't steal my shortcuts!
          "privacy.donottrackheader.enabled" = true;
          "privacy.donottrackheader.value" = 1;
          "privacy.firstparty.isolate" = true;
          # "signon.rememberSignons" = false;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "widget.content.gtk-theme-override" = "Adwaita:light";
        };

        userChrome = ''
          @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul");

          #TabsToolbar {
            visibility: collapse;
          }

          #titlebar {
            display: none;
          }

          #sidebar-header {
            display: none;
          }
        '';
      };
    };
  };
}
