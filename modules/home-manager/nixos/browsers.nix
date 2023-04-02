{ config, pkgs, ... }:

{
  home = {
    sessionVariables = {
      BROWSER = "firefox";
    };
    packages = with pkgs; [
      tor
    ];
  };
  programs = {
    chromium.enable = true;
    firefox = {
      enable = true;
      profiles.venikx = {
        settings = {
          "devtools.theme" = "dark";
          "browser.download.dir" = "${config.home.homeDirectory}/dl"; # Stop creating ~/Downloads!
          "signon.rememberSignons" = false; # Don't use the built-in password manager
          "browser.shell.checkDefaultBrowser" = false;
          "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
          # Enable ETP for decent security (makes firefox containers and many
          # common security/privacy add-ons redundant).
          "browser.contentblocking.category" = "strict";
          "privacy.donottrackheader.enabled" = true;
          "privacy.donottrackheader.value" = 1;
          "privacy.purge_trackers.enabled" = true;
          # Disable new tab bloat
          "browser.newtabpage.enabled" = false;
          "browser.newtab.url" = "about:blank";
          "browser.newtabpage.activity-stream.enabled" = false;
          "browser.newtabpage.enhanced" = false;
          "browser.newtab.preload" = false;
          "browser.newtabpage.directory.ping" = "";
          "browser.newtabpage.directory.source" = "data:text/plain,{}";
          "browser.newtabpage.introShown" = true;
          # Reduce search engine noise in the urlbar's completion window. The
          # shortcuts and suggestions will still work, but Firefox won't clutter
          # its UI with reminders that they exist.
          "browser.urlbar.suggest.searches" = false;
          "browser.urlbar.shortcuts.bookmarks" = false;
          "browser.urlbar.shortcuts.history" = false;
          "browser.urlbar.shortcuts.tabs" = false;
          "browser.urlbar.showSearchSuggestionsFirst" = false;
          "browser.urlbar.speculativeConnect.enabled" = false;
          # https://bugzilla.mozilla.org/1642623
          "browser.urlbar.dnsResolveSingleWordsAfterSearch" = 0;
          # https://blog.mozilla.org/data/2021/09/15/data-and-firefox-suggest/
          "browser.urlbar.suggest.quicksuggest.nonsponsored" = false;
          "browser.urlbar.suggest.quicksuggest.sponsored" = false;
          # Show whole URL in address bar
          "browser.urlbar.trimURLs" = false;
          # Reduce File IO / SSD abuse
          # Otherwise, Firefox bombards the HD with writes. Not so nice for SSDs.
          # This forces it to write every 30 minutes, rather than 15 seconds.
          "browser.sessionstore.interval" = "1800000";
          # Disable some not so useful functionality.
          "extensions.htmlaboutaddons.recommendations.enabled" = false;
          "extensions.htmlaboutaddons.discover.enabled" = false;
          "extensions.pocket.enabled" = false;
          "app.normandy.enabled" = false;
          "app.normandy.api_url" = "";
          "app.shield.optoutstudies.enabled" = false;
          "extensions.shield-recipe-client.enabled" = false;
          # Disable vulnerable API's
          "dom.battery.enabled" = false;
          "dom.gamepad.enabled" = false;
          # Disable analytics & tracking
          "beacon.enabled" = false;
          "browser.send_pings" = false;
          "toolkit.telemetry.unified" = false;
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.server" = "data:,";
          "toolkit.telemetry.archive.enabled" = false;
          "toolkit.telemetry.coverage.opt-out" = true;
          "toolkit.coverage.opt-out" = true;
          "toolkit.coverage.endpoint.base" = "";
          "experiments.supported" = false;
          "experiments.enabled" = false;
          "experiments.manifest.uri" = "";
          "browser.ping-centre.telemetry" = false;
          "datareporting.healthreport.uploadEnabled" = false;
          "datareporting.healthreport.service.enabled" = false;
          "datareporting.policy.dataSubmissionEnabled" = false;
          # Don't try to guess domain names when entering an invalid domain name in URL bar
          # http://www-archive.mozilla.org/docs/end-user/domain-guessing.html
          "browser.fixup.alternate.enabled" = false;
          # Disable crash reports
          "breakpad.reportURL" = "";
          "browser.tabs.crashReporting.sendReport" = false;
          "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;  # don't submit backlogged reports
          # Disable Form autofill
          #"browser.formfill.enable" = false;
          #"extensions.formautofill.addresses.enabled" = false;
          #"extensions.formautofill.available" = "off";
          #"extensions.formautofill.creditCards.available" = false;
          #"extensions.formautofill.creditCards.enabled" = false;
          #"extensions.formautofill.heuristics.enabled" = false;
        };
        search = {
          default = "DuckDuckGo";
          force = true;
          order = [
            "DuckDuckGo"
            "Google"
          ];
          engines = {
            "Bing".metaData.hidden = true;
            "Google".metaData.alias = "@g";
            "NixOS Wiki" = {
              urls = [{ template = "https://nixos.wiki/index.php?search={searchTerms}"; }];
              iconUpdateURL = "https://nixos.wiki/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000; # every day
              definedAliases = [ "@nw" ];
            };
            "Nix Packages" = {
              urls = [{
                template = "https://search.nixos.org/packages";
                params = [
                  { name = "type"; value = "packages"; }
                  { name = "query"; value = "{searchTerms}"; }
                ];
              }];

              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@np" ];
            };
            "NixOS Options" = {
              urls = [{
                template = "https://search.nixos.org/options";
                params = [
                  { name = "type"; value = "options"; }
                  { name = "query"; value = "{searchTerms}"; }
                ];
              }];

              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@no" ];
            };
          };
        };
      };
    };
  };
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "x-scheme-handler/http" = [ "firefox.desktop" ];
      "x-scheme-handler/https" = [ "firefox.desktop" ];
    };
  };
}
