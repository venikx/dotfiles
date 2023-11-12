{ config, pkgs, ... }:

{
  home = {
    sessionVariables = { BROWSER = "google-chrome-stable"; };
    # TODO(Kevin): Replace with Thorium when available
    packages = with pkgs; [ tor google-chrome ];
  };
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "x-scheme-handler/http" = [ "google-chrome.desktop" ];
      "x-scheme-handler/https" = [ "google-chrome.desktop" ];
    };
  };
}
