{ config, lib, pkgs, ... }:

{
  system.defaults =  {
    dock.mru-spaces = false;
    CustomSystemPreferences = {
      "com.apple.Accessibility" = {
        "ReduceMotionEnabled" = true;
      };
    };
    CustomUserPreferences = {
      "com.apple.Accessibility" = {
        "ReduceMotionEnabled" = true;
      };
    };
  };
}
