{ config, lib, pkgs, ... }:

{
  homebrew.taps = [ "koekeishiya/formulae" ];
  homebrew.brews = [
    {
      name = "yabai";
      restart_service = "changed";
    }
    {
      name = "skhd";
      restart_service = "changed";
    }
  ];
  home-manager.users.venikx = {
    xdg.configFile."yabai/yabairc" = {
      executable = true;
      text = ''
        #!/bin/sh
        yabai -m config layout                       bsp
        yabai -m config window_placement             second_child
        yabai -m config focus_follows_mouse          autoraise
        yabai -m config mouse_follows_focus          on
        yabai -m config mouse_modifier               alt
        yabai -m config mouse_action1                move
        yabai -m config mouse_action2                resize

        yabai -m rule --add app='System Settings' manage=off
      '';
    };

    xdg.configFile."skhd/skhdrc" = {
      text = ''
        hyper - j : yabai -m window --focus south
        hyper - k : yabai -m window --focus north
        hyper - h : yabai -m window --focus west
        hyper - l : yabai -m window --focus east

        meh - j : yabai -m window --swap south
        meh - k : yabai -m window --swap north
        meh - h : yabai -m window --swap west
        meh - l : yabai -m window --swap east

        meh - f : yabai -m window --toggle float --grid 4:4:1:1:2:2
        hyper - f : yabai -m window --toggle zoom-fullscreen
        meh - s : yabai -m window --toggle split

        # Broken atm on Ventura :/
        hyper - 1 : yabai -m space --focus 1
        hyper - 2 : yabai -m space --focus 2
        hyper - 3 : yabai -m space --focus 3
        hyper - 4 : yabai -m space --focus 4
        hyper - 5 : yabai -m space --focus 5
        hyper - 6 : yabai -m space --focus 6
        hyper - 7 : yabai -m space --focus 7

        meh - 1 : yabai -m window --space 1
        meh - 2 : yabai -m window --space 2
        meh - 3 : yabai -m window --space 3
        meh - 4 : yabai -m window --space 4
        meh - 5 : yabai -m window --space 5
        meh - 6 : yabai -m window --space 6
        meh - 7 : yabai -m window --space 7

        hyper - space : yabai -m window --swap recent
      '';
    };
  };

  #services.yabai = {
  #  enable = true;
  #  enableScriptingAddition = true;
  #  config = {
  #    layout = "bsp";
  #    window_placement    = "second_child";
  #    focus_follows_mouse = "autoraise";
  #    mouse_follows_focus = "off";
  #    window_opacity      = "off";

  #    # dragging & resizing
  #    mouse_modifier = "alt";
  #    mouse_action2 = "resize";
  #  };
  #  extraConfig = ''
  #    yabai -m rule --add app='System Settings' manage=off
  #  '';
  #};

  #services.skhd = {
  #  enable = true;
  #  skhdConfig = ''
  #    hyper - j : yabai -m window --focus south
  #    hyper - k : yabai -m window --focus north
  #    hyper - h : yabai -m window --focus west
  #    hyper - l : yabai -m window --focus east

  #    meh - j : yabai -m window --swap south
  #    meh - k : yabai -m window --swap north
  #    meh - h : yabai -m window --swap west
  #    meh - l : yabai -m window --swap east

  #    meh - f : yabai -m window --toggle float --grid 4:4:1:1:2:2
  #    hyper - f : yabai -m window --toggle zoom-fullscreen
  #    meh - s : yabai -m window --toggle split

  #    hyper - 1 : yabai -m space --focus 1
  #    hyper - 2 : yabai -m space --focus 2
  #    hyper - 3 : yabai -m space --focus 3
  #    hyper - 4 : yabai -m space --focus 4
  #    hyper - 5 : yabai -m space --focus 5
  #    hyper - 6 : yabai -m space --focus 6
  #    hyper - 7 : yabai -m space --focus 7

  #    # Broken atm :/
  #    meh - 1 : yabai -m window --space 1
  #    meh - 2 : yabai -m window --space 2
  #    meh - 3 : yabai -m window --space 3
  #    meh - 4 : yabai -m window --space 4
  #    meh - 5 : yabai -m window --space 5
  #    meh - 6 : yabai -m window --space 6
  #    meh - 7 : yabai -m window --space 7

  #    hyper - space : yabai -m window --swap recent
  #  '';
  #};
}
