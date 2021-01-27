{ config, home-manager, ... }:
{
  home-manager.users.venikx.xdg.enable = true;

  environment = {
    sessionVariables = {
      # These are the defaults, and xdg.enable does set them, but due to load
      # order, they're not set before environment.variables are set, which could
      # cause race conditions.
      XDG_CACHE_HOME  = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME   = "$HOME/.local/share";
      XDG_BIN_HOME    = "$HOME/.local/bin";
    };
   variables = {
      # Conform more programs to XDG conventions. The rest are handled by their
      # respective modules.
      HISTFILE        = "$XDG_DATA_HOME/bash/history";
      INPUTRC         = "$XDG_CONFIG_HOME/readline/inputrc";
      LESSHISTFILE    = "$XDG_CACHE_HOME/lesshst";
      WGETRC          = "$XDG_CONFIG_HOME/wgetrc";
   };

   # Move ~/.Xauthority out of $HOME (setting XAUTHORITY early isn't enough)
   extraInit = ''
     export XAUTHORITY=/tmp/Xauthority
     [ -e ~/.Xauthority ] && mv -f ~/.Xauthority "$XAUTHORITY"
   '';
  };
}
