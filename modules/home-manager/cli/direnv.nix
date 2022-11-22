{ config, ... }:
{
  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
      enableZshIntegration = true;
      config = {
        whitelist = {
          prefix = [
            "${config.home.homeDirectory}/code" ];
        };
      };
    };

    git.ignores = [ ".direnv" ];
  };
}
