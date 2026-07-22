{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    davinci-resolve-studio
    gimp-with-plugins
    ffmpeg
  ];

  programs.obs-studio = {
    enable = true;
    plugins = [ ];
  };
}
