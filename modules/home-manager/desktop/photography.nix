{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    darktable
    gimp-with-plugins
  ];
}
