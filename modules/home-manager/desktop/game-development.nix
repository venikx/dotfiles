{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    reaper
    tenacity
    blender
    godot_4
    gimp-with-plugins
  ];
}
