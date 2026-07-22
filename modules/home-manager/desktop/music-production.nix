{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    reaper # full-featured DAW
    # ardour # FOSS DAW, can't run executable
  ];
}
