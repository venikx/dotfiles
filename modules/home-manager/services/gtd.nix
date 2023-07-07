{ config, lib, pkgs, ... }:

{
  # NOTE(Kevin): The rclone configuration wizard still needs to be run when the device is
  # is setup for the first time though.
  home.packages = [ pkgs.rclone ];
  systemd.user.timers."gtd-sync" = {
    Timer = {
      OnCalendar = "*:0/1";
      Persistent = true;
    };
    Install.WantedBy = [ "timers.target" ];
  };

  systemd.user.services."gtd-sync" = {
    Install.WantedBy = [ "default.target" ];
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.rclone}/bin/rclone sync ./org/gtd 99-org:/gtd --timeout 10s --contimeout 5s";
    };
  };
}
