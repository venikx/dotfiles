{ config, lib, pkgs, ... }:

# TODO(Kevin): Covert to braindump autoscript
let
  gtd-sync-script = (pkgs.writeShellScript "gtd-sync" ''
    ${pkgs.rclone}/bin/rclone sync -u ./org/gtd 99-org:/gtd --timeout 10s --contimeout 5s;
    ${pkgs.rclone}/bin/rclone sync -u 99-org:/gtd ./org/gtd  --timeout 10s --contimeout 5s
  '');
in {
  # NOTE(Kevin): The rclone configuration wizard still needs to be run when the device is
  # is setup for the first time though.
  home.packages = [ pkgs.rclone ];

  systemd.user.timers."gtd-sync" = {
    Timer = {
      OnCalendar = "*:0/15";
      Persistent = true;
    };
    Install.WantedBy = [ "timers.target" ];
  };

  systemd.user.services."gtd-sync" = {
    Install.WantedBy = [ "default.target" ];
    Service = {
      Type = "oneshot";
      ExecStart = "${gtd-sync-script}";
    };
  };
}
