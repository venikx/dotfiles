{ ... }: {
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    settings = {
      "*" = {
        IdentityFile = [
          "~/.ssh/personal_yubi"
          "~/.ssh/personal_sm_yubi"
        ];
        IdentitiesOnly = true;
        AddKeysToAgent = "yes";
      };
      "github.com" = {
        HostName = "github.com";
        User = "git";
        ControlMaster = "auto";
        ControlPath = "~/.ssh/control-%n@%r:%p";
        ControlPersist = "600";
      };
      "truenas.local" = {
        HostName = "truenas.local";
        User = "admin";
        Port = 69;
      };
    };
  };
}
