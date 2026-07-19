{ ... }: {
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;

    settings = {
      "*" = {
        IdentityFile = "~/.ssh/personal_yubi";
        IdentitiesOnly = true;
        AddKeysToAgent = "yes";
      };
      "github.com" = {
        HostName = "github.com";
        User = "git";
      };
      "truenas.local" = {
        HostName = "truenas.local";
        User = "admin";
        Port = 69;
      };
    };
  };
}
