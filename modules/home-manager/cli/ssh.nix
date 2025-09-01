{ ... }: {
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    
    matchBlocks = {
      "github.com" = {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/personal_yubi"; 
        identitiesOnly = true;
        addKeysToAgent = "yes";
      };
    };
  };
}
