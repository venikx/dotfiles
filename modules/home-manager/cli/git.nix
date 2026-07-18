{ ... }:
{
  programs.zsh.shellAliases = {
    git = "noglob git";
    ga = "git add";
    gap = "git add --patch";
    gb = "git branch -av";
    gc = "git commit";
    gcm = "git commit -m";
    gca = "git commit --amend";
    gl = ''git log --graph --pretty="format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset"'';
    gll = ''git log --pretty="format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset"'';
    gL = "gl --stat";
    gss = "git status";
    gs = "git status --short .";
    gst = "git stash";
    gr = "git reset HEAD";
    grv = "git rev-parse";
  };

  programs.delta.enable = true;

  programs.git = {
    enable = true;
    lfs.enable = true;
    signing.key = "AA445668182C2AD4";

    settings = {
      user = {
        email = "code@venikx.com";
        name = "Kevin De Baerdemaeker";
      };
      alias = {
        "lg" =
          "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --";
        "ls" = "ls-files";
        "undo-commit" = ''reset --soft "HEAD^"'';
      };
      core = {
        editor = "nvim";
        autocrlf = "input";
        ignorecase = false;
        whitespace = "trailing-space";
      };
      init.defaultBranch = "main";
      push.default = "simple";
      pull.rebase = true;
      credential.helper = "helper = cache --timeout=3600";
      url."https://github.com/".insteadOf = "gh";
    };

    ignores = [
      # Ignore tags (from etags and ctags)
      "TAGS"
      "!TAGS/"
      "tags"
      "!tags/"

      # Logs and databases
      "*.log"
      "*.cache"

      # OS generated files
      ".DS_Store?"
      ".DS_Store"
      ".CFUserTextEncoding"
      ".Trash"
      ".Xauthority"
      "thumbs.db"
      "Icon\\?"
      "Thumbs.db"
      ".cache"
      ".pid"
      ".sock"

      # Code stuffs
      ".svn"
      ".git"
      ".swp"
      ".idea"
      ".*.swp"
      "*~"
      ".tags"
      "tags"
      ".sass-cache"
      "tmp"
      ".codekit-cache"
      "config.codekit"

      # Compiled thangs
      "*.class"
      "*.exe"
      "*.o"
      "*.so"
      "*.dll"
      "*.pyc"
    ];
  };
}
