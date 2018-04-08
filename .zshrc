#!/bin/zsh -f
autoload -Uz colors && colors

# Key bindings
bindkey -v
bindkey '^R' history-incremental-search-backward

# Aliases
alias -g ls="ls -F --color=always"
alias -g ll="ls -la --color=always"
alias -g cp="cp -i"
alias -g mv="mv -i"
alias -g rm="rm -i"
alias reload="source ~/.zshrc"

# env
export HISTFILE="~/.zsh_history"
export HISTSIZE=1000
export SAVEHIST=1000
export EDITOR="vim"
export KEYTIMEOUT=1
export PASSWORD_STORE_X_SELECTION="clipboard"
export PASSWORD_STORE_CLIP_TIME=15

# History
setopt append_history           # append
setopt hist_ignore_all_dups     # no duplicate
unsetopt hist_ignore_space      
setopt hist_reduce_blanks       # trim blanks
setopt inc_append_history       # add commands as they are typed, don't wait until shell exit 
setopt share_history            # share hist between sessions

# Completion
autoload -Uz compinit && compinit
setopt completealiases 
setopt list_ambiguous           # complete as much as possible
setopt complete_in_word         # complete from within the word
setopt always_to_end            # move cursor to end of word, when completing from middle
zstyle ':completion:*' menu select

# vcs
# vcs_info is a function that populates a variable for you
# http://arjanvandergaag.nl/blog/customize-zsh-prompt-with-vcs-info.html
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' formats "%{$fg[yellow]%}%c%{$fg[green]%}%u%{$reset_color%} [%{$fg[blue]%}%b%{$reset_color%}]"
precmd() {  # run before each prompt
  vcs_info
}

# Prompt
local user_name="%{$fg[cyan]%}%n%{$reset_color%}"
local host_name="%{$fg[blue]%}%m%{$reset_color%}"
local path_string="%{$fg[grey]%}%~"
local prompt_string="%{$fg[cyan]%}λ%{$reset_color%}"

setopt PROMPT_SUBST     # allow funky stuff in prompt
PROMPT='${user_name}@${host_name} %{$reset_color%}${prompt_string} '
RPROMPT='${path_string} ${vcs_info_msg_0_} %{$reset_color%}'

# Quality of life
setopt auto_cd                  # if path, cd into it
setopt chase_links              # resolve symlinks
setopt print_exit_value         # print return value if non-zero
unsetopt beep                   # no bell on error
unsetopt hist_beep              # no bell on error in history
unsetopt hup                    # no hup signal at shell exit
unsetopt list_beep              # no bell on ambiguous completion
unsetopt nomatch                # prevent "no matches found" error, when using wildcards
setopt extended_glob            # activate complex pattern globbing
setopt glob_dots                # include dotfiles in globbing

# Functions
export docker_cleanup() {
  docker volume rm $(docker volume ls -qf dangling=true)
}

export run_beige() {
  cd ~/git/beige && docker-compose up -d &&
  cd ~/git/login-app && docker-compose up -d &&
  cd ~/git/fms && docker-compose up -d && 
  cd
}

export run_vpn() {
  cd ~/.openvpn && sudo openvpn --config dev.ovpn --daemon &&
  sudo openvpn --config prod.ovpn --daemon &&
  cd
}

export build_ops() {
  cd ~/git/ops &&
  docker run -it --rm --name ops \
    --volume /path/to/.gnupg:/root/.gnupg \
    --volume $HOME/git/ops:/root/famops \
    --volume $SSH_AUTH_SOCK:/ssh-agent \
    --volume $HOME/.aws:/root/.aws/ \
    --env SSH_AUTH_SOCK=/ssh-agent \
    --env FAMOCO_USER="kdebaerdemaeker" docker-registry2.dev.famoco.com:5000/ops  
}

export run_ops() {
  cd ~/git/ops &&
  docker start ops &&
  docker exec -it ops bash
}

