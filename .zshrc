########################################
# ~/.zshrc
# Zsh settings
#
# Structure:
#   -> Start-up
#   -> Navigation
#   -> Completion
#   -> History
#   -> Prompt
#   -> Quality of Life
#   -> Variables
#   -> Keybindings
#   -> Functions
########################################

########################################
# Start-up
########################################
autoload -Uz colors && colors
[[ $DISPLAY != "" &&  "$(tty)" == "/dev/tty1" ]] && exec startx

########################################
# Navigation
########################################
# Moving around folders doesn't require cd
setopt cdable_vars
setopt auto_cd
# Automatically follow symlinks
setopt chase_links

########################################
# Completion
########################################
autoload -Uz compinit && compinit
setopt completealiases
# Attempt to autocomplete by itself
setopt list_ambiguous
setopt complete_in_word
setopt always_to_end
# Display menu with possible completions
setopt auto_menu
zstyle ':completion:*' menu select
# When completing a directory name, add a trailing slash
setopt auto_param_slash
# When listing files as completions, show the file type as a symbol
setopt list_types

########################################
# History
########################################
# History addition
setopt inc_append_history
setopt share_history
setopt hist_reduce_blanks
# Remove unwanted history
setopt hist_ignore_space
setopt hist_no_store
setopt hist_ignore_dups
setopt hist_find_no_dups
setopt hist_expire_dups_first
# Search history like bash
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey '^R' history-incremental-search-backward
bindkey  '^P'  history-beginning-search-backward-end
bindkey '^N' history-beginning-search-forward-end

########################################
# Prompt
########################################
# vcs => vcs_info is a function that populates a variable for you
# http://arjanvandergaag.nl/blog/customize-zsh-prompt-with-vcs-info.html
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' formats "%{$fg[yellow]%}%c%{$fg[green]%}%u%{$reset_color%} [%{$fg[blue]%}%b%{$reset_color%}]"
precmd() {  # run before each prompt
  vcs_info
}

local user_name="%{$fg[cyan]%}%n%{$reset_color%}"
local host_name="%{$fg[blue]%}%m%{$reset_color%}"
local path_string="%{$fg[magenta]%}%1d%{$reset_color%}"
local prompt_string="%{$fg[cyan]%}λ%{$reset_color%}"

setopt PROMPT_SUBST     # allow funky stuff in prompt
PROMPT='${user_name}@${host_name} %{$reset_color%}${prompt_string} '
RPROMPT='${path_string} ${vcs_info_msg_0_} %{$reset_color%}'

########################################
# Quality of life
########################################
setopt print_exit_value         # print return value if non-zero
unsetopt beep                   # no bell on error
unsetopt hist_beep              # no bell on error in history
unsetopt hup                    # no hup signal at shell exit
unsetopt list_beep              # no bell on ambiguous completion
unsetopt nomatch                # prevent "no matches found" error, when using wildcards
setopt extended_glob            # activate complex pattern globbing
setopt glob_dots                # include dotfiles in globbing

########################################
# Variables
########################################
HISTFILE="${HOME}/.zsh_history"
HISTSIZE=1000
SAVEHIST=1000
EDITOR="vim"
KEYTIMEOUT=1
PASSWORD_STORE_X_SELECTION="clipboard"
PASSWORD_STORE_CLIP_TIME=15

alias ls="ls -F --color=always"
alias ll="ls -la --color=always"
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"
alias reload="source ~/.zshrc"
eval $(thefuck --alias)
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

## NVM
[ -d "$HOME/.nvm" ]      && export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

########################################
# Keybindings
########################################
bindkey -e

########################################
# Functions
########################################
export run_beige() {
  cd ~/git/beige && docker-compose up -d --build &&
  cd ~/git/login-app && docker-compose up -d  --build &&
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

