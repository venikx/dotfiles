########################################
# ~/.zshrc
# Zsh settings
#
# Structure:
#   1. Environment Configuration
#   2. Better Defaults
#   3. Prompt
#   4. Utility Functions / Aliases
########################################

# ------------------------------
# 1. ENVIRONMENT CONFIGURATION
# ------------------------------

# See .zshenv


# ------------------------------
# 2. BETTER DEFAULTS
# ------------------------------

# Aliases
alias reload="source ~/.zshrc"
eval $(thefuck --alias)
alias dots='/usr/bin/git --git-dir=$HOME/.dots/ --work-tree=$HOME'
alias mkdir='mkdir -pv'
alias ls="ls -F"
alias ll="ls -FGlAhp"
alias cp="cp -iv"
alias mv="mv -iv"
alias rm="rm -iv"
cd() { builtin cd "$@"; ll; }
alias cd..='cd ../'
alias ..='cd ../'
alias ...='cd ../../'
alias fix_stty='stty sane'
alias c='clear'

# Navigation
setopt cdable_vars # Moving around folders doesn't require cd
setopt auto_cd
setopt chase_links # Automatically follow symlinks

# Completion
autoload -Uz compinit && compinit
setopt completealiases
setopt list_ambiguous  # Attempt to autocomplete by itself
setopt complete_in_word
setopt always_to_end
setopt auto_menu # Display menu with possible completions
zstyle ':completion:*' menu select
setopt auto_param_slash # When completing a directory name, add a trailing slash
setopt list_types # When listing files as completions, show the file type as a symbol

# History
setopt inc_append_history  # History addition
setopt share_history
setopt hist_reduce_blanks
setopt hist_ignore_space # Remove unwanted history
setopt hist_no_store
setopt hist_ignore_dups
setopt hist_find_no_dups
setopt hist_expire_dups_first
autoload history-search-end # Search history like bash
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey '^R' history-incremental-search-backward
bindkey  '^P'  history-beginning-search-backward-end
bindkey '^N' history-beginning-search-forward-end

# Misc
bindkey -e
setopt print_exit_value         # print return value if non-zero
unsetopt beep                   # no bell on error
unsetopt hist_beep              # no bell on error in history
unsetopt hup                    # no hup signal at shell exit
unsetopt list_beep              # no bell on ambiguous completion
unsetopt nomatch                # prevent "no matches found" error, when using wildcards
setopt extended_glob            # activate complex pattern globbing
setopt glob_dots                # include dotfiles in globbing

# Colors
(cat ~/.cache/wal/sequences &)
source ~/.cache/wal/colors-tty.sh

########################################
# Start-up
########################################
autoload -Uz colors && colors

# ------------------------------
# 3. PROMPT
# ------------------------------

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

# ------------------------------
# 3. UTILITY FUNCTIONS
# ------------------------------

# Docker
alias dps='docker ps -a'
alias di='docker images -a'
alias dv='docker volume ls'
alias dn='docker network ls'
alias sprune='docker system prune'
alias vprune='docker volume prune'
