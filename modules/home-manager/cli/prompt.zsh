setopt prompt_subst
autoload -U colors && colors

## Hooks ###############################
prompt_hook_precmd() {
  vcs_info # get git info
  # Newline before prompt, except on init
  [[ -n $PROMPT_DONE ]] && print ""; PROMPT_DONE=1
}

## Initialization ######################
prompt_init() {
  # prevent percentage showing up
  # if output doesn't end with a newline
  export PROMPT_EOL_MARK=

  # prompt_opts=(cr subst percent)
  setopt promptsubst
  autoload -Uz add-zsh-hook
  autoload -Uz vcs_info

  add-zsh-hook precmd prompt_hook_precmd
  # Updates cursor shape and prompt symbol based on vim mode
  zle-keymap-select() {
    case $KEYMAP in
      vicmd)      PROMPT_SYMBOL="%F{green}« " ;;
      main|viins) PROMPT_SYMBOL="%(?.%F{magenta}.%F{yellow})λ " ;;
    esac
    zle reset-prompt
    zle -R
  }
  zle -N zle-keymap-select
  zle -A zle-keymap-select zle-line-init

  zstyle ':vcs_info:*' enable git
  zstyle ':vcs_info:*' use-simple true
  zstyle ':vcs_info:*' max-exports 2
  zstyle ':vcs_info:*' check-for-changes true
  zstyle ':vcs_info:*' unstagedstr '%F{yellow}●%f'
  zstyle ':vcs_info:*' stagedstr '%F{green}●%f'
  zstyle ':vcs_info:git*' formats ' %u%c %b'
  zstyle ':vcs_info:git*' actionformats ' %u%c %b(%a)'

  # show username@host if logged in through SSH
  if [[ -n $SSH_CONNECTION ]]; then
    prompt_username='%m '
    if [[ $(whoami) != venikx ]]; then
      prompt_username="%n.$prompt_username"
    fi
  fi


  RPROMPT='%F{blue}%~%F{magenta}${vcs_info_msg_0_}%f'
  PROMPT='%F{magenta}${prompt_username}%f${PROMPT_SYMBOL:-$ }%f'
}

prompt_init "$@"
