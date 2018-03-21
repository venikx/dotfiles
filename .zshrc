# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt extendedglob
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/anarobynn/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# User specific aliases and functions
bindkey -v
bindkey '^R' history-incremental-search-backward

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


