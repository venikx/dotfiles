OS="$(uname -s)"

# ------------------------------
# 1. Misc
# ------------------------------
export EDITOR=/usr/bin/nvim
export BROWSER=/usr/bin/firefox-nightly
export HISTFILE="${HOME}/.zsh_history"
export HISTSIZE=1000
export SAVEHIST=1000
export KEYTIMEOUT=1
export PASSWORD_STORE_X_SELECTION="clipboard"
export PASSWORD_STORE_CLIP_TIME=15


# ------------------------------
# 2. Programming Configuration
# ------------------------------
# NVM
[ -d "$HOME/.nvm" ] && export NVM_DIR="$HOME/.nvm"
# if test "$OS" = "Darwin"; then
#     NVM_HOMEBREW="/usr/local/opt/nvm/nvm.sh"
#     [ -s "$NVM_HOMEBREW" ] && \. "$NVM_HOMEBREW"
# elif test "$OS" = "Linux"; then
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
#fi

# Rust
[ -d "$HOME/.cargo/bin:$PATH" ] && export PATH="$HOME/.cargo/bin:$PATH"
type rustc &> /dev/null && [ -d "$(rustc --print sysroot)/lib/rustlib/src/rust/src" ] && export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"


# ------------------------------
# 2. GPG and SSH-agents
# ------------------------------
GPG_TTY=$(tty)
export GPG_TTY
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpg-connect-agent updatestartuptty /bye > /dev/null
