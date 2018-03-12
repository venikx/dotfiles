export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

export NPM_TOKEN=$(cat $HOME/.npmrc|grep npm.famoco.com/repository/npm-all|egrep -o "authToken=(.+)" | cut -d= -f2)

export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

