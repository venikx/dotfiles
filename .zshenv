export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

if [ -f $HOME/.npmrc ]; then
	export NPM_TOKEN=$(cat $HOME/.npmrc|grep npm.famoco.com/repository/npm-all|egrep -o "authToken=(.+)" | cut -d= -f2)
fi

export PATH="$HOME/.cargo/bin:$PATH"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

