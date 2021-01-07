OS="$(uname -s)"

# ------------------------------
# 1. Programming Configuration
# ------------------------------
# Rust
[ -d "$HOME/.cargo/bin:$PATH" ] && export PATH="$HOME/.cargo/bin:$PATH"
type rustc &> /dev/null && [ -d "$(rustc --print sysroot)/lib/rustlib/src/rust/src" ] && export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
