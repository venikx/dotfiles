#!/bin/sh
set -e
sudo -v
start=`date +%s`

titleMessage() {
    echo
    echo "=============================="
    echo "$1"
    echo "=============================="
}

titleMessage "PREPARE PKG MANAGERS"
sudo pacman -Syuu --noconfirm
yay -Syu --noconfirm

titleMessage "INSTALLING PACKAGES"
Packages=(
    git
    curl
    bat
    ccid            # Needed for smartcards
    picom           # Replacement for comptom 
    neovim
    ranger
    ripgrep         # Replacement for grep
    unzip
    zsh
    thefuck
    xorg
    xorg-xclipboard
    scrot
    rxvt-unicode
    gimp
    imagemagick
    feh
    python-pywal    # Used to set the colours
    gcc
    clang
    vlc
    obs-studio
    qbittorrent
    pulseaudio
    pulseaudio-alsa
    manjaro-pulse
    pavucontrol
    pa-applet
    v4l-utils       # Edit video and camera settings
    docker
    docker-compose
    i3lock
    openvpn
    pcsc-tools
    yubikey-personalization
    code
)
sudo pacman -S --noconfirm --needed ${Packages[@]}

titleMessage "INSTALLING AUR PACKAGES"
AURPackages=(
    i3status-rust
    nvm
    chromium
    firefox-nightly
    slack
    discord
    # spotify
    emacs-git
)
yay -S --noconfirm ${AURPackages[@]}

titleMessage "RETRIEVING DOTFILES"
putgitrepo() {
    dir=$(mktemp -d)
    [ ! -d "$2" ] && mkdir -p "$2"
	git clone "$1" "$dir"
	cp -rfT "$dir" "$2"
}

putgitrepo "https://gitlab.com/venikx/dotfiles.git" "/home/venikx"
git config status.showUntrackedFiles no

titleMessage "MANAGING SERVICES"
systemctl disable lightdm.service
sudo systemctl start docker
sudo systemctl enable docker
sudo systemctl start pcscd
sudo systemctl enable pcscd
killall pulseaudio; pulseaudio --start

titleMessage "CONFIGURE DOOM EMACS"
~/.emacs.d/bin/doom install
~/.emacs.d/bin/doom sync

titleMessage "RUN PYWAL TO SET COLOURS"
wal -i ~/wallpapers/cyberpunk-street.jpg

titleMessage "CHANGING THE SHELL"
chsh -s $(which zsh) 
mkdir -p "~/.cache/zsh/"
exec zsh

titleMessage "CONFIGURING NVM AND NPM"
source /usr/share/nvm/init-nvm.sh
nvm install --lts
npm i -g yarn
npm i -g vscode-css-languageserver-bin
npm i -g vscode-html-languageserver-bin
npm i -g typescript-language-server typescript eslint
npm i -g vscode-json-languageserver
npm i -g vue-language-server

titleMessage "INSTALL RUST-LANG"
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | zsh -s -- -y && source ~/.cargo/env

titleMessage "SETUP COMPLETED"
runtime=$((($(date +%s)-$start)/60))
titleMessage "The setup took $runtime minutes to complete!"
