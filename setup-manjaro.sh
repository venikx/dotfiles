#!/bin/bash

sudo -v
start=`date +%s`
# ------------------------------
# 1. PREPARE MANJARO
# ------------------------------

echo
echo "------------------------------"
echo "Updating pacman..."
sudo pacman -Sy --noconfirmyyu

echo
echo "------------------------------"
echo "Updating/cleaning yay (AUR)..."
yay -Yc


# ------------------------------
# 2. PACKAGE INSTALLATION
# ------------------------------

echo
echo "------------------------------"
echo "Installing core CLI utilities..."
CLIPackages=(
    git
    curl
    bat
    ccid
    picom
    neovim
    ranger
    ripgrep
    unzip
    zsh
    thefuck
    xorg
    xorg-xclipboard
    scrot
    rxvt-unicode
    tmux
)
AURCLIPackages=(
    i3status-rust
)
sudo pacman -Sy --noconfirm ${CLIPackages[@]}
yay -S --norebuild ${AURCLIPackages[@]}
systemctl disable lightdm.service

echo
echo "------------------------------"
echo "Installing art packages..."
ArtPackages=(
    gimp
    imagemagick
    feh
    python-pywal
)
sudo pacman -Sy --noconfirm ${ArtPackages[@]}

echo
echo "------------------------------"
echo "Installing IDE's..."
sudo pacman -Sy --noconfirm code
yay -S --norebuild emacs-git

echo
echo "------------------------------"
echo "Installing development packages"
DevelopmentPackages=(
    gcc
)
AURDevelopmentPackages=(
    nvm
)
sudo pacman -Sy --noconfirm ${DevelopmentPackages[@]}
yay -S --norebuild ${AURDevelopmentPackages[@]}
source /usr/share/nvm/init-nvm.sh

echo
echo "------------------------------"
echo "Installing browsers..."
BrowserPackages=(
    chromium
    firefox-nightly
)
yay -S --norebuild ${BrowserPackages[@]}

echo
echo "------------------------------"
echo "Installing media packages..."
AURMediaPackages=(
    slack
    discord
    ncmpcpp
    mopidy
    mopidy-spotify
    mopidy-mpd
    mpc
)
MediaPackages=(
    vlc
    obs-studio
    qbittorrent
    pavucontrol
    v4l-utils # utilities to edit video and camera settings
)
yay -S --norebuild ${AURMediaPackages[@]}
sudo pacman -Sy --noconfirm ${MediaPackages[@]}
systemctl start --user mopidy.service
systemctl enable --user mopidy.service

echo
echo "------------------------------"
echo "Installing devops-ish packages..."
DevOpsPackages=(
    docker
    docker-compose
)
sudo pacman -Sy --noconfirm ${DevOpsPackages[@]}
sudo systemctl start docker
sudo systemctl enable docker

echo
echo "------------------------------"
echo "Installing security packages..."
SecurityPackages=(
    i3lock
    openvpn
    pcsc-tools
    yubikey-personalization
)
sudo pacman -Sy --noconfirm ${SecurityPackages[@]}
sudo systemctl start pcscd
sudo systemctl enable pcscd


# ------------------------------
# 3. MISC
# ------------------------------
git config user.email kevin.debaerdemaeker@epicgames.com

runtime=$((($(date +%s)-$start)/60))
echo
echo "------------------------------"
echo "------------------------------"
echo "The setup took $runtime minutes to complete!"
