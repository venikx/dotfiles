#!/bin/bash

# ------------------------------
# 1. PREPARE MANJARO
# ------------------------------

echo
echo "------------------------------"
echo "Updating pacman..."
sudo pacman -Syyu

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
)
sudo pacman -S ${CLIPackages[@]}

echo
echo "------------------------------"
echo "Installing art packages..."
ArtPackages=(
    gimp
    imagemagick
    feh
)
sudo pacman -S ${ArtPackages[@]}

echo
echo "------------------------------"
echo "Installing IDE's..."
sudo pacman -S code
yay -S emacs-git

echo
echo "------------------------------"
echo "Installing development packages"
DevelopmentPackages=(
    gcc
)
AURDevelopmentPackages=(
    nvm
)
sudo pacman -S ${DevelopmentPackages[@]}
yay -S ${AURDevelopmentPackages[@]}
source /usr/share/nvm/init-nvm.sh

echo
echo "------------------------------"
echo "Installing browsers..."
BrowserPackages=(
    chromium
    firefox-nightly
)
yay -S ${BrowserPackages[@]}

echo
echo "------------------------------"
echo "Installing media packages..."
AURMediaPackages=(
    slack
    discord
    spotify
    # dropbox
)
MediaPackages=(
    vlc
    obs-studio
    qbittorrent
)
yay -S ${AURMediaPackages[@]}
sudo pacman -S ${MediaPackages[@]}


echo
echo "------------------------------"
echo "Installing devops-ish packages..."
DevOpsPackages=(
    docker
    docker-compose
)
sudo pacman -S ${DevOpsPackages[@]}
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
sudo pacman -S ${SecurityPackages[@]}
sudo systemctl start pcscd
sudo systemctl enable pcscd


# ------------------------------
# 3. MISC
# ------------------------------
git config user.email kevin.debaerdemaeker@epicgames.com
