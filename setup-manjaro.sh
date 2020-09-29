#!/bin/sh

sudo -v
start=`date +%s`
# ------------------------------
# 1. PREPARE MANJARO
# ------------------------------

echo
echo "------------------------------"
echo "Updating pacman..."
sudo pacman -Suy --noconfirm

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
    picom #compton 
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
AURCLIPackages=(
    i3status-rust
)
sudo pacman -Sy --noconfirm ${CLIPackages[@]}
yay -S --norebuild ${AURCLIPackages[@]}

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
    spotify
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

echo
echo "------------------------------"
echo "Installing devops-ish packages..."
DevOpsPackages=(
    docker
    docker-compose
)
sudo pacman -Sy --noconfirm ${DevOpsPackages[@]}

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


# ------------------------------
# 3. Services
# ------------------------------
echo
echo "------------------------------"
echo "Managing services"
systemctl disable lightdm.service
sudo systemctl start docker
sudo systemctl enable docker
sudo systemctl start pcscd
sudo systemctl enable pcscd

chsh -s $(which zsh)
~/.emacs.d/bin/doom install
~/.emacs.d/bin/doom sync
wal -i ~/wallpapers/cyberpunk-street.jpg

runtime=$((($(date +%s)-$start)/60))
echo
echo "------------------------------"
echo "------------------------------"
echo "The setup took $runtime minutes to complete!"
echo "Please reboot since your shell has changed from bash to zsh"
