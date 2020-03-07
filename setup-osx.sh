#!/bin/bash

# ------------------------------
# 1. PREPARE OSX
# ------------------------------

echo
echo "------------------------------"
echo "Updating OSX..."
softwareupdate -ia --verbose

echo
echo "------------------------------"
echo "Installing Xcode Command Line Tools..."
xcode-select --install

if test ! $(which brew); then
    echo
    echo "------------------------------"
    echo "Installing homebrew..."
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew tap 'homebrew/cask'
    brew tap 'homebrew/cask-drivers'
    brew tap 'homebrew/cask-fonts'
    brew tap 'homebrew/core'
    brew tap 'homebrew/services'
    brew tap daviderestivo/emacs-head
fi

echo
echo "------------------------------"
echo "Updating homebrew..."
brew update
brew upgrade


# ------------------------------
# 2. PACKAGE INSTALLATION
# ------------------------------

echo
echo "------------------------------"
echo "Installing core CLI utilities..."
CLIPackages=(
    curl
    coreutils
    moreutils
    findutils
    bat
    neovim
    ranger
    ripgrep
    unzip
    zsh
    thefuck
    alacritty
)
brew install ${CLIPackages[@]}
ln -s /usr/local/bin/gsha256sum /usr/local/bin/sha256sum

echo
echo "------------------------------"
echo "Installing art packages..."
CaskArtPackages=(
    figma
    gimp
)
brew cask install --appdir="/Applications" ${CaskArtPackages[@]}
brew install imagemagick

echo
echo "------------------------------"
echo "Installing IDE's..."
brew cask install --appdir="/Applications" visual-studio-code
brew install emacs-head --HEAD --with-cocoa --with-imagemagick --with-jansson
ln -s /usr/local/opt/emacs-head/Emacs.app /Applications

echo
echo "------------------------------"
echo "Installing development packages"
DevelopmentPackages=(
    nvm
)
CaskDevelopmentPackages=(
)
brew install ${DevelopmentPackages[@]}
source /usr/local/opt/nvm/nvm.sh

echo
echo "------------------------------"
echo "Installing browsers..."
BrowserPackages=(
    chromium
    firefox
)
brew cask install --appdir="/Applications" ${BrowserPackages[@]}

echo
echo "------------------------------"
echo "Installing media packages..."
MediaPackages=(
    slack
    discord
    ncmpcpp
    mopidy
    mopidy-spotify
    mopidy-mpd
    dropbox
)
brew cask install --appdir="/Applications" ${MediaPackages[@]}

echo
echo "------------------------------"
echo "Installing devops-ish packages..."
DevOpsPackages=(
    docker
    virtualbox
)
brew cask install --appdir="/Applications" ${DevOpsPackages[@]}
open /Applications/Docker.app

echo
echo "------------------------------"
echo "Installing security packages..."
SecurityPackages=(
    openvpn
    gnupg
)
CaskSecurityPackages=(
    1password
    gpg-suite
)
brew install ${SecurityPackages[@]}
brew cask install --appdir="/Applications" ${CaskSecurityPackages[@]}


# ------------------------------
# 3. CLEANUP
# ------------------------------
brew cleanup
git config user.email kevin.debaerdemaeker@epicgames.com
