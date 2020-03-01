#!/bin/bash
set -e

echo
echo "------------------------------"
echo "Setting up dotfiles..."

chsh -s /bin/zsh

function config {
    /usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME $@
}
git clone --bare https://gitlab.com/venikx/dotfiles.git $HOME/.cfg
mkdir -p .config-backup

config checkout

if [ $? = 0 ]; then
    echo "Config succesfully checked out.";
else
    echo "The dotfiles are in conflict. Backing up pre-existing dotfiles.";
    config checkout 2>&1 | \
        egrep "\s+\." | \
        awk {'print $1'} | xargs -I{} mv {} .config-backup/{}
fi;

config checkout
config config status.showUntrackedFiles no
