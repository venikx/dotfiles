#!/bin/sh

set -e

echo
echo "------------------------------"
echo "Setting up dotfiles..."

mkdir -p .config-backup
function config {
    /usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME $@
}
git clone --bare https://gitlab.com/venikx/dotfiles.git $HOME/.cfg
config config user.email me@venikx.com
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
