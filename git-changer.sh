#!/bin/sh

/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME filter-branch --env-filter '

OLD_EMAIL="me@venikx.com"
CORRECT_NAME="Kevin Rangel"
CORRECT_EMAIL="me@venikx.com"

# if [ "$GIT_COMMITTER_EMAIL" = "$OLD_EMAIL" ]
# then
    export GIT_COMMITTER_NAME="$CORRECT_NAME"
    export GIT_COMMITTER_EMAIL="$CORRECT_EMAIL"
# fi
# if [ "$GIT_AUTHOR_EMAIL" = "$OLD_EMAIL" ]
# then
#     export GIT_AUTHOR_NAME="$CORRECT_NAME"
#     export GIT_AUTHOR_EMAIL="$CORRECT_EMAIL"
# fi
' --tag-name-filter cat -- --branches --tags
