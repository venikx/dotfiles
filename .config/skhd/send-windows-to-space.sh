#!/bin/sh

if ! yabai -m query --spaces --space $@; then
    yabai -m space --create
    sidx="$(yabai -m query --spaces | jq -r 'map(select(."native-fullscreen" == 0))[-1].index')"
    yabai -m space "${sidx}" --label $@
fi

yabai -m window --space $@

# if yabai -m query --windows --space | jq -e '. == []'; then 
#    yabai -m space --destroy
# fi
