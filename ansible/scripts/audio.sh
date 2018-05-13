#!/usr/bin/env zsh

function ensure_link {
    test -L "/var/service/$1" || ln -s "/etc/sv/$1" "/var/service/"
}

ensure_link "alsa"
ensure_link "dbus"
ensure_link "cgmanager"
ensure_link "consolekit"
