#!/bin/sh

# NOTE:
# This setup script should only be sourced
# when the environment is within ChromeOS.
#
# Reference:
# https://github.com/dnschneid/crouton/wiki/Sommelier-%28A-more-native-alternative-to-xiwi%29

# FIXME:
# Cannot start emacs with wayland backend.
# Disable these variables for now.

# export GDK_BACKEND=wayland
# export CLUTTER_BACKEND=wayland
# export XDG_RUNTIME_DIR='/var/run/chrome'

export WAYLAND_DISPLAY=wayland-0
export DISPLAY=:0
