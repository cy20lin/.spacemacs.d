layer_help() {
    echo "app/emacs"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v /bin/emacs /mingw64/bin/emacs
}

layer_install() {
    pacman -S --needed --noconfirm \
           emacs \
           mingw-w64-x86_64-emacs
}
