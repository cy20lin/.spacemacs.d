layer_help() {
    echo "app/make"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v automake
}

layer_install() {
    pacman -S --needed --noconfirm \
        binutils autoconf make libtool automake
}
