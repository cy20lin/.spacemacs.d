layer_help() {
    echo "app/gcc"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v gcc
}

layer_install() {
    pacman -S --needed --noconfirm \
           gcc \
           mingw-w64-x86_64-gcc
}
