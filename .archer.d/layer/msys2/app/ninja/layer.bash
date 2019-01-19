
layer_help() {
    echo "app/ninja"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v ninja
}

layer_install() {
    pacman -S --needed --noconfirm \
           mingw-w64-x86_64-ninja
}

