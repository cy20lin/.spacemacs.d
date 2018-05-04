layer_help() {
    echo "app/clang"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v /mingw64/bin/clang
}

layer_install() {
    pacman -S --needed --noconfirm \
           mingw-w64-x86_64-clang
}
