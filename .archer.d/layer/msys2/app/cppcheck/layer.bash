layer_help() {
    echo "app/cppcheck"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v cppcheck
}

layer_install() {
    pacman -S --needed --noconfirm \
           mingw-w64-x86_64-cppcheck
}
