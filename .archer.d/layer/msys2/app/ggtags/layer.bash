layer_help() {
    echo "app/ggtags"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v global
}

layer_install() {
    pacman -S --needed --noconfirm \
           global \
           mingw-w64-x86_64-global
}
