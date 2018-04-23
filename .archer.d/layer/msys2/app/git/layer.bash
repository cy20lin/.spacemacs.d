layer_help() {
    echo "app/git"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v git
}

layer_install() {
    pacman -S --needed --noconfirm \
           git
}
