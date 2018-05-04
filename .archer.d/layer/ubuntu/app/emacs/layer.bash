layer_help() {
    echo "app/emacs"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v emacs
}

layer_install() {
    sudo apt-get install -y emacs
}
