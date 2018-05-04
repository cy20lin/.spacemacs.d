layer_help() {
    echo "app/emacs"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v rtags
}

layer_install() {
    false
    # sudo apt-get install -y rtags
}
