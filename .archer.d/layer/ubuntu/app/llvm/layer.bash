layer_help() {
    echo "app/emacs"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v llvm-config
}

layer_install() {
    sudo apt-get install -y llvm
}
