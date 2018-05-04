layer_help() {
    echo "app/emacs"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v gcc
}

layer_install() {
    sudo apt-get install -y gcc
    sudo apt-get install -y g++
}
