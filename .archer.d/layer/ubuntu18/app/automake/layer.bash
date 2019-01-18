layer_help() {
    echo "app/automake"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v automake
}

layer_install() {
    sudo apt-get install -y automake
}
