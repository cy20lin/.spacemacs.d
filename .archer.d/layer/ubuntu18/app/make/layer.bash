layer_help() {
    echo "app/make"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v make
}

layer_install() {
    sudo apt-get install -y make
}
