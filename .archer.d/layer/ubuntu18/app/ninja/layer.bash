layer_help() {
    echo "app/ninja"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v ninja
}

layer_install() {
    sudo apt-get install -y ninja-build
}
