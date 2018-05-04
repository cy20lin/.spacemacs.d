layer_help() {
    echo "app/cmake"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v cmake
}

layer_install() {
    sudo apt-get install -y cmake
