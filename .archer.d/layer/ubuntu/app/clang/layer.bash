layer_help() {
    echo "app/clang"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v clang
}

layer_install() {
    sudo apt-get install -y clang
}
