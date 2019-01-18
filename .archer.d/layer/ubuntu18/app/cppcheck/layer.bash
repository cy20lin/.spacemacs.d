layer_help() {
    echo "app/cppcheck"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v cppcheck
}

layer_install() {
    sudo apt-get install -y cppcheck
}
