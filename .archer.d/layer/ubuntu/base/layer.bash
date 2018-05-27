layer_help() {
    echo "base"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v add-apt-repository
}

layer_install() {
    sudo apt-get install -y software-properties-common python-software-properties
}
