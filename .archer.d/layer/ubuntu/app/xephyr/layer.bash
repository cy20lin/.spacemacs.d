
layer_help() {
    echo "app/xephyr"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v Xephyr
}

layer_install() {
    sudo apt-get install xserver-xephyr
}
