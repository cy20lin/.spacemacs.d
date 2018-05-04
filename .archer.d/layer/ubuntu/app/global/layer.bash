layer_help() {
    echo "app/ggtags"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v global gtags
}

layer_install() {
    sudo apt-get install -y global
}
