layer_help() {
    echo "app/chrimium"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v chromium-browser
}

layer_install() {
    sudo apt-get install -y chromium-browser
}
