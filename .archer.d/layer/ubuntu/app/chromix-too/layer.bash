layer_help() {
    echo "app/chromix-too"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        lang/javascript
    )
}

layer_is_installed() {
    command -v chromix-too
}

layer_install() {
    sudo npm install -g chromix-too
}
