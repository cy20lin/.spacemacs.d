layer_help() {
    echo "app/chromix-too"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        # app/chromium
        lang/javascript
    )
}

layer_is_installed() {
    command -v chromix-too && command -v chrome-too-server
}

layer_install() {
    npm install -g chromix-too
}
