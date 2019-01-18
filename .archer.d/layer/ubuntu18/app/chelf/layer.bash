layer_help() {
    echo "app/chelf"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        lang/javascript
    )
}

layer_is_installed() {
    command -v chelf
}

layer_install() {
    npm install -g git+https://github.com/cy20lin/chelf
}
