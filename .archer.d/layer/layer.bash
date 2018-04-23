layer_help() {
    echo "/"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/spacemacs
        lang/javascript
        lang/python
    )
}

layer_is_installed() {
    false
}

layer_install() {
    true
}
