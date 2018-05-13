layer_help() {
    echo "/msys2"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/fakecygpty
        app/spacemacs
        lang/c-c++
        lang/javascript
        lang/python
    )
}

layer_is_installed() {
    true
}

layer_install() {
    true
}
